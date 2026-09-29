# =============================================================================
# 02_training.R
# Step 2: define a calibrator and train/load a calibration method
#
# This file contains:
#   - the generic calibrator object/interface constructor
#   - the generic BiLSTM adapter
#   - compatibility with the original pretrained SIR BiLSTM
#   - training-data saving and generic BiLSTM training
#
# The calibration method is independent of the epidemic model. A future
# Transformer, CNN, Bayesian model, or other method can implement the same
# list(...) |> structure(class = "calibrator") interface.
# =============================================================================

# Generic calibrator interface -------------------------------------------------
#
# A calibrator is deliberately just a list of three functions plus a model
# label.  This is the public extension mechanism: users do not need to inherit
# from a complicated R6/S4 class or modify package internals.
#
# The required shape is:
#
# calibrator_x <- list(
#   pre_process = function(x, ...) { ... },
#   run          = function(x, ...) { ... },
#   post_process = function(x, ...) { ... },
#   type         = "MODEL"
# ) |> structure(class = "calibrator")

#' @export
print.calibrator <- function(x, ...) {
  cat("Calibrator object for", x$type, "models\n")
  invisible(x)
}


# Generic BiLSTM calibrator ---------------------------------------------------

# Post-processing used by the original pretrained SIR model: keep the predicted
# ptran and R0, and recompute crate from R0 = ptran * crate / recov.
# Models without ptran/crate/R0 targets or a known recov are left unchanged.
derive_crate <- function(x, recov = NULL, ...) {
  if (!is.null(recov) && all(c("ptran", "crate", "R0") %in% names(x))) {
    x["crate"] <- x["R0"] * recov / x["ptran"]
  }
  x
}

#' Create a BiLSTM calibrator from a saved model directory
#'
#' The saved model determines the number of inputs and outputs.  `known_names`
#' tells the R wrapper which named values from `...` should be sent to Python.
#' Nothing here assumes SIR or SEIR.
#'
#' @param model_dir Directory containing model.pt and the saved scalers.
#' @param known_names Names of known parameters expected by the model.
#' @param type Epidemiological model label.
#' @param name Calibrator label.
#' @param post_process Result transformation. By default, the same as the
#'   pretrained SIR model: when the model predicts `ptran`, `crate` and `R0`
#'   and `recov` is known, `crate` is replaced by `R0 * recov / ptran`.
#'   Use `function(x, ...) x` to keep the raw network output.
#' @return A `calibrator` object.
#' @export
bilstm_calibrator <- function(
    model_dir,
    known_names,
    type = "generic",
    name = "BiLSTM",
    post_process = derive_crate
) {
  model_dir <- normalizePath(model_dir, mustWork = TRUE)

  pre_process <- function(daily_cases, ...) {
    dots <- list(...)
    missing <- setdiff(known_names, names(dots))
    if (length(missing)) {
      stop("Missing known parameter(s): ", paste(missing, collapse = ", "))
    }

    list(
      epicurve = as.numeric(daily_cases),
      known = as.numeric(unlist(dots[known_names], use.names = FALSE))
    )
  }

  run <- function(x, ...) {
    py <- load_python_script("bilstm.py")
    ans <- py$predict_saved_model(
      x$epicurve,
      matrix(x$known, nrow = 1),
      model_dir
    )
    unlist(ans)
  }

  list(
    pre_process = pre_process,
    run = run,
    post_process = post_process,
    type = type
  ) |>
    structure(class = "calibrator")
}

#' Create the original pretrained SIR BiLSTM calibrator
#'
#' This is a compatibility adapter for the already-trained 61-day SIR model.
#' It is separate from the generic BiLSTM engine because the original checkpoint
#' used its own architecture and saved scalers.
#'
#' @export
pretrained_sir_calibrator <- function(model_dir = NULL) {
  if (is.null(model_dir)) {
    model_dir <- package_file("models", "sir", "pretrained")
  }
  model_dir <- normalizePath(model_dir, mustWork = TRUE)

  pre_process <- function(daily_cases, n, recov, ...) {
    list(
      epicurve = as.numeric(daily_cases),
      n = as.numeric(n),
      recov = as.numeric(recov)
    )
  }

  run <- function(x, ...) {
    py <- load_python_script("legacy_sir.py")
    unlist(py$predict_pretrained_sir(
      x$epicurve,
      x$n,
      x$recov,
      model_dir
    ))
  }

  list(
    pre_process = pre_process,
    run = run,
    post_process = derive_crate,
    type = "SIR"
  ) |>
    structure(class = "calibrator")
}


# Generic training helpers ----------------------------------------------------

#' Save a training bank
#'
#' The Python trainer only needs a parameter table and an observed-curve matrix.
#' `name` can be "sir", "seir", "sirh", or any future model label.
#' @export
save_training_data <- function(x, name, output_dir = tempdir()) {
  if (is.null(x$theta) || !is.data.frame(x$theta)) {
    stop("x$theta must be a data.frame.")
  }

  observations <- x$observations
  if (is.null(observations)) observations <- x$incidence
  if (is.null(observations)) {
    stop("x must contain an `observations` matrix.")
  }
  observations <- as.matrix(observations)

  if (nrow(observations) != nrow(x$theta)) {
    stop("theta and observations must have the same number of rows.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  theta_path <- file.path(output_dir, paste0(name, "_theta.csv"))
  observations_path <- file.path(output_dir, paste0(name, "_observations.csv"))

  write.csv(x$theta, theta_path, row.names = FALSE)
  write.csv(observations, observations_path, row.names = FALSE)

  invisible(c(theta = theta_path, observations = observations_path))
}

#' Train the generic BiLSTM calibrator
#'
#' @param name Label used for input/output file names.
#' @param known_names Columns in the parameter table treated as known inputs.
#' @param target_names Columns the neural network should estimate.
#' @param type Epidemiological model label written to model_config.json.
#' @param data Optional `training_data` object. If supplied, it is saved to a
#'   temporary folder and used directly; no package simulator is required.
#' @param parameter_file Optional external CSV containing parameter rows.
#' @param observation_file Optional external CSV containing observation rows.
#' @param output_dir Directory the trained model is written to. Defaults to a
#'   temporary folder; pass your own folder to keep the model after the R
#'   session ends.
#' @param epochs Number of epochs.
#' @param batch_size Batch size.
#' @param learning_rate Adam learning rate.
#' @param seed Random seed.
#' @param hidden_size BiLSTM hidden dimension.
#' @param num_layers Number of BiLSTM layers.
#' @param dropout LSTM dropout.
#' @param physics_weight Optional SIR identity penalty. Set to zero for a fully
#'   generic model.  It is only used when the required columns are present.
#' @return Invisibly, the path to the trained model directory.
#' @export
train_bilstm_calibrator <- function(
    name,
    known_names,
    target_names,
    type = name,
    data = NULL,
    parameter_file = NULL,
    observation_file = NULL,
    output_dir = file.path(tempdir(), name),
    epochs = 100,
    batch_size = 64,
    learning_rate = 0.001,
    seed = 122,
    hidden_size = 160,
    num_layers = 3,
    dropout = 0.5,
    physics_weight = 0
) {
  script <- package_file("python", "train_bilstm.py")
  python <- reticulate::py_config()$python

  # Two supported input modes:
  #   1. `data=`: pass a training_data object directly;
  #   2. `parameter_file=` + `observation_file=`: use external CSV files.
  if (!is.null(data)) {
    if (is.null(data$theta) || is.null(data$observations)) {
      stop("data must contain `theta` and `observations`.")
    }
    paths <- save_training_data(
      data, name = name, output_dir = tempfile("training_data_")
    )
    theta <- paths[["theta"]]
    observations <- paths[["observations"]]
  } else if (!is.null(parameter_file) && !is.null(observation_file)) {
    theta <- normalizePath(parameter_file, mustWork = TRUE)
    observations <- normalizePath(observation_file, mustWork = TRUE)
  } else {
    stop(
      "Supply either `data`, or both `parameter_file` and `observation_file`."
    )
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  if (!file.exists(theta)) stop("Missing training file: ", theta)
  if (!file.exists(observations)) {
    stop("Missing training observations: ", observations)
  }

  args <- c(
    script,
    "--theta", theta,
    "--curves", observations,
    "--output-dir", output_dir,
    "--known", paste(known_names, collapse = ","),
    "--targets", paste(target_names, collapse = ","),
    "--type", type,
    "--epochs", as.integer(epochs),
    "--batch-size", as.integer(batch_size),
    "--learning-rate", learning_rate,
    "--seed", as.integer(seed),
    "--hidden-size", as.integer(hidden_size),
    "--num-layers", as.integer(num_layers),
    "--dropout", dropout,
    "--physics-weight", physics_weight
  )

  status <- system2(python, args = args)
  if (!identical(status, 0L)) stop("Python training failed with status ", status)

  invisible(output_dir)
}
