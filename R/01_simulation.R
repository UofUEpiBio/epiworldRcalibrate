# =============================================================================
# 01_simulation.R
# Step 1: define/import a simulator and create training data
#
# This file contains:
#   - internal path/Python helpers used by the package
#   - generic simulator interface
#   - generic user-supplied training-data interface
#   - built-in epiworldR SIR and SEIR simulators
#   - SIR/SEIR training-bank generators
#
# A model does NOT have to exist in epiworldR. Users can supply their own
# parameter table + observation matrix, or wrap their own simulator with
# new_simulator().
# =============================================================================

# Internal path helpers -------------------------------------------------------

find_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  ok <- file.exists(file.path(candidates, "DESCRIPTION")) &
    dir.exists(file.path(candidates, "R")) &
    dir.exists(file.path(candidates, "inst"))
  if (!any(ok)) return(NULL)
  normalizePath(candidates[which(ok)[1]], mustWork = TRUE)
}

package_file <- function(..., package_dir = NULL) {
  if (is.null(package_dir)) package_dir <- find_project_root()

  if (!is.null(package_dir)) {
    path <- file.path(package_dir, ...)
    if (file.exists(path) || dir.exists(path)) return(normalizePath(path, mustWork = FALSE))
  }

  installed <- system.file(..., package = "epiworldRcalibrate")
  if (nzchar(installed)) return(installed)

  stop("Cannot find package file: ", file.path(...))
}

load_python_script <- function(filename, package_dir = NULL) {
  path <- package_file("inst", "python", filename, package_dir = package_dir)
  env <- new.env(parent = globalenv())
  reticulate::source_python(path, envir = env, convert = TRUE)
  env
}

setup_python_deps <- function(force = FALSE) {
  packages <- c("numpy", "pandas", "torch", "scikit-learn==1.6.1", "joblib")
  reticulate::py_require(packages, action = if (force) "set" else "add")
  invisible(TRUE)
}


# Generic simulation interface ------------------------------------------------
#
# Data generation is deliberately separated from both the epidemiological
# model and the calibration method.  A simulator object only needs two pieces:
#   1. a function that samples parameter sets;
#   2. a function that turns one parameter set into one numeric epidemic curve.
#
# The generic simulate_training_data() function does not know what SIR, SEIR,
# SIRH, hospitalization, or any other state means.

#' Create a simulator object
#'
#' @param sample_parameters Function with argument `n_sims` that returns a data
#'   frame with one row per simulated parameter set.
#' @param run Function with arguments `parameters` and `seed`. `parameters` is a
#'   named list containing one row of the sampled parameter table. The function
#'   must return one numeric epidemic curve.
#' @param type Label for the epidemiological model, e.g. "SIR", "SEIR", "SIRH".
#' @param name Human-readable simulator name.
#' @return An object of class `simulator`.
#' @export
new_simulator <- function(
    sample_parameters,
    run,
    type = "generic",
    name = NULL
) {
  if (!is.function(sample_parameters)) {
    stop("sample_parameters must be a function.")
  }
  if (!is.function(run)) {
    stop("run must be a function.")
  }

  structure(
    list(
      sample_parameters = sample_parameters,
      run = run,
      type = type,
      name = name
    ),
    class = "simulator"
  )
}

#' @export
print.simulator <- function(x, ...) {
  cat(
    "Simulator:",
    if (!is.null(x$name)) x$name else "unnamed",
    "\n"
  )
  cat("Model type:", x$type, "\n")
  invisible(x)
}


#' Create a generic training-data object
#'
#' Use this when the package cannot simulate the user's epidemiological model.
#' The user supplies one parameter row and one observed time series per
#' simulation. `observations` can represent cases, hospitalizations, deaths,
#' prevalence, or any other numeric series used by the calibrator.
#'
#' @param parameters Data frame with one row per simulation and named columns.
#' @param observations Numeric matrix/data.frame with one row per simulation.
#' @param type Optional epidemiological model label.
#' @param source Optional description of where the training data came from.
#' @return An object of class `training_data`.
#' @export
new_training_data <- function(
    parameters,
    observations,
    type = "generic",
    source = "user"
) {
  if (!is.data.frame(parameters)) {
    parameters <- as.data.frame(parameters)
  }
  if (!nrow(parameters) || !ncol(parameters)) {
    stop("parameters must contain at least one row and one column.")
  }
  if (is.null(names(parameters)) || any(names(parameters) == "")) {
    stop("parameters must have named columns.")
  }
  if (anyDuplicated(names(parameters))) {
    stop("parameters contains duplicated column names.")
  }

  observations <- as.matrix(observations)
  storage.mode(observations) <- "double"

  if (!nrow(observations) || !ncol(observations)) {
    stop("observations must contain at least one row and one column.")
  }
  if (nrow(observations) != nrow(parameters)) {
    stop(
      "parameters and observations must have the same number of rows. ",
      "Got ", nrow(parameters), " and ", nrow(observations), "."
    )
  }
  if (any(!is.finite(observations))) {
    stop("observations contains non-finite values.")
  }

  if (is.null(colnames(observations))) {
    colnames(observations) <- paste0("time_", seq_len(ncol(observations)))
  }

  structure(
    list(
      theta = parameters,
      observations = observations,
      # Backward-compatible alias for older code.
      incidence = observations
    ),
    class = "training_data",
    training_type = type,
    training_source = source
  )
}

#' @export
print.training_data <- function(x, ...) {
  cat("Training data\n")
  cat("  model type:", attr(x, "training_type"), "\n")
  cat("  source:", attr(x, "training_source"), "\n")
  cat("  simulations:", nrow(x$theta), "\n")
  cat("  parameters:", ncol(x$theta), "\n")
  cat("  observation length:", ncol(x$observations), "\n")
  invisible(x)
}

#' Read externally generated training data
#'
#' @param parameter_file CSV file containing one parameter row per simulation.
#' @param observation_file CSV file containing one observed time series per row.
#' @param type Optional epidemiological model label.
#' @export
read_training_data <- function(
    parameter_file,
    observation_file,
    type = "generic"
) {
  parameters <- read.csv(parameter_file, check.names = FALSE)
  observations <- as.matrix(
    read.csv(observation_file, check.names = FALSE)
  )

  new_training_data(
    parameters = parameters,
    observations = observations,
    type = type,
    source = "external files"
  )
}

#' Generate a generic simulation bank
#'
#' This function is model-agnostic. It samples parameter rows from the supplied
#' simulator and calls the simulator once for each row.
#'
#' @param simulator A `simulator` object created with `new_simulator()`.
#' @param n_sims Number of simulations.
#' @param seed Master seed used for parameter sampling and per-simulation seeds.
#' @return A list with `theta` (parameter table) and `observations` (curve matrix).
#' @export
simulate_training_data <- function(
    simulator,
    n_sims,
    seed = 122
) {
  if (!inherits(simulator, "simulator")) {
    stop(
      "The object 'simulator' must be of class 'simulator'. It is of class: ",
      paste(class(simulator), collapse = ", ")
    )
  }

  n_sims <- as.integer(n_sims)
  if (length(n_sims) != 1L || is.na(n_sims) || n_sims < 1L) {
    stop("n_sims must be a positive integer.")
  }

  set.seed(seed)
  theta <- simulator$sample_parameters(n_sims = n_sims)

  if (!is.data.frame(theta)) {
    stop("simulator$sample_parameters() must return a data.frame.")
  }
  if (nrow(theta) != n_sims) {
    stop(
      "simulator$sample_parameters() returned ", nrow(theta),
      " rows, but n_sims = ", n_sims, "."
    )
  }
  if (!ncol(theta)) {
    stop("The sampled parameter table has no columns.")
  }
  if (anyDuplicated(names(theta))) {
    stop("The sampled parameter table contains duplicated column names.")
  }

  simulation_seeds <- sample.int(.Machine$integer.max, n_sims)
  curves <- vector("list", n_sims)

  for (i in seq_len(n_sims)) {
    parameters <- as.list(theta[i, , drop = FALSE])

    curve <- simulator$run(
      parameters = parameters,
      seed = simulation_seeds[i]
    )

    if (!is.numeric(curve)) {
      stop("Simulation ", i, " did not return a numeric curve.")
    }
    if (!length(curve)) {
      stop("Simulation ", i, " returned an empty curve.")
    }
    if (any(!is.finite(curve))) {
      stop("Simulation ", i, " returned non-finite values.")
    }

    curves[[i]] <- as.numeric(curve)
    names(curves[[i]]) <- names(curve)
  }

  curve_lengths <- vapply(curves, length, integer(1))
  if (length(unique(curve_lengths)) != 1L) {
    stop(
      "All simulated curves must have the same length. Observed lengths: ",
      paste(sort(unique(curve_lengths)), collapse = ", ")
    )
  }

  incidence <- do.call(rbind, curves)

  curve_names <- names(curves[[1]])
  if (!is.null(curve_names) && length(curve_names) == ncol(incidence)) {
    colnames(incidence) <- curve_names
  } else {
    colnames(incidence) <- paste0("time_", seq_len(ncol(incidence)))
  }

  rownames(incidence) <- NULL

  ans <- new_training_data(
    parameters = theta,
    observations = incidence,
    type = simulator$type,
    source = if (!is.null(simulator$name)) simulator$name else "simulator"
  )
  attr(ans, "simulator_type") <- simulator$type
  attr(ans, "simulator_name") <- simulator$name
  ans
}

#' Simulate one curve from a simulator
#'
#' @param simulator A `simulator` object.
#' @param parameters Named list or one-row data frame of parameter values.
#' @param seed Simulation seed.
#' @export
simulate_from_parameters <- function(
    simulator,
    parameters,
    seed = 1
) {
  if (!inherits(simulator, "simulator")) {
    stop("simulator must be of class 'simulator'.")
  }

  if (is.data.frame(parameters)) {
    if (nrow(parameters) != 1L) {
      stop("parameters must contain exactly one row.")
    }
    parameters <- as.list(parameters[1, , drop = FALSE])
  }

  if (!is.list(parameters) || is.null(names(parameters))) {
    stop("parameters must be a named list or one-row data.frame.")
  }

  curve <- simulator$run(parameters = parameters, seed = seed)
  if (!is.numeric(curve) || !length(curve) || any(!is.finite(curve))) {
    stop("The simulator did not return a valid numeric curve.")
  }
  curve
}

# epiworldR examples ----------------------------------------------------------
# These are adapters for the two models currently used in this project.  They
# are examples built on top of the generic simulator API; the generic API does
# not depend on SIR or SEIR.

get_incidence <- function(model, ndays, keep_day_zero = FALSE) {
  x <- as.numeric(epiworldR::plot_incidence(model, plot = FALSE)[, "Infected"])
  if (!keep_day_zero) x <- x[-1]
  expected <- if (keep_day_zero) ndays + 1L else ndays
  if (length(x) < expected) x <- c(x, rep(0, expected - length(x)))
  x[seq_len(expected)]
}

merge_priors <- function(defaults, priors) {
  if (is.null(priors)) return(defaults)
  unknown <- setdiff(names(priors), names(defaults))
  if (length(unknown)) {
    stop("Unknown prior(s): ", paste(unknown, collapse = ", "))
  }
  defaults[names(priors)] <- priors
  defaults
}

sample_sir_theta <- function(n_sims, p) {
  out <- data.frame()

  while (nrow(out) < n_sims) {
    n_need <- n_sims - nrow(out)
    n_draw <- max(2L * n_need, 10L)

    z <- data.frame(
      n = sample(p$n[1]:p$n[2], n_draw, replace = TRUE),
      recov = runif(n_draw, p$recov[1], p$recov[2]),
      preval = runif(n_draw, p$preval[1], p$preval[2]),
      crate = runif(n_draw, p$crate[1], p$crate[2]),
      R0 = runif(n_draw, p$R0[1], p$R0[2])
    )

    z$ptran <- z$R0 * z$recov / z$crate
    z <- z[z$ptran <= 1, , drop = FALSE]
    out <- rbind(out, z)
  }

  out <- out[seq_len(n_sims), , drop = FALSE]
  rownames(out) <- NULL
  out$initial_infected <- pmax(1L, round(out$preval * out$n))
  out
}

#' Create the current epiworldR SIR simulator
#'
#' @param ndays Number of simulated days. The current SIR adapter preserves the
#'   historical day-zero convention and returns `ndays + 1` values.
#' @param priors Optional named list overriding the default prior ranges.
#' @export
sir_simulator <- function(
    ndays = 60,
    priors = NULL
) {
  p <- merge_priors(
    list(
      n = c(5000, 10000),
      recov = c(0.071, 0.25),
      preval = c(0.007, 0.02),
      crate = c(1, 5),
      R0 = c(1.1, 5)
    ),
    priors
  )

  sample_parameters <- function(n_sims) {
    sample_sir_theta(n_sims, p)
  }

  run <- function(parameters, seed) {
    m <- epiworldR::ModelSIRCONN(
      name = "sim",
      n = parameters$n,
      prevalence = parameters$preval,
      contact_rate = parameters$crate,
      transmission_rate = parameters$ptran,
      recovery_rate = parameters$recov
    )
    epiworldR::verbose_off(m)
    epiworldR::run(m, ndays = ndays, seed = seed)

    ans <- get_incidence(m, ndays, keep_day_zero = TRUE)
    names(ans) <- paste0("day_", 0:ndays)
    ans
  }

  new_simulator(
    sample_parameters = sample_parameters,
    run = run,
    type = "SIR",
    name = "epiworldR ModelSIRCONN"
  )
}

#' Create the current epiworldR SEIR simulator
#'
#' @param ndays Number of returned incidence days.
#' @param priors Optional named list overriding the default prior ranges.
#' @export
seir_simulator <- function(
    ndays = 365,
    priors = NULL
) {
  p <- merge_priors(
    list(
      n = c(5000, 10000),
      recov = c(0.071, 0.25),
      crate = c(5, 15),
      incub = c(3, 21),
      R0 = c(1, 5),
      initial_infected = c(100, 2000)
    ),
    priors
  )

  sample_parameters <- function(n_sims) {
    theta <- data.frame(
      n = sample(p$n[1]:p$n[2], n_sims, replace = TRUE),
      recov = runif(n_sims, p$recov[1], p$recov[2]),
      crate = runif(n_sims, p$crate[1], p$crate[2]),
      incub = runif(n_sims, p$incub[1], p$incub[2]),
      R0 = runif(n_sims, p$R0[1], p$R0[2]),
      initial_infected = sample(
        p$initial_infected[1]:p$initial_infected[2],
        n_sims,
        replace = TRUE
      )
    )

    theta$preval <- theta$initial_infected / theta$n
    theta$ptran <- theta$R0 * theta$recov / theta$crate
    theta
  }

  run <- function(parameters, seed) {
    m <- epiworldR::ModelSEIRCONN(
      name = "sim",
      n = parameters$n,
      prevalence = parameters$preval,
      contact_rate = parameters$crate,
      incubation_days = parameters$incub,
      transmission_rate = parameters$ptran,
      recovery_rate = parameters$recov
    )
    epiworldR::verbose_off(m)
    epiworldR::run(m, ndays = ndays + 1L, seed = seed)

    ans <- get_incidence(m, ndays, keep_day_zero = FALSE)
    names(ans) <- paste0("day_", seq_len(ndays))
    ans
  }

  new_simulator(
    sample_parameters = sample_parameters,
    run = run,
    type = "SEIR",
    name = "epiworldR ModelSEIRCONN"
  )
}


# Built-in training-bank generators -------------------------------------------
#
# These are convenience functions for the two models currently supported by
# epiworldR in this project. They are NOT required by the generic interface.

#' Generate the built-in SIR training bank
#'
#' @param n_sims Number of simulated parameter sets.
#' @param ndays Number of simulated days. Historical SIR output contains
#'   `ndays + 1` values including day zero.
#' @param seed Master random seed.
#' @param priors Optional prior overrides passed to `sir_simulator()`.
#' @param save If TRUE, save the bank under `data/`.
#' @param name File/model label.
#' @param package_dir Project root.
#' @export
generate_sir_training_data <- function(
    n_sims = 5000,
    ndays = 60,
    seed = 122,
    priors = NULL,
    save = FALSE,
    name = "sir",
    package_dir = tempdir()
) {
  bank <- simulate_training_data(
    simulator = sir_simulator(ndays = ndays, priors = priors),
    n_sims = n_sims,
    seed = seed
  )

  if (isTRUE(save)) {
    message("Saving training bank to ", file.path(package_dir, "data"))
    save_training_data(bank, name = name, package_dir = package_dir)
  }

  bank
}

#' Generate the built-in SEIR training bank
#'
#' @param n_sims Number of simulated parameter sets.
#' @param ndays Number of returned incidence days.
#' @param seed Master random seed.
#' @param priors Optional prior overrides passed to `seir_simulator()`.
#' @param save If TRUE, save the bank under `data/`.
#' @param name File/model label.
#' @param package_dir Project root.
#' @export
generate_seir_training_data <- function(
    n_sims = 20000,
    ndays = 365,
    seed = 122,
    priors = NULL,
    save = FALSE,
    name = "seir",
    package_dir = tempdir()
) {
  bank <- simulate_training_data(
    simulator = seir_simulator(ndays = ndays, priors = priors),
    n_sims = n_sims,
    seed = seed
  )

  if (isTRUE(save)) {
    message("Saving training bank to ", file.path(package_dir, "data"))
    save_training_data(bank, name = name, package_dir = package_dir)
  }

  bank
}
