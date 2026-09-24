# Smoke test for a BiLSTM model shipped with the package.
# Usage: Rscript tests/test_trained_model.R sir
# Needs Python with torch, so it is skipped when no model name is given
# (for example, during R CMD check).

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  cat("SKIP: run `Rscript tests/test_trained_model.R <model-name>` to test a trained model\n")
  quit(save = "no")
}
name <- args[[1]]

library(epiworldRcalibrate)

model_dir <- system.file("models", name, "trained", package = "epiworldRcalibrate")
config_path <- file.path(model_dir, "model_config.json")
if (!nzchar(model_dir) || !file.exists(config_path)) {
  stop("No trained model named '", name, "' is installed with the package.")
}

config <- jsonlite::fromJSON(config_path)

# Simulate one test curve with the same length the model was trained on.
bank <- switch(
  name,
  sir = generate_sir_training_data(n_sims = 1, ndays = config$sequence_length - 1),
  seir = generate_seir_training_data(n_sims = 1, ndays = config$sequence_length),
  stop("No built-in simulator for model '", name, "'.")
)

cal <- bilstm_calibrator(
  model_dir = model_dir,
  known_names = config$known_names,
  type = config$model_type
)

dots <- as.list(bank$theta[1, config$known_names, drop = FALSE])
ans <- do.call(
  calibrate,
  c(list(daily_cases = as.numeric(bank$observations[1, ]), calibrator = cal), dots)
)

print(ans)
stopifnot(length(ans) == length(config$target_names))
stopifnot(all(is.finite(ans)))
cat("PASS: trained", name, "calibrator loaded and returned finite predictions\n")
