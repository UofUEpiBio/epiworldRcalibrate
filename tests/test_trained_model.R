# Smoke test for any saved generic BiLSTM model.
# Usage: Rscript tests/test_trained_model.R sir

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: Rscript tests/test_trained_model.R <model-name>")
name <- args[[1]]

source(file.path("R", "01_simulation.R"))
source(file.path("R", "02_training.R"))
source(file.path("R", "03_test_compare.R"))

model_dir <- file.path("inst", "models", name, "trained")
config_path <- file.path(model_dir, "model_config.json")
if (!file.exists(config_path)) stop("No trained model found at: ", model_dir)

config <- jsonlite::fromJSON(config_path)
theta <- read.csv(file.path("data", paste0(name, "_theta.csv")))
curve_path <- file.path("data", paste0(name, "_observations.csv"))
if (!file.exists(curve_path)) {
  curve_path <- file.path("data", paste0(name, "_incidence.csv"))
}
inc <- read.csv(curve_path, check.names = FALSE)

cal <- bilstm_calibrator(
  model_dir = model_dir,
  known_names = config$known_names,
  type = config$model_type
)

dots <- as.list(theta[1, config$known_names, drop = FALSE])
ans <- do.call(
  calibrate,
  c(list(daily_cases = as.numeric(inc[1, ]), calibrator = cal), dots)
)

print(ans)
stopifnot(length(ans) == length(config$target_names))
stopifnot(all(is.finite(ans)))
cat("PASS: trained", name, "calibrator loaded and returned finite predictions\n")
