source("R/02_training.R")
source("R/03_test_compare.R")

calibrator_test <- list(

  pre_process = function(x, scale = 1, ...) {
    as.numeric(x) * scale
  },

  run = function(x, ...) {
    x + 1
  },

  post_process = function(x, ...) {
    sum(x)
  },

  type = "TEST"

) |>
  structure(class = "calibrator")

stopifnot(inherits(calibrator_test, "calibrator"))

ans <- calibrate(
  daily_cases = c(1, 2, 3),
  calibrator = calibrator_test,
  scale = 2
)

stopifnot(ans == sum(c(1, 2, 3) * 2 + 1))

cat("PASS: advisor-style calibrator interface\n")
