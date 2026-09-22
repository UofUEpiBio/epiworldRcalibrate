source("R/02_training.R")
source("R/03_test_compare.R")

cal <- list(
  pre_process = function(x, ...) x,
  run = function(x, ...) c(a = sum(x)),
  post_process = function(x, ...) x,
  type = "TEST"
) |>
  structure(class = "calibrator")

ans <- calibrate(
  daily_cases = c(1, 2, 3),
  calibrator = cal,
  n = 8000,
  recov = 0.1
)

stopifnot(is.numeric(ans))
stopifnot(ans[["a"]] == 6)

cat("PASS: post_process accepts forwarded ... arguments\n")
