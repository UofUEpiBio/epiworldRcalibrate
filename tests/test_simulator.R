# Small test of the generic simulator interface. No epiworldR model is needed.

library(epiworldRcalibrate)

sampler <- function(n_sims) {
  data.frame(
    level = seq_len(n_sims),
    slope = rep(2, n_sims)
  )
}

runner <- function(parameters, seed) {
  # A deterministic toy curve is enough to test the interface.
  out <- parameters$level + parameters$slope * 0:4
  names(out) <- paste0("time_", 0:4)
  out
}

toy <- new_simulator(
  sample_parameters = sampler,
  run = runner,
  type = "toy",
  name = "toy simulator"
)

bank <- simulate_training_data(toy, n_sims = 3, seed = 122)

stopifnot(nrow(bank$theta) == 3)
stopifnot(nrow(bank$observations) == 3)
stopifnot(ncol(bank$observations) == 5)
stopifnot(identical(colnames(bank$observations), paste0("time_", 0:4)))

one <- simulate_from_parameters(
  toy,
  parameters = list(level = 10, slope = 2),
  seed = 1
)
stopifnot(all(one == c(10, 12, 14, 16, 18)))

cat("PASS: generic simulator interface\n")

# Reporting model of the SEIR simulator: reported cases from true cases.
set.seed(1)
true_cases <- rep(c(0, 50, 200, 1000), each = 7)
reported <- epiworldRcalibrate:::observe_cases(true_cases, report_rate = 0.3)
stopifnot(all(reported[true_cases == 0] == 0))
stopifnot(abs(sum(reported) / sum(true_cases) - 0.3) < 0.02)

# The noise uses the simulation seed and leaves the caller's random numbers alone.
set.seed(5); before <- runif(1); set.seed(5)
a <- epiworldRcalibrate:::with_seed(3, epiworldRcalibrate:::observe_cases(true_cases, 0.5, 0.1, 0.3))
after <- runif(1)
b <- epiworldRcalibrate:::with_seed(3, epiworldRcalibrate:::observe_cases(true_cases, 0.5, 0.1, 0.3))
stopifnot(identical(a, b), identical(before, after))

cat("PASS: reporting model\n")
