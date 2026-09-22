source("R/01_simulation.R")
source("R/02_training.R")
source("R/03_test_compare.R")

toy_sim <- new_simulator(
  sample_parameters = function(n_sims) {
    data.frame(a = runif(n_sims, 0, 2), b = runif(n_sims, 0, 2))
  },
  run = function(parameters, seed) {
    c(
      parameters$a,
      parameters$a + parameters$b,
      2 * parameters$a + parameters$b
    )
  },
  type = "TOY",
  name = "toy"
)

obs <- c(1, 1.5, 2.5)
truth <- c(a = 1, b = 0.5)

nm <- make_calibrator(
  "NelderMead",
  simulator = toy_sim,
  bounds = list(a = c(0, 2), b = c(0, 2)),
  maxit = 500,
  restarts = 1,
  n_eval_seeds = 1
)

fit <- calibrate(obs, nm)
est <- calibration_estimate(fit)

stopifnot(all(c("a", "b") %in% names(est)))
stopifnot(all(is.finite(est)))

cmp <- compare_calibrators(
  observed = obs,
  calibrators = list(NelderMead = nm),
  truth = truth,
  simulator = toy_sim
)

stopifnot(nrow(cmp$summary) == 1)
stopifnot(nrow(cmp$estimates) == 2)

cat("PASS: generic baseline/benchmark interface\n")
