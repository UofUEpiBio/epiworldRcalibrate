library(epiworldRcalibrate)

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

# Nelder-Mead must search even when the box midpoint is not exactly 0 on its
# logit scale (a = 0.8 to 5 leaves a rounding error of about -4e-16 there,
# which used to make the first simplex vanish and the search stop at once).
nm_round <- make_calibrator("NelderMead", simulator = toy_sim,
                            bounds = list(a = c(0.8, 5), b = c(0, 2)),
                            maxit = 500, restarts = 1, n_eval_seeds = 1)
fit_round <- calibrate(obs, nm_round)
est_round <- calibration_estimate(fit_round)
stopifnot(fit_round$diagnostics$n_evals > 20)
stopifnot(abs(est_round[["a"]] - 1) < 0.05, abs(est_round[["b"]] - 0.5) < 0.05)

cat("PASS: Nelder-Mead searches from a rounded midpoint\n")
