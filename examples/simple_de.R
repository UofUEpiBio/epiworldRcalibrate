# A simple differential evolution (DE) baseline, set up the way a typical user
# would fit an epiworldR model to a curve of daily new infections:
#   - it searches the model's own inputs: transmission rate (ptran), contact rate
#     (crate) and the share infected at the start (preval; exposed for SEIR);
#   - it assumes the window is the start of an epidemic (nobody recovered);
#   - the fit is the sum of squared differences between the observed daily counts
#     and one model run with a fixed seed;
#   - DEoptim with its default settings (population 10 x the number of
#     parameters, 200 generations, F = 0.8, CR = 0.5).
# The ranges are those of the BiLSTM's training data, so neither method knows more:
# crate as in training, preval 0.001-0.4, and ptran from R0 0.8-5 and recov
# 0.071-0.25. R0 = ptran x crate / recov is computed from the estimates.
#
# `run(p, L, seed)` runs the model from the parameter list `p` (n, recov, incub,
# ptran, crate and the shares exposed/infected/recovered) for L days and returns
# the daily new infections.
simple_de_pars <- function(model, est, known) {
  p <- c(as.list(known), list(ptran = est[["ptran"]], crate = est[["crate"]]))
  if (model == "SEIR") c(p, list(exposed_share = est[["preval"]], infected_share = 0, recovered_share = 0)) else
    c(p, list(infected_share = est[["preval"]], recovered_share = 0))
}

simple_de <- function(y, model, known, crate_range, run, seed = 1) {
  known <- as.list(known)
  L <- length(y)
  lower <- c(ptran = 0.8 * 0.071 / crate_range[2], crate = crate_range[1], preval = 0.001)
  upper <- c(ptran = min(1, 5 * 0.25 / crate_range[1]), crate = crate_range[2], preval = 0.4)
  sse <- function(par) {
    names(par) <- names(lower)
    sum((y - run(simple_de_pars(model, par, known), L, seed))^2)
  }
  set.seed(seed)
  t0 <- proc.time()[["elapsed"]]
  fit <- DEoptim::DEoptim(sse, lower, upper, control = DEoptim::DEoptim.control(trace = FALSE))
  est <- stats::setNames(as.numeric(fit$optim$bestmem), names(lower))
  list(estimate = c(est, R0 = est[["ptran"]] * est[["crate"]] / known$recov),
       seconds = proc.time()[["elapsed"]] - t0, model_runs = fit$optim$nfeval)
}
