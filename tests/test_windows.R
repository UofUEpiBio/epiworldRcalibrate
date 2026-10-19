library(epiworldRcalibrate)

# Scores ---------------------------------------------------------------------

# Band 10-30: inside scores the width (20), a miss of 5 above 20 + 40 * 5.
stopifnot(interval_score(25, 10, 30) == 20)
stopifnot(interval_score(35, 10, 30) == 220)
stopifnot(interval_score(8, 10, 30) == 100)
stopifnot(isTRUE(all.equal(coverage(c(5, 15, 35), c(10, 10, 10), c(30, 30, 30)), 100 / 3)))

# A window to calibrate: daily new infectious people of an SIR epidemic --------

set.seed(1)
m <- epiworldR::ModelSIRCONN(name = "test", n = 5000, prevalence = 0.002, contact_rate = 3,
                             transmission_rate = 0.1, recovery_rate = 0.15)
epiworldR::verbose_off(m)
epiworldR::run(m, ndays = 41, seed = 3)
tr <- epiworldR::get_hist_transition_matrix(m)
tr <- tr[tr$state_from == "Susceptible" & tr$state_to == "Infected", ]
cases <- tr$counts[match(1:40, tr$date)]
cases[is.na(cases)] <- 0
known <- list(n = 5000, recov = 0.15)

# The standard baselines, with small budgets so the test is quick --------------

base <- window_baselines("SIR", settings = list(
  `ABC-MCMC` = list(n_samples = 60L, burnin = 30L),
  `ABC-SMC` = list(n_particles = 20L, n_generations = 2L),
  DE = list(itermax = 3),
  NelderMead = list(maxit = 30)
))
stopifnot(identical(names(base), c("ABC-MCMC", "ABC-SMC", "DE", "NelderMead")))
for (method in names(base)) {
  fit <- do.call(calibrate, c(list(cases, base[[method]]), known))
  stopifnot(inherits(fit, "window_fit"))
  est <- calibration_estimate(fit)
  stopifnot(all(c("R0", "ptran", "crate", "infected_share", "recovered_share") %in% names(est)))
  stopifnot(est[["R0"]] >= 0.8, est[["R0"]] <= 5)
  stopifnot(fit$diagnostics$model_runs > 0)
}

# The contact rate is fixed unless ptran and crate are searched -----------------

fit <- do.call(calibrate, c(list(cases, base$DE), known))
stopifnot(identical(fit$fixed, "crate"), fit$estimate[["crate"]] == 3)
free <- window_baselines("SIR", methods = c("ABC-SMC", "DE"), unknowns = "ptran_crate", settings = list(
  `ABC-SMC` = list(n_particles = 20L, n_generations = 2L), DE = list(itermax = 3)))
for (method in names(free)) {
  fit <- do.call(calibrate, c(list(cases, free[[method]]), known))
  est <- fit$estimate
  stopifnot(length(fit$fixed) == 0, est[["crate"]] >= 1, est[["crate"]] <= 5,
            isTRUE(all.equal(est[["R0"]], est[["ptran"]] * est[["crate"]] / known$recov)))
}
fit_smc <- do.call(calibrate, c(list(cases, free$`ABC-SMC`), known))
stopifnot(all(is.finite(fit_smc$intervals)))

# Predictions: model runs and the renewal equation -----------------------------

fit <- do.call(calibrate, c(list(cases, base$DE), known))
for (method in c("simulation", "renewal")) {
  band <- predict_cases(fit, ahead = 7, method = method, n = 20)
  stopifnot(nrow(band) == 47, sum(band$in_window) == 40)
  stopifnot(all(band$new_lower <= band$new_median), all(band$new_median <= band$new_upper))
  s <- score_band(band)
  stopifnot(all(c("coverage", "width", "MAE", "interval_score") %in% names(s)), s[["coverage"]] >= 0)
}
band <- predict_cases(fit, method = "renewal", n = 20, initial_state = "renewal")
stopifnot(nrow(band) == 40)

# The renewal equation follows the model's average curve -----------------------

info <- epiworldRcalibrate:::.window_info("SIR")
p <- list(n = 5000, recov = 0.15, ptran = 0.1, crate = 3, infected_share = 0.002, recovered_share = 0)
abm <- rowMeans(vapply(1:40, function(s) epiworldRcalibrate:::.run_window_model(info, p, 60, s)$new, numeric(60)))
ren <- epiworldRcalibrate:::.renewal_curve(info, p, 60)$new
stopifnot(abs(sum(ren) / sum(abm) - 1) < 0.1)

# compare_methods() on two windows ----------------------------------------------

windows <- list(list(cases = cases, n = 5000, recov = 0.15, R0 = 2, ptran = 0.1, crate = 3, position = "beginning"),
                list(cases = cases[11:40], n = 5000, recov = 0.15, R0 = 2, ptran = 0.1, crate = 3, position = "later"))
cmp <- compare_methods(windows, "SIR", calibrators = base["DE"], n = 20)
stopifnot(nrow(cmp) == 2, all(!cmp$failed), all(is.finite(cmp$R0_error)), all(is.finite(cmp$ptran_error)),
          all(cmp$crate_error == 0), identical(cmp$position, c("beginning", "later")))

# compare_methods() with the BiLSTM and forked processes, after Python is loaded
# (torch can hang in a forked process, so the BiLSTM is fitted in this one) ----

bilstm <- window_calibrator("SIR")
invisible(calibrate(cases, bilstm, n = 5000, recov = 0.15))
cmp <- compare_methods(windows, "SIR", calibrators = c(list(BiLSTM = bilstm), base["DE"]), n = 20, cores = 2)
stopifnot(nrow(cmp) == 4, all(!cmp$failed), identical(cmp$method, c("BiLSTM", "DE", "BiLSTM", "DE")))

# Real data and plots --------------------------------------------------------------

rw <- real_data_windows()
stopifnot(length(rw) == 315, sum(vapply(rw, `[[`, "", "group") == "US states") == 270)
rw <- real_data_windows("Baltimore flu 1918", lengths = 60, positions = "middle")
stopifnot(length(rw) == 1, length(rw[[1]]$cases) == 60, length(rw[[1]]$future) == 14)
stopifnot(nrow(wave_data("Baltimore flu 1918")) == 92, nrow(flu_baltimore_1918) == 92,
          all(c("state", "date", "cases", "population") %in% names(covid_us_states)))

grDevices::pdf(NULL)
bands <- lapply(list(DE = fit, `ABC-SMC` = fit_smc), predict_cases, ahead = 5, n = 20)
sc <- plot_bands(bands, future = rep(10, 5))
stopifnot(nrow(sc) == 2)
plot_comparison(cmp)
plot_estimates(cmp, "R0")
plot_ptran_crate(list(`ABC-SMC` = fit_smc), truth = c(ptran = 0.1, crate = 3))
grDevices::dev.off()
