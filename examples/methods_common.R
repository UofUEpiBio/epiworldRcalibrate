# Shared code for the comparison reports (Compare_methods_simulated.Rmd,
# Clean_real_data.Rmd, Clean_states.Rmd): the epiworldR model run, the BiLSTM with
# its R0 range, the three standard baselines (ABC, DE, Nelder-Mead), and the bands
# and scores used to compare them.
#
# The baselines are deliberately simple and standard, with default settings:
#   ABC-MCMC     the package's ABC-MCMC (make_calibrator("ABC-MCMC")) with its
#                defaults: 3,000 random-walk steps (1,500 burn-in), proposal sd 8% of
#                each range, Gaussian kernel with tolerance 0.30 x the root mean
#                square of the observed counts, root mean squared error distance.
#   ABC-SMC      the package's ABC-SMC (make_calibrator("ABC-SMC")) with its
#                defaults (200 particles, 6 generations, each tolerance the median
#                distance of the previous generation), except its limits: at most
#                30,000 model runs and 20,000 attempts per generation instead of
#                8,000 and 5,000, which stopped it on many windows (its error message
#                asks for this). If it still runs out, the window is recorded as failed.
#   DE           DEoptim::DEoptim with its default settings (population 10 x the
#                number of unknowns, 200 generations, F = 0.8, CR = 0.5).
#   Nelder-Mead  stats::optim(method = "Nelder-Mead") with its default settings,
#                started at the middle of the ranges; values outside the ranges are
#                rejected.
# All three use the same unknowns and ranges as the BiLSTM: R0 in [0.8, 5] and the
# state on the first day of the window (shares exposed (SEIR), infected and
# recovered, searched as log10 shares from 1e-4 to 0.4, 0.95 for recovered), with
# crate fixed at the middle of its range (only R0 = ptran x crate / recov shapes a
# curve). The fit is the sum of squared differences between the observed daily
# counts and the model's daily new infections. DE and Nelder-Mead use one model run
# with a fixed seed per candidate, so the same candidate always gets the same fit.

STATE_OFFSET <- 1e-4
BASELINES <- c("ABC-MCMC", "ABC-SMC", "DE", "NelderMead")
METHOD_COLS <- c(BiLSTM = "forestgreen", `ABC-MCMC` = "firebrick", `ABC-SMC` = "purple",
                 DE = "goldenrod", NelderMead = "steelblue")

model_info <- function(M) {
  seir <- M == "SEIR"
  list(model = M, seir = seir,
       known = if (seir) c("n", "recov", "incub") else c("n", "recov"),
       states = if (seir) c("exposed_share", "infected_share", "recovered_share") else c("infected_share", "recovered_share"),
       crate_range = if (seir) c(5, 15) else c(1, 5))
}

cap_shares <- function(sh) sh * min(1, 0.95 / max(sum(sh), 1e-12))

# Run the model for `days` days from a starting state. Returns the daily number of
# people who became infectious (E -> I for SEIR, S -> I for SIR) and the number
# infectious at the start of each day.
run_model <- function(info, p, days, seed) {
  if (info$seir) {
    E <- p$exposed_share; I <- p$infected_share; R <- p$recovered_share
    carriers <- E + I
    m <- epiworldR::ModelSEIRCONN(name = "sim", n = p$n, prevalence = carriers, contact_rate = p$crate,
                                  incubation_days = p$incub, transmission_rate = p$ptran, recovery_rate = p$recov)
    if (I > 0 || R > 0) epiworldR::initial_states(m, c(if (carriers > 0) I / carriers else 0, R / (1 - carriers)))
  } else {
    I <- p$infected_share; R <- p$recovered_share
    m <- epiworldR::ModelSIRCONN(name = "sim", n = p$n, prevalence = I, contact_rate = p$crate,
                                 transmission_rate = p$ptran, recovery_rate = p$recov)
    if (R > 0) epiworldR::initial_states(m, R / (1 - I))
  }
  epiworldR::verbose_off(m)
  epiworldR::run(m, ndays = days + 1L, seed = seed)
  tr <- epiworldR::get_hist_transition_matrix(m)
  tr <- tr[tr$state_from == (if (info$seir) "Exposed" else "Susceptible") & tr$state_to == "Infected", ]
  new <- tr$counts[match(seq_len(days), tr$date)]
  new[is.na(new)] <- 0
  h <- epiworldR::get_hist_total(m)
  infected <- h$counts[h$state == "Infected"][match(seq_len(days) - 1, h$date[h$state == "Infected"])]
  list(new = as.numeric(new), infected = as.numeric(infected))
}

# ---- The baselines --------------------------------------------------------

baseline_space <- function(info) {
  sh <- paste0("log10_", info$states)
  list(names = c("R0", sh),
       lower = c(0.8, rep(log10(STATE_OFFSET), length(sh))),
       upper = c(5, ifelse(info$states == "recovered_share", log10(0.95), log10(0.4))))
}
# Model inputs from a candidate (R0 and log10 shares).
baseline_pars <- function(info, theta, known) {
  crate <- mean(info$crate_range)
  sh <- cap_shares(10^theta[-1]); names(sh) <- info$states
  c(as.list(known), list(ptran = theta[[1]] * known$recov / crate, crate = crate), as.list(sh))
}
baseline_result <- function(theta, info, known, seconds, runs, draws = NULL, name) {
  names(theta) <- baseline_space(info)$names
  list(method = name, failed = FALSE, R0 = theta[["R0"]],
       R0_low = if (is.null(draws)) NA else quantile(draws[, 1], 0.025, names = FALSE),
       R0_high = if (is.null(draws)) NA else quantile(draws[, 1], 0.975, names = FALSE),
       state = setNames(cap_shares(10^theta[-1]), info$states),
       draws = if (is.null(draws)) list(baseline_pars(info, theta, known)) else
         lapply(seq_len(nrow(draws)), function(i) baseline_pars(info, draws[i, ], known)),
       seconds = seconds, model_runs = runs)
}

fit_de <- function(y, info, known, seed = 1L) {
  sp <- baseline_space(info)
  runs <- 0L
  sse <- function(theta) {
    runs <<- runs + 1L
    sum((y - run_model(info, baseline_pars(info, theta, known), length(y), seed = seed)$new)^2)
  }
  set.seed(seed)
  t0 <- proc.time()[["elapsed"]]
  fit <- DEoptim::DEoptim(sse, sp$lower, sp$upper, control = DEoptim::DEoptim.control(trace = FALSE))
  baseline_result(as.numeric(fit$optim$bestmem), info, known, proc.time()[["elapsed"]] - t0, runs, name = "DE")
}

fit_nm <- function(y, info, known, seed = 1L) {
  sp <- baseline_space(info)
  runs <- 0L
  sse <- function(theta) {
    if (any(theta < sp$lower | theta > sp$upper)) return(Inf)
    runs <<- runs + 1L
    sum((y - run_model(info, baseline_pars(info, theta, known), length(y), seed = seed)$new)^2)
  }
  t0 <- proc.time()[["elapsed"]]
  fit <- stats::optim((sp$lower + sp$upper) / 2, sse, method = "Nelder-Mead")
  baseline_result(fit$par, info, known, proc.time()[["elapsed"]] - t0, runs, name = "NelderMead")
}

# The package's ABC-MCMC or ABC-SMC with their default settings, on the same unknowns
# and ranges (the simulator maps R0 and the log10 shares to the model inputs).
fit_package_abc <- function(method, y, info, known, seed = 2025L) {
  sp <- baseline_space(info)
  sim <- new_simulator(
    sample_parameters = function(n_sims) stop("Not used."),
    run = function(parameters, seed) {
      run_model(info, baseline_pars(info, unlist(parameters[sp$names]), parameters[info$known]), length(y), seed)$new
    }, type = info$model)
  bounds <- stats::setNames(lapply(seq_along(sp$names), function(j) c(sp$lower[j], sp$upper[j])), sp$names)
  cal <- if (method == "ABC-SMC") make_calibrator(method, sim, bounds, info$known, sim_budget = 30000L, max_attempt_mult = 100L) else
    make_calibrator(method, sim, bounds, info$known)
  set.seed(seed)
  t0 <- proc.time()[["elapsed"]]
  res <- do.call(calibrate, c(list(y, cal), as.list(known)))
  post <- res$posterior
  draws <- if (is.matrix(post)) post else
    post$particles[sample(nrow(post$particles), 500, replace = TRUE, prob = post$weights), , drop = FALSE]
  draws <- as.matrix(draws[, sp$names, drop = FALSE])
  est <- calibration_estimate(res)[sp$names]
  baseline_result(as.numeric(est), info, known, proc.time()[["elapsed"]] - t0, res$diagnostics$n_model_runs, draws, method)
}

# A failed fit (for example ABC-SMC running out of model runs) is recorded, not fatal.
fit_baseline <- function(method, y, info, known) {
  t0 <- proc.time()[["elapsed"]]
  tryCatch(switch(method, `ABC-MCMC` = fit_package_abc("ABC-MCMC", y, info, known),
                  `ABC-SMC` = fit_package_abc("ABC-SMC", y, info, known), DE = fit_de(y, info, known),
                  NelderMead = fit_nm(y, info, known)),
           error = function(e) list(method = method, failed = TRUE, error = conditionMessage(e),
                                    seconds = proc.time()[["elapsed"]] - t0, model_runs = NA))
}

# ---- The BiLSTM -----------------------------------------------------------------
# Average of the seeds' estimates; R0 range and parameter draws from the K
# calibration windows with the most similar estimates, known values and length.

make_bilstm <- function(info, model_dir, seeds, device = "cpu", k = 500L, n_draws = 500L) {
  lg <- function(x) log(x + STATE_OFFSET)
  dirs <- file.path(model_dir, paste0("model_seed_", seeds))
  predict <- function(curves, known) {
    known <- as.data.frame(known)[, info$known, drop = FALSE]
    ests <- lapply(dirs, function(d) bilstm_predict_many(d, unname(curves), known, device = device))
    avg <- Reduce(`+`, ests) / length(ests)
    for (s in info$states) avg[[s]] <- pmax(avg[[s]] - STATE_OFFSET, 0)
    avg
  }
  calib <- readRDS(file.path(model_dir, "calibration_windows.rds"))
  ce <- predict(calib$cases, calib$theta)
  features <- function(e, known, L) {
    st <- matrix(sapply(info$states, function(s) lg(e[[s]])), nrow = nrow(e))
    out <- cbind(e$R0, st, known$recov, log(L))
    if (info$seir) out <- cbind(out, known$incub)
    out
  }
  Fc <- features(ce, calib$theta, calib$length)
  mu <- colMeans(Fc); sdv <- apply(Fc, 2, stats::sd); Ft <- t(scale(Fc, mu, sdv))
  resid <- data.frame(R0 = calib$theta$R0 - ce$R0, sapply(info$states, function(s) lg(calib$state[[s]]) - lg(ce[[s]])))
  # Estimates for a list of windows (curves) with their known values (data frame).
  function(curves, known) {
    known <- as.data.frame(known)
    t0 <- proc.time()[["elapsed"]]
    e_all <- predict(curves, known)
    secs <- (proc.time()[["elapsed"]] - t0) / length(curves)
    lapply(seq_along(curves), function(i) {
      e <- e_all[i, ]; kn <- known[i, info$known, drop = FALSE]
      d <- colSums((Ft - (features(e, kn, length(curves[[i]]))[1, ] - mu) / sdv)^2)
      j <- which(d <= sort(d, partial = k)[k])[seq_len(k)]
      draws <- lapply(sample(j, n_draws, replace = TRUE), function(r) {
        R0 <- max(0.05, e$R0 + resid$R0[r])
        sh <- cap_shares(vapply(info$states, function(s) max(exp(lg(e[[s]]) + resid[[s]][r]) - STATE_OFFSET, 0), 0))
        c(as.list(kn), list(ptran = e$ptran, crate = R0 * kn$recov / e$ptran), as.list(sh))
      })
      list(method = "BiLSTM", R0 = e$R0, R0_low = e$R0 + quantile(resid$R0[j], 0.025, names = FALSE),
           R0_high = e$R0 + quantile(resid$R0[j], 0.975, names = FALSE),
           state = unlist(e[info$states]), draws = draws, seconds = secs, model_runs = 0L)
    })
  }
}

# ---- Bands and scores -----------------------------------------------------------

# 95% bands of daily new infections and of the number infectious from n runs of the
# model, with parameters drawn from a method's draws (a single set for DE and
# Nelder-Mead: then the band shows only the randomness of the epidemic).
band_of <- function(info, draws, days, n = 500L) {
  pick <- if (length(draws) == n) draws else draws[sample(length(draws), n, replace = TRUE)]
  runs <- lapply(seq_len(n), function(i) run_model(info, pick[[i]], days, seed = i))
  summ <- function(x) data.frame(median = apply(x, 1, stats::median), lower = apply(x, 1, stats::quantile, 0.025),
                                 upper = apply(x, 1, stats::quantile, 0.975))
  list(new = summ(vapply(runs, `[[`, numeric(days), "new")),
       infected = summ(vapply(runs, `[[`, numeric(days), "infected")))
}
# Over the days `idx`: coverage (%), mean band width, MAE of the median, and the
# interval score (band width plus 40 times how far a day falls outside the band; a
# band cannot score well just by being wide).
score_band <- function(b, y, idx = seq_along(y)) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper), width = mean(b$upper - b$lower), MAE = mean(abs(b$median - y)),
    interval_score = mean((b$upper - b$lower) + 40 * pmax(b$lower - y, 0) + 40 * pmax(y - b$upper, 0)))
}

# ---- The renewal equation --------------------------------------------------------
# Expected daily new infectious people from a starting state, by the renewal equation
#   C_t = R0 x (S_{t-1} / n) x sum_k g_k C_{t-k},
# where g is the model's generation interval (SIR: the infectious time, geometric with
# mean 1/recov; SEIR: the incubation time, geometric with mean incub, plus the infectious
# time). Because both waiting times are geometric, the sum can be carried day by day with
# the numbers exposed, infectious and susceptible, which gives the same numbers; the
# people already exposed or infectious on the first day start it. R0 enters only through
# ptran x crate = R0 x recov. Returns the daily new infectious (E -> I for SEIR, S -> I
# for SIR) and the number infectious at the start of each day.
renewal_curve <- function(info, p, days) {
  n <- p$n; beta <- p$ptran * p$crate
  E <- if (info$seir) p$exposed_share * n else 0
  I <- p$infected_share * n
  S <- n - E - I - p$recovered_share * n
  new <- infected <- numeric(days)
  for (t in seq_len(days)) {
    infected[t] <- I
    new_inf <- S * (1 - exp(-beta * I / n))
    onset <- if (info$seir) E / p$incub else new_inf
    if (info$seir) E <- E + new_inf - onset
    I <- I + onset - p$recov * I
    S <- S - new_inf
    new[t] <- onset
  }
  list(new = new, infected = infected)
}

# 95% bands from the renewal equation: one curve per parameter draw, and on each day a
# Poisson count around the curve (the renewal equation gives the average, not the
# randomness of a single epidemic).
renewal_band <- function(info, draws, days, n = 500L) {
  pick <- if (length(draws) == n) draws else draws[sample(length(draws), n, replace = TRUE)]
  curves <- lapply(pick, function(p) renewal_curve(info, p, days))
  summ <- function(x) data.frame(median = apply(x, 1, stats::median), lower = apply(x, 1, stats::quantile, 0.025),
                                 upper = apply(x, 1, stats::quantile, 0.975))
  new <- vapply(curves, `[[`, numeric(days), "new")
  list(new = summ(matrix(stats::rpois(length(new), pmax(new, 0)), nrow = days)),
       infected = summ(vapply(curves, `[[`, numeric(days), "infected")))
}

# The initial cases by the renewal equation: with R0 fixed, the shares exposed (SEIR),
# infectious and recovered on the first day of the window that make the renewal curve
# closest to the observed counts (least squares; the curve is smooth, so a standard
# optimizer with three starts is enough). Returns the model inputs.
renewal_initial_state <- function(info, y, R0, known) {
  sp <- baseline_space(info)
  lo <- sp$lower[-1]; hi <- sp$upper[-1]
  sse <- function(z) sum((y - renewal_curve(info, baseline_pars(info, c(R0, z), known), length(y))$new)^2)
  starts <- list((lo + hi) / 2, lo + 0.25 * (hi - lo), lo + 0.75 * (hi - lo))
  fits <- lapply(starts, function(s0) stats::optim(s0, sse, method = "L-BFGS-B", lower = lo, upper = hi))
  best <- fits[[which.min(vapply(fits, `[[`, 0, "value"))]]
  baseline_pars(info, c(R0, best$par), known)
}
