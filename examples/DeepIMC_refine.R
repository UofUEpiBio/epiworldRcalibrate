# DeepIMC corrected by the data: each of DeepIMC's 500 parameter draws (R0 and starting state) is run once
# through the agent-based model, and the 50 draws whose simulated curves are closest to the observed window
# are kept (rejection ABC with DeepIMC's draws as the proposal); the band is then made from 10 runs of each
# kept draw. The distance is the root mean squared difference between the square roots of the simulated
# and observed daily counts (on this scale Poisson noise has a standard deviation of about 0.5), so the
# distance of the best draw also tells whether any of DeepIMC's draws fits the window (a fit check).
# Nothing is retrained: the networks, the draws and the model are those of the paper.
#
# Run from examples/:
#   Rscript DeepIMC_refine.R real        real windows of Real_data_five.R (networks for 15-365 days)
#   Rscript DeepIMC_refine.R simulated   1,000 windows of the 15-365-day test (as in State_ablation.R)
# -> results/deepimc_refine/results_<set>.csv
suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
OUT <- "results/deepimc_refine"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
MODEL_DIRS <- c(SEIR = "results/seir_clean_w15", SIR = "results/sir_clean_w15")
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
N_KEEP <- 50L; N_EACH <- 10L

# Coverage, MAE and WIS of a band (quantiles of a matrix of runs, days x runs) against y on the days idx.
band_from_runs <- function(x) {
  q <- t(apply(x, 1, stats::quantile, c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975), names = FALSE))
  data.frame(median = q[, 4], lower = q[, 1], upper = q[, 7], l80 = q[, 2], u80 = q[, 6], l50 = q[, 3], u50 = q[, 5])
}
scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper), MAE = mean(abs(b$median - y)), WIS = wis_of(b, y))
}
fits_of <- function(dir, windows) {
  out <- list()
  for (M in names(MODEL_DIRS)) {
    b <- readRDS(file.path(dir, sprintf("bilstm_%s.rds", M)))
    if (is.null(names(b))) names(b) <- names(windows)[vapply(windows, function(w) w$model == M, logical(1))][seq_along(b)]
    out <- c(out, b)
  }
  out
}

# Which of DeepIMC's 500 draws to use, from their distances d to the window; 500 indices are returned
# (a draw may repeat), so that every version makes its band from 500 runs:
#   best50    the 50 closest draws, 10 runs each (hard selection)
#   best100   the 100 closest, 5 runs each
#   soft      every draw weighted by exp(-(d / eps)^2 / 2), eps = the distance of the 50th closest draw,
#             and 500 draws resampled with these weights
pick_draws <- function(d, version) {
  switch(version,
    best50 = rep(order(d)[1:50], each = 10),
    best100 = rep(order(d)[1:100], each = 5),
    soft = { eps <- sort(d)[50]; w <- exp(-0.5 * (d / eps)^2); sample(length(d), 500, replace = TRUE, prob = w) })
}
VERSIONS <- c("best50", "best100", "soft")

refine_one <- function(w, f, seed) {
  info <- INFO[[w$model]]; L <- w$length; nf <- length(w$future); days <- L + nf
  draws <- f$draws
  # One run of every draw; distance on the square-root scale over the window.
  first <- lapply(seq_along(draws), function(i) run_model(info, draws[[i]], days, seed = seed + i))
  d <- vapply(first, function(r) sqrt(mean((sqrt(r$new[seq_len(L)]) - sqrt(w$y))^2)), 0)
  set.seed(seed)
  out <- c(fit_best = min(d), fit_median = stats::median(d))
  for (v in VERSIONS) {
    idx <- pick_draws(d, v)
    runs <- lapply(seq_along(idx), function(k) run_model(info, draws[[idx[k]]], days, seed = seed + 100000 + k))
    new <- band_from_runs(vapply(runs, `[[`, numeric(days), "new"))
    inf <- band_from_runs(vapply(runs, `[[`, numeric(days), "infected"))
    R0s <- vapply(draws[idx], function(p) p$ptran * p$crate / p$recov, 0)
    st <- sapply(info$states, function(s) stats::median(vapply(draws[idx], `[[`, 0, s)))
    o <- c(R0 = stats::median(R0s), R0_low = unname(stats::quantile(R0s, 0.025)), R0_high = unname(stats::quantile(R0s, 0.975)),
           setNames(scores(new, w$y, seq_len(L)), paste0("window_", c("coverage", "MAE", "WIS"))),
           if (nf) setNames(scores(new, w$future, L + seq_len(nf)), paste0("future_", c("coverage", "MAE", "WIS"))))
    if (!is.null(w$infected)) o <- c(o, setNames(scores(inf, w$infected, seq_len(L)), paste0("infected_", c("coverage", "MAE", "WIS"))),
                                     state_error = 100 * mean(abs(st[names(w$state)] - w$state)))
    out <- c(out, setNames(o, paste0(v, ".", names(o))))
  }
  # DeepIMC's own (calibrated) range, for comparison.
  c(out, orig.R0 = f$R0, orig.R0_low = f$R0_low, orig.R0_high = f$R0_high, R0_true = if (is.null(w$R0)) NA else w$R0)
}

set <- commandArgs(TRUE)[1]
if (set == "real") {
  windows <- readRDS("results/real_five/windows.rds")
  fits <- fits_of("results/real_five", windows)
} else {
  windows <- readRDS("results/simulated_test_15_365/windows.rds")
  set.seed(13); ids <- unlist(lapply(c("SEIR", "SIR"), function(M) sample(names(windows)[startsWith(names(windows), M)], 500)))
  fits <- fits_of("results/simulated_test_15_365", windows)[ids]; windows <- windows[ids]; invisible(gc())
}
rows <- parallel::mclapply(names(windows), function(id) {
  w <- windows[[id]]
  v <- refine_one(w, fits[[id]], seed = match(id, names(windows)))
  data.frame(id = id, model = w$model, dataset = if (is.null(w$dataset)) "simulated" else w$dataset, main = isTRUE(w$main) || is.null(w$dataset),
             length = w$length, position = w$position, as.list(v))
}, mc.cores = CORES, mc.preschedule = FALSE)
ok <- vapply(rows, is.data.frame, logical(1))
res <- as.data.frame(data.table::rbindlist(rows[ok], fill = TRUE))
write.csv(res, file.path(OUT, sprintf("versions_%s.csv", set)), row.names = FALSE)
cat(set, ": windows", nrow(res), "of", length(windows), "\n")
