# The large simulated test: 10,000 new noise-free test epidemics (5,000 per model, simulated with
# the training distributions but new seeds, never used in training), one window from each. Each
# window is calibrated by the BiLSTM (on a GPU when available) and the four standard methods
# (methods_common.R; ABC on summary statistics); each method's estimates are then used to
# regenerate the epidemic (500 model runs from the estimated state, for the window and the 14
# days after it), which is compared with the true curves.
#
# Windows: a random length (30 to 180 days) at a random start among the epidemic's informative
# days (before its first week without new infections); epidemics with fewer than 30 such days are
# skipped. A window is "early", "middle" or "late" by where it starts: in the first, second or last
# third of the possible starts.
#
# Run from examples/ (each stage saves its results; a stage that is stopped can be re-run):
#   Rscript Simulated_test.R windows                         simulate the test epidemics, draw the windows
#   Rscript Simulated_test.R methods <shard> <n_shards>     the four standard methods (CPU)
#   Rscript Simulated_test.R bilstm                          the BiLSTM (BILSTM_DEVICE=cuda for a GPU)
#   Rscript Simulated_test.R curves <shard> <n_shards>      regenerated curves and scores (CPU)
#   Rscript Simulated_test.R renewal <shard> <n_shards>     the renewal equation from the BiLSTM fits
#   Rscript Simulated_test.R collect                         results_by_window.csv
args  <- commandArgs(TRUE)
stage <- args[1]
# "pool": every job takes the next task nobody has started (a lock directory per task), so jobs
# on machines of different speed share the work; otherwise a fixed shard <shard> of <n_shards>.
pool <- identical(args[2], "pool")
shard <- if (length(args) >= 3) as.integer(args[2]) else 1L
n_shards <- if (length(args) >= 3) as.integer(args[3]) else 1L

suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
# The test of the paper: windows of 15 to 365 days and the networks trained for them
#   SIMTEST=w15 Rscript Simulated_test.R ...      (results/simulated_test_15_365)
# Without SIMTEST: the earlier test of windows of 30 to 180 days with the 30-365 networks.
if (identical(Sys.getenv("SIMTEST"), "w15")) {
  OUT <- "results/simulated_test_15_365"
  MODEL_DIRS <- c(SEIR = "results/seir_clean_w15", SIR = "results/sir_clean_w15")
  L_MIN <- 15L; L_MAX <- 365L
} else {
  OUT <- "results/simulated_test_10k"
  MODEL_DIRS <- c(SEIR = "results/seir_clean_big", SIR = "results/sir_clean_big")
  L_MIN <- 30L; L_MAX <- 180L
}
SEEDS <- c(122L, 1:4)
N_PER_MODEL <- 5000L; H <- 14L; N_REPS <- 500L
# DeepIMC's R0 draws started from the state fitted by the renewal equation (instead of DeepIMC's state).
RENEWAL_START <- "BiLSTM-renewal-start"
# DeepIMC's draws weighted by how well their simulated curves match the window (weight_draws()).
WEIGHTED <- "BiLSTM-weighted"; WEIGHTING <- Sys.getenv("WEIGHTING", "best50")
BIL_METHODS <- c("BiLSTM", RENEWAL_START, WEIGHTED)
METHODS4 <- c("ABC-MCMC-summary", "ABC-SMC-summary", "DE", "NelderMead")   # ABC on summary statistics
CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
run_dir <- file.path(OUT, "method_runs"); fit_dir <- file.path(OUT, "curves")
for (d in c(OUT, run_dir, fit_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
informative_days <- function(curve, k = 7) {
  r <- rle(as.numeric(curve) == 0); hit <- which(r$values & r$lengths >= k)[1]
  if (is.na(hit)) length(curve) else sum(r$lengths[seq_len(hit)]) - r$lengths[hit]
}
unit_file <- function(id, m) file.path(run_dir, sprintf("%s_%s.rds", id, m))
curve_file <- function(id, m) file.path(fit_dir, sprintf("%s_%s.rds", id, m))
my_shard <- function(n) which((seq_len(n) - 1L) %% n_shards == shard - 1L)
win_file <- file.path(OUT, "windows.rds")

# The BiLSTM fits of both models, named by window (older files were saved without names, in the
# order of the windows of each model).
load_bilstm <- function(windows) {
  out <- list()
  for (M in names(MODEL_DIRS)) {
    if (!file.exists(file.path(OUT, sprintf("bilstm_%s.rds", M)))) next
    b <- readRDS(file.path(OUT, sprintf("bilstm_%s.rds", M)))
    if (is.null(names(b))) names(b) <- names(windows)[vapply(windows, function(w) w$model == M, logical(1))]
    out <- c(out, b)
  }
  out
}

if (stage == "windows") {
  windows <- list()
  for (M in names(MODEL_DIRS)) {
    info_p <- epiworldRcalibrate:::.window_info(M)
    f_test <- file.path(OUT, sprintf("test_epidemics_%s.rds", M))
    if (!file.exists(f_test)) {
      t0 <- proc.time()[["elapsed"]]
      saveRDS(epiworldRcalibrate:::.simulate_window_epidemics(info_p, n_keep = 5600L, n_days = 365L,
        priors = epiworldRcalibrate:::.window_priors(M), min_cases = 20L, seed = if (M == "SEIR") 31001L else 32001L,
        cores = CORES), f_test)
      cat(M, "test epidemics simulated in", round(proc.time()[["elapsed"]] - t0), "seconds\n")
    }
    test <- readRDS(f_test)
    active <- apply(test$cases, 1, informative_days)
    epi <- which(active >= L_MIN)
    if (length(epi) < N_PER_MODEL) stop("Only ", length(epi), " test epidemics with ", L_MIN, " informative days.")
    epi <- epi[seq_len(N_PER_MODEL)]
    set.seed(if (M == "SEIR") 2201 else 2202)
    for (k in seq_along(epi)) {
      i <- epi[k]; a <- active[i]
      L <- if (min(L_MAX, a) == L_MIN) L_MIN else sample(L_MIN:min(L_MAX, a), 1)
      s <- if (a == L) 1L else sample.int(a - L + 1, 1)
      f <- if (a == L) 0 else (s - 1) / (a - L)
      days <- s:(s + L - 1); after <- s + L - 1 + seq_len(H); after <- after[after <= ncol(test$cases)]
      id <- sprintf("%s_%04d", M, k)
      windows[[id]] <- list(
        id = id, model = M, epidemic = i, length = L, start = s,
        position = c("early", "middle", "late")[findInterval(f, c(1 / 3, 2 / 3)) + 1],
        y = as.numeric(test$cases[i, days]), known = as.list(test$theta[i, INFO[[M]]$known]),
        R0 = test$theta$R0[i], ptran = test$theta$ptran[i], crate = test$theta$crate[i],
        state = vapply(INFO[[M]]$states, function(st) test$states[[st]][i, s], 0),
        infected = test$states$infected_share[i, days] * test$theta$n[i],
        future = as.numeric(test$cases[i, after]),
        future_infected = test$states$infected_share[i, after] * test$theta$n[i])
    }
  }
  saveRDS(windows, win_file)
  w <- do.call(rbind, lapply(windows, function(x) data.frame(model = x$model, length = x$length, position = x$position)))
  print(table(w$model, w$position)); print(tapply(w$length, w$model, summary))
}

if (stage == "methods") {
  windows <- readRDS(win_file)
  units <- expand.grid(id = names(windows), method = METHODS4, stringsAsFactors = FALSE)
  if (!pool) units <- units[my_shard(nrow(units)), ]
  units <- units[!file.exists(unit_file(units$id, units$method)), ]
  units <- units[order(-vapply(units$id, function(i) windows[[i]]$length, 0)), ]   # long windows first
  cat(format(Sys.time()), if (pool) "pool" else paste("shard", shard, "of", n_shards), ":", nrow(units),
      "fits to do on", CORES, "cores\n")
  done <- parallel::mclapply(seq_len(nrow(units)), function(k) try({
    out <- unit_file(units$id[k], units$method[k]); lock <- paste0(out, ".lock")
    if (file.exists(out) || (pool && !dir.create(lock, showWarnings = FALSE))) return(NULL)
    w <- windows[[units$id[k]]]
    saveRDS(fit_baseline(units$method[k], w$y, INFO[[w$model]], w$known), paste0(out, ".tmp"))
    file.rename(paste0(out, ".tmp"), out)
    if (pool) unlink(lock, recursive = TRUE)
  }), mc.cores = CORES, mc.preschedule = FALSE)
  cat(format(Sys.time()), "errors:", sum(vapply(done, inherits, logical(1), what = "try-error")), "\n")
}

if (stage == "bilstm") {
  windows <- readRDS(win_file)
  set.seed(1)
  for (M in names(MODEL_DIRS)) {
    f_out <- file.path(OUT, sprintf("bilstm_%s.rds", M))
    if (file.exists(f_out)) next
    ws <- windows[vapply(windows, function(w) w$model == M, logical(1))]
    f <- make_bilstm(INFO[[M]], MODEL_DIRS[[M]], SEEDS, device = Sys.getenv("BILSTM_DEVICE", "cpu"))
    t0 <- proc.time()[["elapsed"]]
    out <- setNames(f(lapply(ws, `[[`, "y"), do.call(rbind, lapply(ws, function(w) as.data.frame(w$known)))), names(ws))
    cat(M, length(ws), "windows in", round(proc.time()[["elapsed"]] - t0, 1), "seconds\n")
    saveRDS(out, f_out)
  }
}

# Scores of a regenerated band against the true values on the days `idx`.
curve_scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  m <- b$median
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper),
    interval_score = mean((b$upper - b$lower) + 40 * pmax(b$lower - y, 0) + 40 * pmax(y - b$upper, 0)),
    MAE = mean(abs(m - y)),
    NRMSE = if (mean(y) > 0) sqrt(mean((m - y)^2)) / mean(y) else NA,
    total_error = if (sum(y) > 0) abs(sum(m) - sum(y)) / sum(y) else NA,
    peak_day_error = abs(unname(which.max(m) - which.max(y))),
    peak_size_error = if (max(y) > 0) abs(max(m) - max(y)) / max(y) else NA,
    WIS = wis_of(b, y))
}
# Scores of the band of cumulative new infections since the first day of the window; rel_MAE is the MAE
# divided by the true cumulative count on the last scored day.
cum_scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper), MAE = mean(abs(b$median - y)),
    rel_MAE = if (y[length(y)] > 0) mean(abs(b$median - y)) / y[length(y)] else NA, WIS = wis_of(b, y))
}
CURVE_METRICS <- c("coverage", "interval_score", "MAE", "NRMSE", "total_error", "peak_day_error", "peak_size_error", "WIS")
CUM_METRICS <- c("coverage", "MAE", "rel_MAE", "WIS")

if (stage == "curves") {
  windows <- readRDS(win_file)
  bil <- load_bilstm(windows)
  jobs <- expand.grid(id = names(windows), method = c(BIL_METHODS, METHODS4), stringsAsFactors = FALSE)
  if (!pool) jobs <- jobs[my_shard(nrow(jobs)), ]
  is_bil <- jobs$method %in% BIL_METHODS
  jobs <- jobs[!file.exists(curve_file(jobs$id, jobs$method)) &
                 ifelse(is_bil, jobs$id %in% names(bil), file.exists(unit_file(jobs$id, jobs$method))), ]
  cat(format(Sys.time()), if (pool) "pool" else paste("shard", shard, "of", n_shards), ":", nrow(jobs),
      "bands to do on", CORES, "cores\n")
  done <- parallel::mclapply(seq_len(nrow(jobs)), function(k) try({
    out <- curve_file(jobs$id[k], jobs$method[k]); lock <- paste0(out, ".lock")
    if (file.exists(out) || (pool && !dir.create(lock, showWarnings = FALSE))) return(NULL)
    on.exit(if (pool) unlink(lock, recursive = TRUE))
    w <- windows[[jobs$id[k]]]; m <- jobs$method[k]; info <- INFO[[w$model]]
    set.seed(k)
    f <- if (m %in% BIL_METHODS) bil[[w$id]] else readRDS(unit_file(w$id, m))
    if (isTRUE(f$failed)) { saveRDS(list(failed = TRUE), curve_file(w$id, m)); return(NULL) }
    nf <- length(w$future); L <- w$length
    t0 <- proc.time()[["elapsed"]]
    start <- NULL; weighted <- NULL
    if (m == WEIGHTED) {
      weighted <- weight_draws(info, f$draws, w$y, WEIGHTING, seed = 1000L * k)
      f$draws <- weighted$draws; weighted$draws <- NULL
    }
    if (m == RENEWAL_START) {
      start <- renewal_initial_state(info, w$y, f$R0, w$known)[info$states]
      f$draws <- lapply(f$draws, function(p) { p[info$states] <- start; p })
    }
    b <- band_of(info, f$draws, L + nf, n = N_REPS)
    secs <- proc.time()[["elapsed"]] - t0
    out <- list(new = curve_scores(b$new, w$y, seq_len(L)),
                infected = curve_scores(b$infected, w$infected, seq_len(L)),
                future = if (nf) curve_scores(b$new, w$future, L + seq_len(nf)) else NULL,
                future_infected = if (nf) curve_scores(b$infected, w$future_infected, L + seq_len(nf)) else NULL,
                cumulative = cum_scores(b$cumulative, cumsum(w$y), seq_len(L)),
                future_cumulative = if (nf) cum_scores(b$cumulative, sum(w$y) + cumsum(w$future), L + seq_len(nf)) else NULL,
                band = lapply(b, function(x) round(x, 2)), band_seconds = secs, state = unlist(start), weighted = weighted)
    saveRDS(out, paste0(curve_file(w$id, m), ".tmp")); file.rename(paste0(curve_file(w$id, m), ".tmp"), curve_file(w$id, m))
  }), mc.cores = CORES, mc.preschedule = FALSE)
  cat(format(Sys.time()), "errors:", sum(vapply(done, inherits, logical(1), what = "try-error")), "\n")
}

# The renewal equation instead of 500 model runs, from DeepIMC's estimates: started from DeepIMC's
# state (A), or from a state worked out by the renewal equation with R0 at 30 values of DeepIMC's
# range (B). Scored against the true curves like the model-run bands.
if (stage == "renewal") {
  windows <- readRDS(win_file)
  bil <- load_bilstm(windows)
  ids <- names(windows)[my_shard(length(windows))]
  # Keep only this shard's windows, so that the forked workers do not each copy all the fits.
  seeds <- setNames(match(ids, names(windows)), ids)
  windows <- windows[ids]; bil <- bil[ids]; invisible(gc())
  out <- parallel::mclapply(ids, function(id) {
    set.seed(seeds[[id]])
    w <- windows[[id]]; f <- bil[[id]]; info <- INFO[[w$model]]
    L <- w$length; nf <- length(w$future); days <- L + nf
    t0 <- proc.time()[["elapsed"]]; bA <- renewal_band(info, f$draws, days); tA <- proc.time()[["elapsed"]] - t0
    t0 <- proc.time()[["elapsed"]]
    R0s <- stats::quantile(vapply(f$draws, function(p) p$ptran * p$crate / p$recov, 0), seq(0.02, 0.98, length.out = 30), names = FALSE)
    bB <- renewal_band(info, lapply(R0s, function(r) renewal_initial_state(info, w$y, r, w$known)), days)
    tB <- proc.time()[["elapsed"]] - t0
    pB <- renewal_initial_state(info, w$y, f$R0, w$known)
    one <- function(b, label, secs, state) {
      sc <- function(x, prefix) setNames(as.list(x), paste0(prefix, "_", names(x)))
      data.frame(model = w$model, id = id, position = w$position, length = L, approach = label, seconds = secs,
                 state_error = 100 * mean(abs(unlist(state[names(w$state)]) - w$state)),
                 as.data.frame(c(sc(curve_scores(b$new, w$y, seq_len(L)), "new"),
                                 sc(curve_scores(b$infected, w$infected, seq_len(L)), "infected"),
                                 if (nf) sc(curve_scores(b$new, w$future, L + seq_len(nf)), "future") else list())))
    }
    rbind(one(bA, "renewal A", tA, as.list(f$state)), one(bB, "renewal B", tB, pB))
  }, mc.cores = CORES, mc.preschedule = FALSE)
  bad <- !vapply(out, is.data.frame, logical(1))
  cat("renewal windows:", length(out), "errors:", sum(bad), "\n")
  cols <- unique(unlist(lapply(out[!bad], names)))
  res <- do.call(rbind, lapply(out[!bad], function(r) { for (c in setdiff(cols, names(r))) r[[c]] <- NA; r[cols] }))
  write.csv(res, file.path(OUT, sprintf("renewal_%d.csv", shard)), row.names = FALSE)
}

if (stage == "unlock") {
  locks <- c(list.files(run_dir, "\\.lock$", full.names = TRUE), list.files(fit_dir, "\\.lock$", full.names = TRUE))
  tmps <- c(list.files(run_dir, "\\.tmp$", full.names = TRUE), list.files(fit_dir, "\\.tmp$", full.names = TRUE))
  unlink(c(locks, tmps), recursive = TRUE)
  cat("removed", length(locks), "stale locks and", length(tmps), "partial files\n")
}

if (stage == "collect") {
  windows <- readRDS(win_file)
  bil <- load_bilstm(windows)
  rows <- list()
  for (id in names(windows)) for (m in c(BIL_METHODS, METHODS4)) {
    w <- windows[[id]]
    f <- if (m %in% BIL_METHODS) bil[[id]] else if (file.exists(unit_file(id, m))) readRDS(unit_file(id, m)) else NULL
    s <- if (file.exists(curve_file(id, m))) readRDS(curve_file(id, m)) else NULL
    if (m == RENEWAL_START && !is.null(f) && !is.null(s$state)) f$state <- s$state
    # The weighted version keeps DeepIMC's R0 and range; its starting state is that of the chosen draws.
    if (m == WEIGHTED && !is.null(f) && !is.null(s$weighted)) f$state <- s$weighted$state
    base <- data.frame(model = w$model, id = id, epidemic = w$epidemic, length = w$length, position = w$position,
                       method = m, R0_true = w$R0, ptran_true = w$ptran, crate_true = w$crate, recov = w$known$recov)
    if (is.null(f) || isTRUE(f$failed)) {
      rows[[length(rows) + 1]] <- cbind(base, failed = TRUE, done = !is.null(f)); next
    }
    # The BiLSTM's ptran is the network's estimate (the same in every draw); the standard
    # methods fix crate at the middle of its range, so their ptran = R0 x recov / crate.
    ptran <- if (m %in% BIL_METHODS) f$draws[[1]]$ptran else f$R0 * w$known$recov / f$draws[[1]]$crate
    sc <- function(part) if (is.null(s) || is.null(s[[part]])) NA else s[[part]]
    flat <- function(part) {
      v <- sc(part)
      metrics <- if (part %in% c("cumulative", "future_cumulative")) CUM_METRICS else CURVE_METRICS
      if (length(v) == 1 && is.na(v)) v <- setNames(rep(NA, length(metrics)), metrics)
      setNames(as.list(v), paste0(part, "_", names(v))) }
    rows[[length(rows) + 1]] <- cbind(base, failed = FALSE, done = TRUE,
      R0 = f$R0, R0_low = f$R0_low, R0_high = f$R0_high, ptran = ptran, crate = f$R0 * w$known$recov / ptran,
      state_error = 100 * mean(abs(f$state[names(w$state)] - w$state)),
      as.data.frame(c(flat("new"), flat("infected"), flat("future"), flat("future_infected"),
                      flat("cumulative"), flat("future_cumulative"))),
      seconds = f$seconds, model_runs = f$model_runs, band_seconds = if (is.null(s)) NA else s$band_seconds,
      fit_best = if (is.null(s$weighted)) NA else s$weighted$fit_best)
  }
  res <- as.data.frame(data.table::rbindlist(rows, fill = TRUE))
  res$R0_error <- abs(res$R0 - res$R0_true)
  res$ptran_error <- abs(res$ptran - res$ptran_true)
  res$crate_error <- abs(res$crate - res$crate_true)
  res$inside <- ifelse(is.na(res$R0_low), NA, res$R0_true >= res$R0_low & res$R0_true <= res$R0_high)
  write.csv(res, file.path(OUT, "results_by_window.csv"), row.names = FALSE)
  cat("rows", nrow(res), "; fits done", sum(res$done), "; failed", sum(res$failed & res$done),
      "; curves done", sum(!is.na(res$new_coverage)), "\n")
}
