# DE as a typical user would run it: DEoptim with its defaults, as in the paper, but the stochastic
# model is called with a new random seed for every model run, instead of one fixed seed for all
# candidates (common random numbers). On 1,000 windows of the 10,000-window test (500 per model, a
# third early, middle and late), scored like the main test (500 model runs from the estimate).
#
# Run from examples/ (a work pool: each job takes the next window nobody has started):
#   Rscript DE_typical_user.R pool        the fits and their curves
#   Rscript DE_typical_user.R collect     results/de_typical_user/results.csv (with DE and the BiLSTM)
# With SIMTEST=w15: 1,000 windows of the 15-365-day test (results/simulated_test_15_365) and the networks for
# 15-365 days -> results/de_typical_user_w15. Without it: the earlier 30-180-day test.
args <- commandArgs(TRUE)
stage <- args[1]
suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
W15 <- identical(Sys.getenv("SIMTEST"), "w15")
SIM_DIR <- if (W15) "results/simulated_test_15_365" else "results/simulated_test_10k"
OUT <- if (W15) "results/de_typical_user_w15" else "results/de_typical_user"
dir.create(file.path(OUT, "fits"), recursive = TRUE, showWarnings = FALSE)
MODEL_DIRS <- if (W15) c(SEIR = "results/seir_clean_w15", SIR = "results/sir_clean_w15") else
  c(SEIR = "results/seir_clean_big", SIR = "results/sir_clean_big")
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
N_REPS <- 500L

windows <- readRDS(file.path(SIM_DIR, "windows.rds"))
set.seed(12)
ids <- unlist(lapply(names(MODEL_DIRS), function(M) {
  x <- names(windows)[vapply(windows, function(w) w$model == M, logical(1))]
  pos <- vapply(windows[x], `[[`, "", "position")
  unlist(lapply(split(x, pos), function(z) sample(z, round(500 * length(z) / length(x)))))
}))
fit_file <- function(id) file.path(OUT, "fits", paste0(id, ".rds"))

# As in Simulated_test.R.
curve_scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  m <- b$median
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper), MAE = mean(abs(m - y)))
}

# LEFTOVERS=1 (once every pool job has ended): also fit windows whose lock was left by a stopped job.
LEFTOVERS <- identical(Sys.getenv("LEFTOVERS"), "1")
if (stage == "pool") {
  done <- parallel::mclapply(sample(ids), function(id) {
    f <- fit_file(id)
    if (file.exists(f) || (!dir.create(paste0(f, ".lock"), showWarnings = FALSE) && !LEFTOVERS)) return(NULL)
    w <- windows[[id]]; info <- INFO[[w$model]]
    fit <- fit_baseline("DE-random-seed", w$y, info, w$known)
    if (!isTRUE(fit$failed)) {
      set.seed(match(id, names(windows)))
      nf <- length(w$future); L <- w$length
      b <- band_of(info, fit$draws, L + nf, n = N_REPS)
      fit$scores <- list(new = curve_scores(b$new, w$y, seq_len(L)),
                         infected = curve_scores(b$infected, w$infected, seq_len(L)),
                         future = if (nf) curve_scores(b$new, w$future, L + seq_len(nf)) else NULL)
    }
    saveRDS(fit, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f); unlink(paste0(f, ".lock"), recursive = TRUE)
    TRUE
  }, mc.cores = CORES, mc.preschedule = FALSE)
  cat(format(Sys.time()), "fits this job:", sum(vapply(done, isTRUE, logical(1))), "\n")
}

if (stage == "collect") {
  rows <- lapply(ids, function(id) {
    w <- windows[[id]]; f <- fit_file(id)
    if (!file.exists(f)) return(NULL)
    r <- readRDS(f)
    if (isTRUE(r$failed)) return(data.frame(id = id, method = "DE-random-seed", failed = TRUE))
    sc <- unlist(r$scores)
    data.frame(id = id, method = "DE-random-seed", failed = FALSE, R0_error = abs(r$R0 - w$R0),
               state_error = 100 * mean(abs(r$state[names(w$state)] - w$state)),
               new_coverage = sc[["new.coverage"]], new_MAE = sc[["new.MAE"]],
               infected_coverage = sc[["infected.coverage"]], infected_MAE = sc[["infected.MAE"]],
               future_coverage = if ("future.coverage" %in% names(sc)) sc[["future.coverage"]] else NA,
               future_MAE = if ("future.MAE" %in% names(sc)) sc[["future.MAE"]] else NA,
               seconds = r$seconds, model_runs = r$model_runs)
  })
  new <- do.call(rbind, rows)
  main <- read.csv(file.path(SIM_DIR, "results_by_window.csv"))
  cols <- c("id", "method", "failed", "R0_error", "state_error", "new_coverage", "new_MAE", "infected_coverage",
            "infected_MAE", "future_coverage", "future_MAE", "seconds", "model_runs")
  old <- main[main$id %in% new$id & main$method %in% c("BiLSTM", "DE"), cols]
  res <- merge(rbind(old, new[, intersect(cols, names(new))]), unique(main[, c("id", "model", "position", "length")]), by = "id")
  write.csv(res, file.path(OUT, "results.csv"), row.names = FALSE)
  ok <- res[!res$failed, ]
  print(aggregate(cbind(R0_error, state_error, new_coverage, new_MAE, infected_coverage, infected_MAE, future_MAE) ~ model + method,
                  ok, mean))
  cat("windows done:", length(unique(new$id)), "of", length(ids), "\n")
}
