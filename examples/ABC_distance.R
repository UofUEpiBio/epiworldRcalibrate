# ABC on the whole curve or on summary statistics: the package's ABC-MCMC and ABC-SMC with their defaults,
# comparing the whole curve day by day (root mean squared error) instead of the summary statistics used in the
# paper, on 216 windows of the 15-365-day test (108 per model, 36 early, middle and late), scored like the main
# test (500 model runs from the estimate). The summary-statistics versions are taken from the main test.
#
# Run from examples/ (a work pool: each job takes the next fit nobody has started):
#   Rscript ABC_distance.R pool        the fits and their curves
#   Rscript ABC_distance.R collect     results/abc_distance_w15/results.csv (with the summary-statistics versions)
# LEFTOVERS=1 (once every pool job has ended): also fit windows whose lock was left by a stopped job.
args <- commandArgs(TRUE)
stage <- args[1]
suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
SIM_DIR <- "results/simulated_test_15_365"
OUT <- "results/abc_distance_w15"; dir.create(file.path(OUT, "fits"), recursive = TRUE, showWarnings = FALSE)
MODEL_DIRS <- c(SEIR = "results/seir_clean_w15", SIR = "results/sir_clean_w15")
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
N_REPS <- 500L
METHODS <- c("ABC-MCMC", "ABC-SMC")
LEFTOVERS <- identical(Sys.getenv("LEFTOVERS"), "1")

windows <- readRDS(file.path(SIM_DIR, "windows.rds"))
set.seed(14)
ids <- unlist(lapply(names(MODEL_DIRS), function(M) lapply(c("early", "middle", "late"), function(p) {
  x <- names(windows)[vapply(windows, function(w) w$model == M && w$position == p, logical(1))]
  sample(x, 36)
})))
tasks <- expand.grid(id = ids, method = METHODS, stringsAsFactors = FALSE)
fit_file <- function(id, m) file.path(OUT, "fits", sprintf("%s_%s.rds", id, m))

# As in Simulated_test.R.
curve_scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper), MAE = mean(abs(b$median - y)))
}

if (stage == "pool") {
  done <- parallel::mclapply(sample(nrow(tasks)), function(k) {
    id <- tasks$id[k]; m <- tasks$method[k]; f <- fit_file(id, m)
    if (file.exists(f) || (!dir.create(paste0(f, ".lock"), showWarnings = FALSE) && !LEFTOVERS)) return(NULL)
    w <- windows[[id]]; info <- INFO[[w$model]]
    fit <- fit_baseline(m, w$y, info, w$known)
    if (!isTRUE(fit$failed)) {
      set.seed(match(id, names(windows)))
      b <- band_of(info, fit$draws, w$length, n = N_REPS)
      fit$scores <- curve_scores(b$new, w$y, seq_len(w$length))
      fit$draws <- NULL
    }
    saveRDS(fit, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f); unlink(paste0(f, ".lock"), recursive = TRUE)
    TRUE
  }, mc.cores = CORES, mc.preschedule = FALSE)
  cat(format(Sys.time()), "fits this job:", sum(vapply(done, isTRUE, logical(1))), "\n")
}

if (stage == "collect") {
  rows <- lapply(seq_len(nrow(tasks)), function(k) {
    id <- tasks$id[k]; m <- tasks$method[k]; w <- windows[[id]]; f <- fit_file(id, m)
    if (!file.exists(f)) return(NULL)
    r <- readRDS(f)
    if (isTRUE(r$failed)) return(data.frame(id = id, method = m, failed = TRUE))
    data.frame(id = id, method = m, failed = FALSE, R0_error = abs(r$R0 - w$R0),
               state_error = 100 * mean(abs(r$state[names(w$state)] - w$state)),
               new_coverage = r$scores[["coverage"]], new_MAE = r$scores[["MAE"]], seconds = r$seconds)
  })
  new <- as.data.frame(data.table::rbindlist(rows, fill = TRUE))
  main <- read.csv(file.path(SIM_DIR, "results_by_window.csv"))
  cols <- c("id", "method", "failed", "R0_error", "state_error", "new_coverage", "new_MAE", "seconds")
  old <- main[main$id %in% ids & main$method %in% paste0(METHODS, "-summary"), cols]
  res <- merge(rbind(old, new[, intersect(cols, names(new))]), unique(main[, c("id", "model", "position", "length")]), by = "id")
  write.csv(res, file.path(OUT, "results.csv"), row.names = FALSE)
  ok <- res[!res$failed, ]
  print(aggregate(cbind(R0_error, state_error, new_coverage, new_MAE, seconds) ~ model + method, ok, mean))
  cat("fits done:", nrow(new), "of", nrow(tasks), "; failed:", sum(new$failed), "\n")
}
