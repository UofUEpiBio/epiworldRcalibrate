# Does estimating the starting state help? DeepIMC's curves (500 model runs from its draws) with the
# estimated state on the first day of the window, against the same R0 draws started from the state a
# user without state estimation would assume: the window is the start of the epidemic, nobody has
# recovered, and the infected (and, for SEIR, exposed) count equals the first day's count.
# Scored on the window, its first 7 days and the 14 days after it, on the real windows of
# Real_data_five.R and on the 1,000 simulated windows of DE_typical_user.R (where the true number
# currently infected is known too).
#
# Run from examples/:  Rscript State_ablation.R real | simulated   ->  results/state_ablation/results_<set>.csv
# With SIMTEST=w15: the networks for windows of 15-365 days, 1,000 windows of the 15-365-day test
# (results/state_ablation_w15/).
suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
W15 <- identical(Sys.getenv("SIMTEST"), "w15")
OUT <- if (W15) "results/state_ablation_w15" else "results/state_ablation"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
MODEL_DIRS <- c(SEIR = "results/seir_clean_big", SIR = "results/sir_clean_big")
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
N_REPS <- 500L

scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper), MAE = mean(abs(b$median - y)))
}
naive_draws <- function(draws, y1, n, seir) {
  s <- max(y1, 1) / n
  lapply(draws, function(p) { p$infected_share <- s; p$recovered_share <- 0; if (seir) p$exposed_share <- s; p })
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

run_set <- function(set, windows, fits) {
  rows <- parallel::mclapply(names(windows), function(id) {
    w <- windows[[id]]; info <- INFO[[w$model]]; f <- fits[[id]]
    L <- w$length; nf <- length(w$future)
    do.call(rbind, lapply(c("estimated", "naive"), function(start) {
      set.seed(match(id, names(windows)))
      draws <- if (start == "estimated") f$draws else naive_draws(f$draws, w$y[1], w$known$n, info$seir)
      b <- band_of(info, draws, L + nf, n = N_REPS)
      sc <- c(window = scores(b$new, w$y, seq_len(L)), first7 = scores(b$new, w$y, seq_len(min(7, L))),
              if (nf) c(future = scores(b$new, w$future, L + seq_len(nf))),
              if (!is.null(w$infected)) c(infected = scores(b$infected, w$infected, seq_len(L))))
      data.frame(set = set, id = id, model = w$model, dataset = if (is.null(w$dataset)) "simulated" else w$dataset,
                 position = w$position, length = L, start = start, as.list(sc))
    }))
  }, mc.cores = CORES, mc.preschedule = FALSE)
  ok <- vapply(rows, is.data.frame, logical(1))
  cols <- unique(unlist(lapply(rows[ok], names)))
  do.call(rbind, lapply(rows[ok], function(r) { for (c in setdiff(cols, names(r))) r[[c]] <- NA; r[cols] }))
}

# One set per run (each saves results_<set>.csv); the fits are cut to the windows used before the
# workers are forked, so that each worker does not copy all 10,000 simulated fits.
set <- commandArgs(TRUE)[1]
if (set == "real") {
  rw <- readRDS("results/real_five/windows.rds")
  res <- run_set("real", rw, fits_of("results/real_five", rw))
} else {
  sim_dir <- if (W15) "results/simulated_test_15_365" else "results/simulated_test_10k"
  sw <- readRDS(file.path(sim_dir, "windows.rds"))
  ids <- if (W15) { set.seed(13); unlist(lapply(c("SEIR", "SIR"), function(M) sample(names(sw)[startsWith(names(sw), M)], 500))) } else
    unique(read.csv("results/de_typical_user/results.csv")$id)
  fits <- fits_of(sim_dir, sw)[ids]; sw <- sw[ids]; invisible(gc())
  res <- run_set("simulated", sw, fits)
}
write.csv(res, file.path(OUT, sprintf("results_%s.csv", set)), row.names = FALSE)
num <- intersect(c("window.coverage", "window.MAE", "first7.coverage", "first7.MAE", "future.coverage", "future.MAE",
                   "infected.coverage", "infected.MAE"), names(res))
print(aggregate(res[num], res[c("set", "model", "position", "start")], mean, na.rm = TRUE))
cat("windows:", length(unique(res$id)), "\n")
