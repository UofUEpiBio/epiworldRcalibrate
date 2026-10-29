# Is 6 generations enough for ABC-SMC? The summary-statistics ABC-SMC of the paper (200 particles,
# each tolerance the median distance of the previous generation) run with 6, 12 and 20 generations
# on the same 120 windows of the 10,000-window test (60 per model, 20 early, 20 middle and 20 late).
# The run budget is raised (200,000 model runs, 500 x 200 attempts per generation) so that the
# longer runs are not stopped by it.
#
# Run from examples/ (a work pool: each job takes the next fit nobody has started):
#   Rscript ABC_SMC_generations.R pool        the fits
#   Rscript ABC_SMC_generations.R collect     results/abc_smc_generations/results.csv
# With SIMTEST=w15: 120 windows of the 15-365-day test (results/simulated_test_15_365)
# -> results/abc_smc_generations_w15. Without it: the earlier 30-180-day test.
args <- commandArgs(TRUE)
stage <- args[1]
suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
W15 <- identical(Sys.getenv("SIMTEST"), "w15")
SIM_DIR <- if (W15) "results/simulated_test_15_365" else "results/simulated_test_10k"
OUT <- if (W15) "results/abc_smc_generations_w15" else "results/abc_smc_generations"
dir.create(file.path(OUT, "fits"), recursive = TRUE, showWarnings = FALSE)
MODEL_DIRS <- if (W15) c(SEIR = "results/seir_clean_w15", SIR = "results/sir_clean_w15") else
  c(SEIR = "results/seir_clean_big", SIR = "results/sir_clean_big")
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
GENERATIONS <- c(6L, 12L, 20L)
CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

windows <- readRDS(file.path(SIM_DIR, "windows.rds"))
set.seed(11)
ids <- unlist(lapply(names(MODEL_DIRS), function(M) lapply(c("early", "middle", "late"), function(p) {
  x <- names(windows)[vapply(windows, function(w) w$model == M && w$position == p, logical(1))]
  sample(x, 20)
})))
tasks <- expand.grid(id = ids, generations = GENERATIONS, stringsAsFactors = FALSE)
fit_file <- function(id, g) file.path(OUT, "fits", sprintf("%s_g%02d.rds", id, g))

fit_smc <- function(w, g) {
  info <- INFO[[w$model]]; sp <- baseline_space(info); y <- w$y
  sim <- new_simulator(
    sample_parameters = function(n_sims) stop("Not used."),
    run = function(parameters, seed) {
      run_model(info, baseline_pars(info, unlist(parameters[sp$names]), parameters[info$known]), length(y), seed)$new
    }, type = info$model)
  bounds <- stats::setNames(lapply(seq_along(sp$names), function(j) c(sp$lower[j], sp$upper[j])), sp$names)
  cal <- make_calibrator("ABC-SMC", sim, bounds, info$known, n_generations = g, sim_budget = 200000L,
                         max_attempt_mult = 500L, distance = "summary")
  set.seed(2025L)
  t0 <- proc.time()[["elapsed"]]
  res <- do.call(calibrate, c(list(y, cal), as.list(w$known)))
  secs <- proc.time()[["elapsed"]] - t0
  post <- res$posterior
  draws <- post$particles[sample(nrow(post$particles), 500, replace = TRUE, prob = post$weights), sp$names, drop = FALSE]
  est <- calibration_estimate(res)[sp$names]
  out <- baseline_result(as.numeric(est), info, w$known, secs, res$diagnostics$n_model_runs, draws, "ABC-SMC-summary")
  out$draws <- NULL
  out$trace <- res$diagnostics$trace
  out
}

# LEFTOVERS=1 (once every pool job has ended): also fit windows whose lock was left by a stopped job.
LEFTOVERS <- identical(Sys.getenv("LEFTOVERS"), "1")
if (stage == "pool") {
  done <- parallel::mclapply(sample(nrow(tasks)), function(k) {
    id <- tasks$id[k]; g <- tasks$generations[k]; f <- fit_file(id, g)
    if (file.exists(f) || (!dir.create(paste0(f, ".lock"), showWarnings = FALSE) && !LEFTOVERS)) return(NULL)
    out <- tryCatch(fit_smc(windows[[id]], g), error = function(e) list(failed = TRUE, error = conditionMessage(e)))
    saveRDS(out, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f); unlink(paste0(f, ".lock"), recursive = TRUE)
    TRUE
  }, mc.cores = CORES, mc.preschedule = FALSE)
  cat(format(Sys.time()), "fits this job:", sum(vapply(done, isTRUE, logical(1))), "\n")
}

if (stage == "collect") {
  rows <- lapply(seq_len(nrow(tasks)), function(k) {
    id <- tasks$id[k]; g <- tasks$generations[k]; w <- windows[[id]]; f <- fit_file(id, g)
    base <- data.frame(model = w$model, id = id, position = w$position, length = w$length, generations = g, R0_true = w$R0)
    if (!file.exists(f)) return(cbind(base, done = FALSE))
    r <- readRDS(f)
    if (isTRUE(r$failed)) return(cbind(base, done = TRUE, failed = TRUE))
    tr <- r$trace
    cbind(base, done = TRUE, failed = FALSE, R0 = r$R0, R0_low = r$R0_low, R0_high = r$R0_high,
          state_error = 100 * mean(abs(r$state[names(w$state)] - w$state)), seconds = r$seconds, model_runs = r$model_runs,
          final_epsilon = tail(tr$next_epsilon, 1), first_epsilon = tr$next_epsilon[1])
  })
  cols <- unique(unlist(lapply(rows, names)))
  res <- do.call(rbind, lapply(rows, function(r) { for (c in setdiff(cols, names(r))) r[[c]] <- NA; r[cols] }))
  res$R0_error <- abs(res$R0 - res$R0_true)
  res$inside <- res$R0_true >= res$R0_low & res$R0_true <= res$R0_high
  res$width <- res$R0_high - res$R0_low
  write.csv(res, file.path(OUT, "results.csv"), row.names = FALSE)
  print(aggregate(cbind(R0_error, width, inside, state_error, model_runs, seconds) ~ model + generations, res, mean))
}
