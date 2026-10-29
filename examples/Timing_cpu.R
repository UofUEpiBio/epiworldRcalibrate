# CPU-only timing for the paper: every method on the same node, one CPU core each (no GPU).
#   inference  calibrating one window (the BiLSTM: first call, one window at a time, and many
#              windows in one pass; the four standard methods: one fit per window), and turning a
#              fit into a 95% band (500 model runs, or the renewal equation)
#   training   the one-time cost of the BiLSTM on CPU: simulating training epidemics on one core,
#              and one training epoch on a 100,000-epidemic subset (all cores of the node), scaled
#              to the 1,000,000 epidemics and 50 epochs of the shipped models
# 20 windows (10 per model, lengths spread over the test's range) from the simulated test set.
# Run from examples/:  Rscript Timing_cpu.R inference | training
# With SIMTEST=w15: the networks for 15-365 days and windows of the 15-365-day test -> results/timing_cpu_w15
# (first_call times the shipped models, which are these networks). Without it: the earlier 30-180-day test.
part <- commandArgs(TRUE)[1]
# One thread per process for inference; all cores of the job for the training epoch.
Sys.setenv(OMP_NUM_THREADS = if (part == "training") Sys.getenv("SLURM_CPUS_PER_TASK", "1") else "1")
suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
W15 <- identical(Sys.getenv("SIMTEST"), "w15")
SIM_DIR <- if (W15) "results/simulated_test_15_365" else "results/simulated_test_10k"
OUT <- if (W15) "results/timing_cpu_w15" else "results/timing_cpu"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
MODEL_DIRS <- if (W15) c(SEIR = "results/seir_clean_w15", SIR = "results/sir_clean_w15") else
  c(SEIR = "results/seir_clean_big", SIR = "results/sir_clean_big")
SEEDS <- c(122L, 1:4)
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
METHODS4 <- c("ABC-MCMC-summary", "ABC-SMC-summary", "DE", "NelderMead")   # ABC on summary statistics
cpu <- sub("^Model name:\\s*", "", grep("^Model name", system("lscpu", intern = TRUE), value = TRUE)[1])
elapsed <- function(expr) { t0 <- proc.time()[["elapsed"]]; force(expr); proc.time()[["elapsed"]] - t0 }

if (part == "inference") {
  all_w <- readRDS(file.path(SIM_DIR, "windows.rds"))
  set.seed(5)
  pick <- unlist(lapply(names(MODEL_DIRS), function(M) {
    ids <- names(all_w)[vapply(all_w, function(w) w$model == M, logical(1))]
    L <- vapply(all_w[ids], `[[`, 0, "length")
    ids[order(L)][round(seq(0.05, 0.95, length.out = 10) * length(ids))]
  }))
  windows <- all_w[pick]
  known_df <- function(ws) do.call(rbind, lapply(ws, function(w) as.data.frame(w$known)))
  rows <- list()

  # The BiLSTM, in this process, on one thread.
  reticulate::py_run_string("import torch; torch.set_num_threads(1); torch.set_num_interop_threads(1)")
  for (M in names(MODEL_DIRS)) {
    ws <- windows[vapply(windows, function(w) w$model == M, logical(1))]
    t_first <- elapsed({ f <- make_bilstm(INFO[[M]], MODEL_DIRS[[M]], SEEDS, device = "cpu"); f(list(ws[[1]]$y), known_df(ws[1])) })
    one <- vapply(seq_along(ws), function(i) elapsed(f(list(ws[[i]]$y), known_df(ws[i]))), 0)
    batch <- elapsed(f(lapply(ws, `[[`, "y"), known_df(ws)))
    rows[[length(rows) + 1]] <- data.frame(model = M, method = "BiLSTM", what = c("first call (loads Python and networks)",
      "one window", sprintf("%d windows in one pass, per window", length(ws))), seconds = c(t_first, mean(one), batch / length(ws)))
    # Turning a fit into a band: 500 model runs, or the renewal equation.
    fits <- f(lapply(ws, `[[`, "y"), known_df(ws))
    band_abm <- vapply(seq_along(ws), function(i) elapsed(band_of(INFO[[M]], fits[[i]]$draws, ws[[i]]$length + 14, n = 500)), 0)
    band_ren <- vapply(seq_along(ws), function(i) elapsed(renewal_band(INFO[[M]], fits[[i]]$draws, ws[[i]]$length + 14, n = 500)), 0)
    rows[[length(rows) + 1]] <- data.frame(model = M, method = c("band: 500 model runs", "band: renewal equation"),
                                           what = "one window", seconds = c(mean(band_abm), mean(band_ren)))
  }

  # The four standard methods: one process each, one core each, the same windows.
  res <- parallel::mclapply(METHODS4, function(m) {
    do.call(rbind, lapply(windows, function(w) {
      t <- elapsed(fit <- fit_baseline(m, w$y, INFO[[w$model]], w$known))
      data.frame(model = w$model, method = m, id = w$id, length = w$length, seconds = t,
                 model_runs = if (isTRUE(fit$failed)) NA else fit$model_runs, failed = isTRUE(fit$failed))
    }))
  }, mc.cores = length(METHODS4), mc.preschedule = FALSE)
  base <- do.call(rbind, res)
  write.csv(base, file.path(OUT, "standard_methods_by_window.csv"), row.names = FALSE)
  rows[[length(rows) + 1]] <- aggregate(seconds ~ model + method, base, mean)
  rows[[length(rows)]]$what <- "one window"
  out <- do.call(rbind, lapply(rows, function(r) r[, c("model", "method", "what", "seconds")]))
  out$cpu <- cpu
  write.csv(out, file.path(OUT, "inference_seconds.csv"), row.names = FALSE)
  print(out)
}

# The BiLSTM's first call as a user makes it, in a fresh R session (one model per session): the
# package's window_calibrator() with its saved calibration windows, then calibrate() on one window.
# (make_bilstm() above also predicts the 100,000 calibration windows, a one-time cost of training.)
#   Rscript Timing_cpu.R first_call SEIR | SIR
if (part == "first_call") {
  M <- commandArgs(TRUE)[2]
  all_w <- readRDS(file.path(SIM_DIR, "windows.rds"))
  ids <- names(all_w)[vapply(all_w, function(w) w$model == M, logical(1))]
  w <- all_w[[ids[order(vapply(all_w[ids], `[[`, 0, "length"))][round(0.05 * length(ids))]]]
  t_first <- elapsed({ cal <- window_calibrator(M, device = "cpu")   # the shipped model (the same networks)
                       fit <- do.call(calibrate, c(list(w$y, cal), w$known)) })
  t_next <- elapsed(do.call(calibrate, c(list(w$y, cal), w$known)))
  out <- data.frame(model = M, method = "BiLSTM", what = c("first call (loads Python and networks)", "next call, one window"),
                    seconds = c(t_first, t_next), cpu = cpu)
  f <- file.path(OUT, "first_call_seconds.csv")
  write.csv(rbind(if (file.exists(f)) subset(read.csv(f), model != M), out), f, row.names = FALSE)
  print(out)
}

if (part == "training") {
  out <- list()
  for (M in names(MODEL_DIRS)) {
    m <- tolower(M)
    # Simulating training epidemics on one core.
    n_sim <- 500L
    t_sim <- elapsed(simulate_window_data(M, n_sims = n_sim, output_dir = file.path(tempdir(), paste0("sim_", m)), cores = 1L))
    # One epoch on the 100,000-epidemic tuning subset with the shipped settings, on CPU.
    ref <- jsonlite::read_json(file.path(MODEL_DIRS[[M]], "model_seed_122", "model_config.json"))
    sub <- sprintf("/scratch/general/vast/u1418987/epiworld_banks/%s_clean_tune100k", m)
    states <- unlist(ref$state_targets)
    n_rows <- nrow(data.table::fread(file.path(sub, paste0(m, "_theta.csv")), select = 1L))
    cat("TRAINING", M, "\n")   # the epoch line printed after this marker has the epoch's own seconds
    t_epoch <- elapsed(train_bilstm_calibrator(
      name = m, known_names = unlist(ref$known_names), target_names = c("ptran", "R0"), type = M,
      parameter_file = file.path(sub, paste0(m, "_theta.csv")), observation_file = file.path(sub, paste0(m, "_observations.csv")),
      output_dir = file.path(tempdir(), paste0("train_", m)), epochs = 1, batch_size = ref$batch_size,
      learning_rate = ref$learning_rate, seed = 122, hidden_size = ref$hidden_size, num_layers = ref$num_layers,
      dropout = ref$dropout, physics_weight = 0, incidence_max = ref$incidence_max, incidence_transform = ref$incidence_transform,
      output_activation = unlist(ref$output_activations), window_min = ref$window_min, window_max = ref$window_max,
      window_stop_zeros = ref$window_stop_zeros, state_files = setNames(file.path(sub, paste0(m, "_", states, ".csv")), states),
      grad_clip = ref$grad_clip, lr_schedule = ref$lr_schedule, device = "cpu"))
    hist <- read.csv(file.path(tempdir(), paste0("train_", m), "training_history.csv"))
    out[[M]] <- data.frame(model = M, cpu = cpu, cores_for_training = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")),
                           simulate_seconds_per_epidemic = t_sim / n_sim,
                           simulate_cpu_hours_1m = t_sim / n_sim * 1e6 / 3600,
                           epoch_seconds_subset = t_epoch, subset_epidemics = n_rows,
                           training_hours_1m_50_epochs = t_epoch * (1e6 / n_rows) * ref$epochs / 3600,
                           epochs = ref$epochs)
    print(out[[M]])
  }
  write.csv(do.call(rbind, out), file.path(OUT, "training_cpu.csv"), row.names = FALSE)
}
