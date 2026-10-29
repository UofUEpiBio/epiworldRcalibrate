# The real-data evaluation of the paper: five datasets whose counts are close to the daily new
# infections the models describe (symptom onsets or hospital admissions, not cases by report
# date), plus Utah COVID-19 cases by report date as a contrast. Every method (the BiLSTM and the
# four standard methods of methods_common.R) calibrates windows of 15, 30, 60, 90, 120 and 180 days (those that
# fit in the wave) at the beginning, the middle (centred on the peak of the 7-day average) and the end of each wave;
# its estimates are then run 500 times for the window and the 14 days after it, and the band is
# compared with the observed counts.
#
#   dataset                         what is counted                         source
#   Utah COVID-19 admissions        hospital admissions, by admission date   Utah DHHS
#   Utah influenza admissions       hospital admissions, by admission date   US HHS
#   Baltimore influenza 1918        cases by date of symptom onset           Frost & Sydenstricker (EpiEstim)
#   Hong Kong SARS 2003             cases by date of symptom onset           Cori et al. 2009 (EpiEstim)
#   Sierra Leone Ebola 2014-15      cases by date of symptom onset           outbreaks package (line list)
#   Utah COVID-19 cases (contrast)  cases by test report date                Utah DHHS
#
# Six more outbreaks, a separate check (extra = TRUE; not in the five-dataset averages), by date of symptom onset:
#   Canada (Toronto) SARS 2003, South Korea MERS 2015, Hagelloch measles 1861, Kikwit Ebola 1995 (outbreaks package);
#   Kosovo smallpox 1972, Pennsylvania school influenza 2009 (EpiEstim)
#
# Known values. SEIR: the latent period (incub) is the published mean incubation period; the
# infectious period is the mean serial interval minus the incubation period, but at least 4 days
# (recov <= 0.25, the trained range). SIR: the infectious period is the mean serial interval,
# within the trained 4 to 14 days. The population is n = 10,000 for every dataset: the counts are
# read as the complete incidence of an effective population of 10,000 (the reported fraction and
# the population size cannot be told apart from a curve).
#
# Run from examples/:
#   Rscript Real_data_five.R windows | methods <shard> <n_shards> | bilstm | curves <shard> <n_shards> | collect
args  <- commandArgs(TRUE)
stage <- args[1]
shard <- if (length(args) >= 3) as.integer(args[2]) else 1L
n_shards <- if (length(args) >= 3) as.integer(args[3]) else 1L

suppressMessages(pkgload::load_all("..", quiet = TRUE))
source("methods_common.R")
OUT <- "results/real_five"
DD <- "data"
MODEL_DIRS <- c(SEIR = "results/seir_clean_w15", SIR = "results/sir_clean_w15")   # networks for windows of 15-365 days
SEEDS <- c(122L, 1:4)
LENGTHS <- c(15L, 30L, 60L, 90L, 120L, 180L); POSITIONS <- c("beginning", "middle", "end"); H <- 14L; N_REPS <- 500L; N_POP <- 10000
METHODS4 <- c("ABC-MCMC-summary", "ABC-SMC-summary", "DE", "NelderMead",   # ABC on summary statistics
              "DE-random-seed")   # DE with a new seed for every model run (sensitivity analysis)
CORES <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
# DeepIMC on the raw counts and on smoothed counts (methods_common.R); every version is scored against the
# raw counts:
#   k20, k50   Gaussian kernel with a bandwidth of 20% or 50% of the window length
#   g2.5       Gaussian kernel with a standard deviation of 2.5 days, whatever the window length
#   med7       7-day running median, then the 2.5-day kernel
#   auto       med7 if the window is noisier than the agent-based model (dispersion index above 2), else raw
#   lowess20, lowess50     local-linear smoothing (tricube kernel) with a span of 20% or 50% of the window
#   rlowess20, rlowess50   the same, robust to outlying days
#   box7       centred 7-day moving average (box kernel)
#   epan       Epanechnikov kernel with the standard deviation of the 2.5-day Gaussian kernel
SMOOTH <- list(BiLSTM = identity, `BiLSTM-k20` = function(y) smooth_counts(y, 0.2), `BiLSTM-k50` = function(y) smooth_counts(y, 0.5),
               `BiLSTM-g2.5` = function(y) smooth_days(y, 2.5), `BiLSTM-med7` = smooth_median,
               `BiLSTM-auto` = function(y) { d <- dispersion_index(y); if (!is.na(d) && d > 2) smooth_median(y) else y },
               `BiLSTM-lowess20` = function(y) smooth_lowess(y, 0.2), `BiLSTM-lowess50` = function(y) smooth_lowess(y, 0.5),
               `BiLSTM-rlowess20` = function(y) smooth_lowess(y, 0.2, robust = TRUE),
               `BiLSTM-rlowess50` = function(y) smooth_lowess(y, 0.5, robust = TRUE),
               `BiLSTM-box7` = smooth_box7, `BiLSTM-epan` = smooth_epanechnikov)
# DeepIMC (raw counts) with its draws weighted by how well their simulated curves match the window.
# Every version (raw and smoothed counts) is also weighted, always against the raw observed counts:
# "BiLSTM-weighted", "BiLSTM-k20-weighted", ...
WEIGHTED <- c("BiLSTM-weighted", paste0(setdiff(names(SMOOTH), "BiLSTM"), "-weighted"))
WEIGHTING <- Sys.getenv("WEIGHTING", "best50")
base_of <- function(m) if (m == "BiLSTM-weighted") "BiLSTM" else sub("-weighted$", "", m)
bil_file <- function(M, v) file.path(OUT, sprintf("bilstm_%s%s.rds", M, gsub("-", "_", sub("^BiLSTM", "", v))))
INFO <- lapply(setNames(names(MODEL_DIRS), names(MODEL_DIRS)), model_info)
run_dir <- file.path(OUT, "method_runs"); fit_dir <- file.path(OUT, "curves")
for (d in c(OUT, run_dir, fit_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
unit_file <- function(id, m) file.path(run_dir, sprintf("%s_%s.rds", id, m))
curve_file <- function(id, m) file.path(fit_dir, sprintf("%s_%s.rds", id, m))
my_shard <- function(n) which((seq_len(n) - 1L) %% n_shards == shard - 1L)
win_file <- file.path(OUT, "windows.rds")

# Published natural history (mean incubation period and serial interval, days).
DISEASES <- list(
  `COVID-19`  = list(incubation = 5.2, serial = 7.5),    # Li et al. 2020, NEJM
  influenza   = list(incubation = 1.4, serial = 2.6),    # Lessler et al. 2009; Cori et al. 2013 (Baltimore 1918)
  SARS        = list(incubation = 6.4, serial = 8.4),    # Donnelly et al. 2003; Lipsitch et al. 2003
  Ebola       = list(incubation = 11.4, serial = 15.3),  # WHO Ebola Response Team 2014
  MERS        = list(incubation = 6.7, serial = 12.6),   # Cowling et al. 2015 (South Korea)
  smallpox    = list(incubation = 12, serial = 22.4),    # Fenner et al. 1988; Cori et al. 2013 (Kosovo 1972)
  measles     = list(incubation = 12.5, serial = 14.9))  # Lessler et al. 2009; Cori et al. 2013 (Hagelloch 1861)
known_values <- function(disease, M) {
  d <- DISEASES[[disease]]
  if (M == "SEIR") list(n = N_POP, recov = 1 / max(4, d$serial - d$incubation), incub = d$incubation)
  else list(n = N_POP, recov = 1 / min(14, max(4, d$serial)))
}

# The part of a season with the peak: between the last week without cases before the peak
# and the first one after it.
season_range <- function(x, y, from, to) {
  s <- which(x >= as.Date(from) & x <= as.Date(to)); z <- y[s]
  ma <- as.numeric(stats::filter(z, rep(1 / 7, 7), sides = 2)); pk <- which.max(ma)
  zero_week <- vapply(seq_along(z), function(i) i >= 7 && all(z[(i - 6):i] == 0), logical(1))
  first <- max(c(0, which(zero_week & seq_along(z) < pk))) + 1
  after <- which(zero_week & seq_along(z) > pk)
  last <- if (length(after)) after[1] - 7 else length(z)
  x[s][c(first, last)]
}

waves <- function() {
  adm <- read.csv(file.path(DD, "utah_covid_admissions_dhhs.csv")); adm$date <- as.Date(adm$date)
  cas <- read.csv(file.path(DD, "utah_covid_all_days.csv")); cas$date <- as.Date(cas$Date)
  hhs <- read.csv(file.path(DD, "utah_hhs_hospital_admissions.csv")); hhs$date <- as.Date(substr(hhs$date, 1, 10)) - 1
  flu <- hhs$previous_day_admission_influenza_confirmed
  e1918 <- new.env(); load(file.path(DD, "Flu1918.rda"), envir = e1918)
  sars <- read.csv(file.path(DD, "sars_hongkong_2003.csv"))
  ebo <- read.csv(file.path(DD, "ebola_sierraleone_2014.csv")); ebo$date <- as.Date(ebo$date)
  COVID_WAVES <- list(`fall-winter 2020` = c("2020-09-01", "2021-02-28"), `Delta 2021` = c("2021-06-15", "2021-11-30"),
                      `Omicron 2021-22` = c("2021-12-01", "2022-03-31"))
  wave <- function(dataset, measure, disease, name, x, y, from = NA, to = NA, main = TRUE, extra = FALSE) {
    idx <- if (is.na(from)) seq_along(x) else which(x >= as.Date(from) & x <= as.Date(to))
    list(dataset = dataset, measure = measure, disease = disease, wave = name, main = main, extra = extra, x = x, y = as.numeric(y), idx = idx)
  }
  # Daily counts from dates (every day from the first to the last).
  daily <- function(dates) { dates <- dates[!is.na(dates)]; d <- seq(min(dates), max(dates), by = "day")
                             list(x = d, y = as.numeric(table(factor(as.character(dates), levels = as.character(d))))) }
  out <- list()
  for (w in names(COVID_WAVES)) {
    out[[length(out) + 1]] <- wave("Utah COVID-19 admissions", "hospital admissions (admission date)", "COVID-19", w,
                                   adm$date, adm$admissions, COVID_WAVES[[w]][1], COVID_WAVES[[w]][2])
    # Reported cases are far more than 10,000 people can produce, so they are used as cases per
    # 10,000 residents (Utah, 2020 census: 3,271,616), as for the US states before.
    out[[length(out) + 1]] <- wave("Utah COVID-19 cases", "cases (test report date)", "COVID-19", w,
                                   cas$date, round(cas$Daily.Cases * 10000 / 3271616), COVID_WAVES[[w]][1],
                                   COVID_WAVES[[w]][2], main = FALSE)
  }
  for (s in list(c("2022-23", "2022-09-01", "2023-05-31"), c("2023-24", "2023-09-01", "2024-04-26"))) {
    r <- season_range(hhs$date, flu, s[2], s[3])
    out[[length(out) + 1]] <- wave("Utah influenza admissions", "hospital admissions (admission date)", "influenza",
                                   s[1], hhs$date, flu, r[1], r[2])
  }
  out[[length(out) + 1]] <- wave("Baltimore influenza 1918", "cases (symptom onset)", "influenza", "1918",
                                 seq_along(e1918$Flu1918$incidence), e1918$Flu1918$incidence)
  out[[length(out) + 1]] <- wave("Hong Kong SARS 2003", "cases (symptom onset)", "SARS", "2003", sars$day, sars$cases)
  out[[length(out) + 1]] <- wave("Sierra Leone Ebola 2014-15", "cases (symptom onset)", "Ebola", "2014-15", ebo$date, ebo$cases)
  # Additional outbreaks (a separate group, not in the five-dataset averages): another SARS outbreak, outbreaks
  # driven by superspreading (MERS, smallpox) and smooth classic outbreaks (influenza in a school, measles).
  sc <- outbreaks::sars_canada_2003
  out[[length(out) + 1]] <- wave("Toronto SARS 2003", "cases (symptom onset)", "SARS", "2003", sc$date,
                                 sc$cases_travel + sc$cases_household + sc$cases_healthcare + sc$cases_other, main = FALSE, extra = TRUE)
  mk <- daily(outbreaks::mers_korea_2015$linelist$dt_onset)
  out[[length(out) + 1]] <- wave("South Korea MERS 2015", "cases (symptom onset)", "MERS", "2015", mk$x, mk$y, main = FALSE, extra = TRUE)
  e <- new.env(); utils::data("Smallpox1972", package = "EpiEstim", envir = e)
  out[[length(out) + 1]] <- wave("Kosovo smallpox 1972", "cases (symptom onset)", "smallpox", "1972", seq_along(e$Smallpox1972$incidence),
                                 e$Smallpox1972$incidence, main = FALSE, extra = TRUE)
  utils::data("Flu2009", package = "EpiEstim", envir = e)
  out[[length(out) + 1]] <- wave("Pennsylvania school influenza 2009", "cases (symptom onset)", "influenza", "2009",
                                 e$Flu2009$incidence$dates, e$Flu2009$incidence$I, main = FALSE, extra = TRUE)
  mh <- daily(outbreaks::measles_hagelloch_1861$date_of_prodrome)
  out[[length(out) + 1]] <- wave("Hagelloch measles 1861", "cases (prodrome onset)", "measles", "1861", mh$x, mh$y, main = FALSE, extra = TRUE)
  ek <- outbreaks::ebola_kikwit_1995
  out[[length(out) + 1]] <- wave("Kikwit Ebola 1995", "cases (symptom onset)", "Ebola", "1995", ek$date, ek$onset, main = FALSE, extra = TRUE)
  out
}

# The BiLSTM fits of both models, named by window (older files were saved without names, in the
# order of the windows of each model).
load_bilstm <- function(windows, v = "BiLSTM") {
  out <- list()
  for (M in names(MODEL_DIRS)) {
    if (!file.exists(bil_file(M, v))) next
    b <- readRDS(bil_file(M, v))
    if (is.null(names(b))) names(b) <- names(windows)[vapply(windows, function(w) w$model == M, logical(1))][seq_along(b)]
    out <- c(out, b)
  }
  out
}

if (stage == "windows") {
  # Windows already in windows.rds keep their ids (and saved fits); new lengths are added after them.
  windows <- if (file.exists(win_file)) readRDS(win_file) else list()
  key <- function(w) paste(w$model, w$dataset, w$wave, w$length, w$position)
  have <- vapply(windows, key, "")
  for (v in waves()) {
    wy <- v$y[v$idx]
    pk <- which.max(as.numeric(stats::filter(wy, rep(1 / 7, 7), sides = 2)))
    for (L in LENGTHS) {
      if (L > length(v$idx)) next
      for (pos in POSITIONS) {
        s0 <- switch(pos, beginning = 1, end = length(wy) - L + 1, middle = min(max(1, pk - floor(L / 2)), length(wy) - L + 1))
        at <- v$idx[s0:(s0 + L - 1)]
        if (sum(v$y[at]) > N_POP) next   # more cases than the effective population could produce
        fut <- at[L] + seq_len(H); fut <- fut[fut <= length(v$y)]
        for (M in names(MODEL_DIRS)) {
          if (paste(M, v$dataset, v$wave, L, pos) %in% have) next
          id <- sprintf("%s_r%03d", M, sum(vapply(windows, function(w) w$model == M, logical(1))) + 1)
          windows[[id]] <- list(id = id, model = M, dataset = v$dataset, measure = v$measure, disease = v$disease,
                                wave = v$wave, main = v$main, extra = isTRUE(v$extra), length = L, position = pos, time = v$x[at],
                                y = v$y[at], future = v$y[fut], known = known_values(v$disease, M))
        }
      }
    }
  }
  saveRDS(windows, win_file)
  w <- do.call(rbind, lapply(windows, function(x) data.frame(model = x$model, dataset = x$dataset, wave = x$wave,
                                                             total = sum(x$y), max = max(x$y))))
  print(aggregate(cbind(windows = 1, max_window_total = total) ~ model + dataset, w,
                  function(z) if (length(z) && all(z == 1)) sum(z) else max(z)))
  print(t(sapply(names(DISEASES), function(d) c(SEIR = unlist(known_values(d, "SEIR")), SIR = unlist(known_values(d, "SIR"))))))
}

if (stage == "methods") {
  windows <- readRDS(win_file)
  units <- expand.grid(id = names(windows), method = METHODS4, stringsAsFactors = FALSE)
  units <- units[my_shard(nrow(units)), ]
  units <- units[!file.exists(unit_file(units$id, units$method)), ]
  units <- units[order(-vapply(units$id, function(i) windows[[i]]$length, 0)), ]
  cat(format(Sys.time()), "shard", shard, "of", n_shards, ":", nrow(units), "fits to do on", CORES, "cores\n")
  done <- parallel::mclapply(seq_len(nrow(units)), function(k) try({
    w <- windows[[units$id[k]]]
    saveRDS(fit_baseline(units$method[k], w$y, INFO[[w$model]], w$known), unit_file(w$id, units$method[k]))
  }), mc.cores = CORES, mc.preschedule = FALSE)
  cat(format(Sys.time()), "errors:", sum(vapply(done, inherits, logical(1), what = "try-error")), "\n")
}

if (stage == "bilstm") {
  windows <- readRDS(win_file)
  set.seed(1)
  for (M in names(MODEL_DIRS)) {
    f <- NULL
    for (v in names(SMOOTH)) {
      ws <- windows[vapply(windows, function(w) w$model == M, logical(1))]
      f_out <- bil_file(M, v)
      # Fits already saved are kept; only windows added later are calibrated.
      old <- if (file.exists(f_out)) { b <- readRDS(f_out); if (is.null(names(b))) names(b) <- names(ws)[seq_along(b)]; b } else list()
      ws <- ws[setdiff(names(ws), names(old))]
      if (!length(ws)) next
      if (is.null(f)) f <- make_bilstm(INFO[[M]], MODEL_DIRS[[M]], SEEDS, device = Sys.getenv("BILSTM_DEVICE", "cpu"))
      y_in <- lapply(ws, function(w) SMOOTH[[v]](w$y))
      new <- setNames(f(y_in, do.call(rbind, lapply(ws, function(w) as.data.frame(w$known)))), names(ws))
      saveRDS(c(old, new), f_out)
    }
  }
}

curve_scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  m <- b$median
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper),
    interval_score = mean((b$upper - b$lower) + 40 * pmax(b$lower - y, 0) + 40 * pmax(y - b$upper, 0)),
    width = mean(b$upper - b$lower), MAE = mean(abs(m - y)),
    NRMSE = if (mean(y) > 0) sqrt(mean((m - y)^2)) / mean(y) else NA,
    total_error = if (sum(y) > 0) abs(sum(m) - sum(y)) / sum(y) else NA, WIS = wis_of(b, y))
}
# Scores of the band of cumulative counts since the first day of the window; rel_MAE is the MAE divided
# by the observed cumulative count on the last scored day.
cum_scores <- function(b, y, idx) {
  b <- b[idx, ]; y <- y[seq_along(idx)]
  c(coverage = 100 * mean(y >= b$lower & y <= b$upper), MAE = mean(abs(b$median - y)),
    rel_MAE = if (y[length(y)] > 0) mean(abs(b$median - y)) / y[length(y)] else NA, WIS = wis_of(b, y))
}

if (stage == "curves") {
  windows <- readRDS(win_file)
  bils <- lapply(setNames(names(SMOOTH), names(SMOOTH)), function(v) load_bilstm(windows, v))
  jobs <- expand.grid(id = names(windows), method = c(names(SMOOTH), WEIGHTED, METHODS4), stringsAsFactors = FALSE)
  jobs <- jobs[my_shard(nrow(jobs)), ]
  is_bil <- jobs$method %in% c(names(SMOOTH), WEIGHTED)
  have_bil <- mapply(function(id, m) id %in% names(bils[[if (m %in% WEIGHTED) base_of(m) else m]]), jobs$id, ifelse(is_bil, jobs$method, "BiLSTM"))
  jobs <- jobs[!file.exists(curve_file(jobs$id, jobs$method)) &
                 ifelse(is_bil, have_bil, file.exists(unit_file(jobs$id, jobs$method))), ]
  cat(format(Sys.time()), "shard", shard, "of", n_shards, ":", nrow(jobs), "bands to do on", CORES, "cores\n")
  done <- parallel::mclapply(seq_len(nrow(jobs)), function(k) try({
    w <- windows[[jobs$id[k]]]; m <- jobs$method[k]
    set.seed(k)
    f <- if (m %in% WEIGHTED) bils[[base_of(m)]][[w$id]] else if (m %in% names(SMOOTH)) bils[[m]][[w$id]] else readRDS(unit_file(w$id, m))
    weighted <- NULL
    if (m %in% WEIGHTED) { weighted <- weight_draws(INFO[[w$model]], f$draws, w$y, WEIGHTING,
                                                    seed = 1000L * match(w$id, names(windows)) + match(m, WEIGHTED))
                         f$draws <- weighted$draws; weighted$draws <- NULL }
    if (isTRUE(f$failed)) { saveRDS(list(failed = TRUE), curve_file(w$id, m)); return(NULL) }
    nf <- length(w$future); L <- w$length
    b <- band_of(INFO[[w$model]], f$draws, L + nf, n = N_REPS)
    saveRDS(list(window = curve_scores(b$new, w$y, seq_len(L)),
                 future = if (nf) curve_scores(b$new, w$future, L + seq_len(nf)) else NULL,
                 cumulative = cum_scores(b$cumulative, cumsum(w$y), seq_len(L)),
                 future_cumulative = if (nf) cum_scores(b$cumulative, sum(w$y) + cumsum(w$future), L + seq_len(nf)) else NULL,
                 band = round(b$new, 2), band_cumulative = round(b$cumulative, 2), weighted = weighted), curve_file(w$id, m))
  }), mc.cores = CORES, mc.preschedule = FALSE)
  cat(format(Sys.time()), "errors:", sum(vapply(done, inherits, logical(1), what = "try-error")), "\n")
}

if (stage == "collect") {
  windows <- readRDS(win_file)
  bils <- lapply(setNames(names(SMOOTH), names(SMOOTH)), function(v) load_bilstm(windows, v))
  rows <- list()
  for (id in names(windows)) for (m in c(names(SMOOTH), WEIGHTED, METHODS4)) {
    w <- windows[[id]]
    f <- if (m %in% WEIGHTED) bils[[base_of(m)]][[id]] else if (m %in% names(SMOOTH)) bils[[m]][[id]] else
      if (file.exists(unit_file(id, m))) readRDS(unit_file(id, m)) else NULL
    s <- if (file.exists(curve_file(id, m))) readRDS(curve_file(id, m)) else NULL
    base <- data.frame(model = w$model, id = id, dataset = w$dataset, measure = w$measure, disease = w$disease,
                       wave = w$wave, main = w$main, extra = isTRUE(w$extra), length = w$length, position = w$position, method = m,
                       recov = w$known$recov)
    if (is.null(f) || isTRUE(f$failed) || is.null(s)) {
      rows[[length(rows) + 1]] <- cbind(base, failed = isTRUE(f$failed), done = !is.null(s)); next
    }
    ptran <- if (m %in% c(names(SMOOTH), WEIGHTED)) f$draws[[1]]$ptran else f$R0 * w$known$recov / f$draws[[1]]$crate
    part <- function(x, prefix) if (is.null(x)) NULL else as.data.frame(as.list(setNames(x, paste0(prefix, "_", names(x)))))
    rows[[length(rows) + 1]] <- do.call(cbind, c(list(base, data.frame(failed = FALSE, done = TRUE,
      R0 = f$R0, R0_low = f$R0_low, R0_high = f$R0_high, ptran = ptran, crate = f$R0 * w$known$recov / ptran)),
      Filter(Negate(is.null), list(part(s$window, "window"), part(s$future, "future"), part(s$cumulative, "cumulative"),
                                   part(s$future_cumulative, "future_cumulative"))),
      list(data.frame(seconds = f$seconds, model_runs = f$model_runs, fit_best = if (is.null(s$weighted)) NA else s$weighted$fit_best))))
  }
  res <- as.data.frame(data.table::rbindlist(rows, fill = TRUE))
  write.csv(res, file.path(OUT, "results_by_window.csv"), row.names = FALSE)
  cat("rows", nrow(res), "; done", sum(res$done), "; failed", sum(res$failed), "\n")
}

