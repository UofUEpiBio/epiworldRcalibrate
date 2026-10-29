# LaTeX tables of the paper, written from the saved results (so that no number is typed by hand):
#   tables/sim_parameters.tex   parameter recovery on the 10,000 simulated test windows (15-365 days)
#   tables/sim_paired.tex       paired R0 differences, standard method minus BiLSTM
#   tables/sim_curves.tex       recovery of the epidemic curves (coverage, MAE and WIS)
#   tables/sim_cumulative.tex   recovery of the cumulative incidence (evaluation only)
#   tables/sim_start.tex        DeepIMC's starting state or one fitted by the renewal equation
#   tables/real_results.tex     the five real datasets, in the window (coverage, MAE and WIS)
#   tables/real_future.tex      the five real datasets, in the 14 days after the window
#   tables/real_cumulative.tex  the five real datasets, cumulative incidence
#   tables/real_extra.tex       six more outbreaks (and Hong Kong SARS): coverage and WIS
#   tables/real_extra_check.tex their noise and fit check; tables/hk_jump.tex Hong Kong around its one-day jump
#   tables/de_typical.tex       DE with a fixed seed or a new seed for every model run
#   tables/abc_sensitivity.tex  ABC on the whole curve or on summary statistics
#   tables/versions.tex         versions of the networks
#   tables/smc_generations.tex  ABC-SMC with 6, 12 or 20 generations
#   tables/renewal.tex          curves from 500 model runs or from the renewal equation
#   tables/timing.tex           CPU-only computing time (inference and one-time training)
# Run from the paper/ folder:  Rscript tables/make_tables.R
EX <- "../examples/results"
# ABC-MCMC and ABC-SMC are the summary-statistics versions (named simply ABC-MCMC and ABC-SMC in the paper).
METHODS <- c("BiLSTM", "ABC-MCMC-summary", "ABC-SMC-summary", "DE", "NelderMead")
LAB <- c(BiLSTM = "DeepIMC", `ABC-MCMC-summary` = "ABC-MCMC", `ABC-SMC-summary` = "ABC-SMC", DE = "DE", NelderMead = "Nelder--Mead",
         `BiLSTM-k20` = "DeepIMC, 20\\%", `BiLSTM-k50` = "DeepIMC, 50\\%", `BiLSTM-renewal-start` = "DeepIMC, renewal start",
         `DE-random-seed` = "DE, new seed per run", `BiLSTM-auto` = "DeepIMC, automatic",
         `BiLSTM-g2.5` = "DeepIMC, 2.5-day kernel", `BiLSTM-med7` = "DeepIMC, median + kernel",
         `BiLSTM-weighted` = "DeepIMC, weighted", `BiLSTM-k20-weighted` = "DeepIMC, 20\\%, weighted",
         `BiLSTM-k50-weighted` = "DeepIMC, 50\\%, weighted")
# On real data, DeepIMC also reads counts smoothed with a kernel of 20% or 50% of the window length.
REAL_METHODS <- c("BiLSTM", "BiLSTM-k20", "BiLSTM-k50", "BiLSTM-weighted", "BiLSTM-k20-weighted", "BiLSTM-k50-weighted", METHODS[-1])
# Simulated curve tables: DeepIMC and DeepIMC weighted by the data (same R0, state and curves of the chosen draws).
SIM_METHODS <- c("BiLSTM", "BiLSTM-weighted", METHODS[-1])
CRATE <- list(SEIR = c(5, 15), SIR = c(1, 5))
f <- function(x, d = 2) ifelse(is.na(x), "--", formatC(x, digits = d, format = "f", big.mark = ","))
bold_min <- function(v, d) { s <- f(v, d); i <- which.min(v); if (length(i)) s[i] <- paste0("\\textbf{", s[i], "}"); s }
bold_near <- function(v, d, target = 95) { s <- f(v, d); i <- which.min(abs(v - target)); if (length(i)) s[i] <- paste0("\\textbf{", s[i], "}"); s }
write_tab <- function(lines, file) { writeLines(lines, file.path("tables", file)); cat("wrote", file, "\n") }
row <- function(...) paste0(paste(c(...), collapse = " & "), " \\\\")

SIM_DIR <- file.path(EX, "simulated_test_15_365")
SIM_FILE <- file.path(SIM_DIR, "results_by_window.csv")
if (file.exists(SIM_FILE)) {
sim <- read.csv(SIM_FILE)
n_fail <- aggregate(failed ~ model + method, sim, sum)
sim <- sim[sim$done & !sim$failed, ]
# The best guesses for ptran and crate when R0 is known exactly but the split is not.
lo <- ifelse(sim$model == "SEIR", 5, 1); hi <- ifelse(sim$model == "SEIR", 15, 5)
sim$ptran_oracle_error <- abs(sim$R0_true * sim$recov * log(hi / lo) / (hi - lo) - sim$ptran_true)
sim$crate_oracle_error <- abs((lo + hi) / 2 - sim$crate_true)

# ---- Parameter recovery -----------------------------------------------------------------------
L <- c("\\begin{tabular}{llrrrrrrrrr}", "\\toprule",
       row("", "", "\\multicolumn{4}{c}{$R_0$ MAE}", "\\multicolumn{2}{c}{95\\% range of $R_0$}", "$p_{tran}$", "$c_{rate}$", "State"),
       "\\cmidrule(lr){3-6}\\cmidrule(lr){7-8}",
       row("Model", "Method", "all", "early", "middle", "late", "has true (\\%)", "width", "MAE", "MAE", "error"), "\\midrule")
for (M in c("SEIR", "SIR")) {
  d <- sim[sim$model == M, ]
  mae <- function(m, col, pos = NULL) { x <- d[d$method == m & (is.null(pos) | d$position %in% pos), col]; mean(x, na.rm = TRUE) }
  SM <- SIM_METHODS[SIM_METHODS %in% d$method]
  stats <- sapply(SM, function(m) c(all = mae(m, "R0_error"), early = mae(m, "R0_error", "early"),
    middle = mae(m, "R0_error", "middle"), late = mae(m, "R0_error", "late"),
    inside = 100 * mean(d$inside[d$method == m], na.rm = TRUE), width = mean((d$R0_high - d$R0_low)[d$method == m], na.rm = TRUE),
    ptran = mae(m, "ptran_error"),
    crate = mae(m, "crate_error"), state = mae(m, "state_error")))
  cols <- list(all = bold_min(stats["all", ], 2), early = bold_min(stats["early", ], 2), middle = bold_min(stats["middle", ], 2),
               late = bold_min(stats["late", ], 2), inside = f(stats["inside", ], 1), width = f(stats["width", ], 2),
               ptran = f(stats["ptran", ], 4),
               crate = f(stats["crate", ], 2), state = bold_min(stats["state", ], 1))
  for (k in seq_along(SM)) L <- c(L, row(if (k == 1) M else "", LAB[[SM[k]]], sapply(cols, `[`, k)))
  L <- c(L, row("", "\\textit{guess knowing $R_0$}", "", "", "", "", "", "",
                f(mean(d$ptran_oracle_error[d$method == "BiLSTM"]), 4), f(mean(d$crate_oracle_error[d$method == "BiLSTM"]), 2), ""))
  if (M == "SEIR") L <- c(L, "\\midrule")
}
write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "sim_parameters.tex")

# ---- Paired R0 differences (standard method minus BiLSTM, bootstrap over test epidemics) -------
set.seed(3)
L <- c("\\begin{tabular}{llllll}", "\\toprule", row("Model", "Method", "All windows", "Early", "Middle", "Late"), "\\midrule")
for (M in c("SEIR", "SIR")) {
  d <- sim[sim$model == M, ]; b <- d[d$method == "BiLSTM", c("id", "epidemic", "position", "R0_error")]
  for (m in METHODS[-1]) {
    o <- merge(d[d$method == m, c("id", "R0_error")], b, by = "id", suffixes = c("", "_b"))
    cell <- function(sel) {
      x <- o[sel, ]; dl <- x$R0_error - x$R0_error_b
      per <- tapply(dl, x$epidemic, sum); n <- tapply(dl, x$epidemic, length)
      bt <- replicate(2000, { i <- sample(length(per), replace = TRUE); sum(per[i]) / sum(n[i]) })
      sprintf("%.2f (%.2f, %.2f)", mean(dl), quantile(bt, 0.025), quantile(bt, 0.975))
    }
    L <- c(L, row(if (m == METHODS[2]) M else "", LAB[[m]], cell(rep(TRUE, nrow(o))), cell(o$position == "early"),
                  cell(o$position == "middle"), cell(o$position == "late")))
  }
  if (M == "SEIR") L <- c(L, "\\midrule")
}
write_tab(gsub("(?<![A-Za-z-])-(?=[0-9])", "$-$", c(L, "\\bottomrule", "\\end{tabular}"), perl = TRUE), "sim_paired.tex")

# ---- Curve recovery ----------------------------------------------------------------------------
L <- c("\\begin{tabular}{llrrrrrrrrr}", "\\toprule",
       row("", "", "\\multicolumn{3}{c}{New infections, window}", "\\multicolumn{3}{c}{Infectious, window}",
           "\\multicolumn{3}{c}{New infections, next 14 days}"),
       "\\cmidrule(lr){3-5}\\cmidrule(lr){6-8}\\cmidrule(lr){9-11}",
       row("Model", "Method", rep(c("Cov. (\\%)", "MAE", "WIS"), 3)), "\\midrule")
for (M in c("SEIR", "SIR")) {
  d <- sim[sim$model == M, ]
  SM <- SIM_METHODS[SIM_METHODS %in% d$method]
  g <- function(col, fun = mean) sapply(SM, function(m) fun(d[d$method == m, col], na.rm = TRUE))
  cols <- list()
  for (p in c("new", "infected", "future"))
    cols <- c(cols, list(bold_near(g(paste0(p, "_coverage")), 1), bold_min(g(paste0(p, "_MAE")), 1), bold_min(g(paste0(p, "_WIS")), 1)))
  for (k in seq_along(SM)) L <- c(L, row(if (k == 1) M else "", LAB[[SM[k]]], sapply(cols, `[`, k)))
  if (M == "SEIR") L <- c(L, "\\midrule")
}
write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "sim_curves.tex")

# ---- Cumulative incidence (evaluation only; every method is fitted to daily counts) -----------------
L <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
       row("", "", "\\multicolumn{3}{c}{Window}", "\\multicolumn{3}{c}{Next 14 days}"),
       "\\cmidrule(lr){3-5}\\cmidrule(lr){6-8}",
       row("Model", "Method", rep(c("Cov. (\\%)", "Rel.\\ MAE (\\%)", "WIS"), 2)), "\\midrule")
for (M in c("SEIR", "SIR")) {
  d <- sim[sim$model == M, ]
  SM <- SIM_METHODS[SIM_METHODS %in% d$method]
  g <- function(col) sapply(SM, function(m) mean(d[d$method == m, col], na.rm = TRUE))
  cols <- list()
  for (p in c("cumulative", "future_cumulative"))
    cols <- c(cols, list(bold_near(g(paste0(p, "_coverage")), 1), bold_min(100 * g(paste0(p, "_rel_MAE")), 1), bold_min(g(paste0(p, "_WIS")), 1)))
  for (k in seq_along(SM)) L <- c(L, row(if (k == 1) M else "", LAB[[SM[k]]], sapply(cols, `[`, k)))
  if (M == "SEIR") L <- c(L, "\\midrule")
}
write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "sim_cumulative.tex")

# ---- Starting state: DeepIMC's estimate or one fitted by the renewal equation ----------------------
if (any(sim$method == "BiLSTM-renewal-start")) {
  L <- c("\\begin{tabular}{lllrrrrrrr}", "\\toprule",
         row("", "", "State", "\\multicolumn{3}{c}{New infections, window}", "\\multicolumn{3}{c}{Infectious, window}", ""),
         "\\cmidrule(lr){4-6}\\cmidrule(lr){7-9}",
         row("Model", "Starting state", "error", rep(c("Cov. (\\%)", "MAE", "WIS"), 2), "Cov., 14 days"), "\\midrule")
  for (M in c("SEIR", "SIR")) {
    for (m in c("BiLSTM", "BiLSTM-renewal-start")) {
      x <- sim[sim$model == M & sim$method == m, ]
      L <- c(L, row(if (m == "BiLSTM") M else "", if (m == "BiLSTM") "DeepIMC" else "renewal equation", f(mean(x$state_error), 1),
                    f(mean(x$new_coverage), 1), f(mean(x$new_MAE), 1), f(mean(x$new_WIS), 1),
                    f(mean(x$infected_coverage), 1), f(mean(x$infected_MAE), 1), f(mean(x$infected_WIS), 1),
                    f(mean(x$future_coverage, na.rm = TRUE), 1)))
    }
    if (M == "SEIR") L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "sim_start.tex")
}

}

# ---- Real data -----------------------------------------------------------------------------------
real <- read.csv(file.path(EX, "real_five/results_by_window.csv"))
real <- real[real$done & !real$failed, ]
DATASETS <- c("Utah COVID-19 admissions", "Utah influenza admissions", "Baltimore influenza 1918",
              "Hong Kong SARS 2003", "Sierra Leone Ebola 2014-15", "Utah COVID-19 cases")
# One table per part (window, next 14 days, cumulative): blocks of coverage, MAE and WIS, a column per
# method (DeepIMC on raw and smoothed counts first). Bold: coverage closest to 95% and lowest MAE / WIS.
# A block may name its own part (prefix), and `labels` renames datasets in the table.
real_table <- function(prefix, file, blocks, datasets = DATASETS, methods = REAL_METHODS, labels = NULL) {
  RM <- methods[methods %in% real$method]
  L <- c(paste0("\\begin{tabular}{ll", strrep("r", length(RM)), "}"), "\\toprule",
         row("Dataset", "Model", sapply(RM, function(m) LAB[[m]])), "\\midrule")
  for (b in blocks) {
    L <- c(L, paste0("\\multicolumn{", length(RM) + 2, "}{l}{\\textit{", b$label, "}} \\\\"))
    pre <- if (is.null(b$prefix)) prefix else b$prefix
    for (ds in datasets) for (M in c("SEIR", "SIR")) {
      d <- real[real$dataset == ds & real$model == M, ]
      if (!nrow(d)) next
      v <- sapply(RM, function(m) b$scale * mean(d[[paste0(pre, "_", b$col)]][d$method == m], na.rm = TRUE))
      lab <- if (ds %in% names(labels)) labels[[ds]] else ds
      name <- if (M == "SEIR") paste0(lab, if (ds == "Utah COVID-19 cases") "$^\\dagger$" else "", " (", length(unique(d$id)), ")") else ""
      L <- c(L, row(name, M, if (b$col == "coverage") bold_near(v, 0) else bold_min(v, b$digits)))
    }
    if (!identical(b, blocks[[length(blocks)]])) L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), file)
}
blocks3 <- list(list(label = "Coverage of the observed counts (\\%)", col = "coverage", scale = 1, digits = 0),
                list(label = "MAE of the median curve", col = "MAE", scale = 1, digits = 1),
                list(label = "Weighted interval score (WIS)", col = "WIS", scale = 1, digits = 1))
real_table("window", "real_results.tex", blocks3)
real_table("future", "real_future.tex", blocks3)
if ("cumulative_rel_MAE" %in% names(real))
  real_table("cumulative", "real_cumulative.tex",
             list(list(label = "Coverage of the observed cumulative counts (\\%)", col = "coverage", scale = 1, digits = 0),
                  list(label = "MAE of the cumulative curve, \\% of the final count", col = "rel_MAE", scale = 100, digits = 1)))

# ---- Timing (CPU only; the networks for 15-365 days, windows of the 15-365-day test) ---------------------
TD <- file.path(EX, "timing_cpu_w15")
tf <- file.path(TD, "inference_seconds.csv")
if (file.exists(tf)) {
  t <- read.csv(tf)
  # The experiments' first call also predicted the 100,000 calibration windows of the R0 range, which
  # the package saves at training time; the first call of the package itself is timed separately.
  calib_h <- setNames(t$seconds[t$method == "BiLSTM" & t$what == "first call (loads Python and networks)"] / 3600,
                      t$model[t$method == "BiLSTM" & t$what == "first call (loads Python and networks)"])
  ff <- file.path(TD, "first_call_seconds.csv")
  if (file.exists(ff)) {
    t <- t[!(t$method == "BiLSTM" & t$what == "first call (loads Python and networks)"), ]
    t <- rbind(t, subset(read.csv(ff), what == "first call (loads Python and networks)"))
  }
  get <- function(M, m, w = "one window") { x <- t$seconds[t$model == M & t$method == m & t$what == w]; if (length(x)) x[1] else NA }
  fmt <- function(x) ifelse(is.na(x), "--", ifelse(x < 1, formatC(x, digits = 3, format = "f"), formatC(x, digits = 1, format = "f", big.mark = ",")))
  L <- c("\\begin{tabular}{lrr}", "\\toprule", row("", "SEIR", "SIR"), "\\midrule",
         "\\multicolumn{3}{l}{\\textit{Calibrating one window (seconds, one CPU core)}} \\\\",
         row("DeepIMC, first call (starts Python, loads networks)", fmt(get("SEIR", "BiLSTM", "first call (loads Python and networks)")), fmt(get("SIR", "BiLSTM", "first call (loads Python and networks)"))),
         row("DeepIMC, one window", fmt(get("SEIR", "BiLSTM")), fmt(get("SIR", "BiLSTM"))),
         row("DeepIMC, 10 windows in one pass, per window", fmt(get("SEIR", "BiLSTM", "10 windows in one pass, per window")), fmt(get("SIR", "BiLSTM", "10 windows in one pass, per window"))))
  for (m in METHODS[-1]) L <- c(L, row(LAB[[m]], fmt(get("SEIR", m)), fmt(get("SIR", m))))
  L <- c(L, "\\midrule", "\\multicolumn{3}{l}{\\textit{From a fit to a 95\\% band (seconds per window)}} \\\\",
         row("500 runs of the agent-based model", fmt(get("SEIR", "band: 500 model runs")), fmt(get("SIR", "band: 500 model runs"))),
         row("Renewal equation", fmt(get("SEIR", "band: renewal equation")), fmt(get("SIR", "band: renewal equation"))))
  tr <- file.path(TD, "training_cpu.csv")
  if (file.exists(tr)) {
    tt <- read.csv(tr)
    # The epoch's own seconds (without reading the data), from the training log (SEIR first, then SIR).
    lg <- readLines(list.files(file.path(TD, "logs"), "^timing_train_.*\\.out$", full.names = TRUE)[1])
    ep <- as.numeric(sub(".*  ([0-9.]+)s$", "\\1", grep("^epoch 1/1", lg, value = TRUE)))
    tt$training_hours_1m_50_epochs <- ep[match(tt$model, c("SEIR", "SIR"))] * (1e6 / tt$subset_epidemics) * tt$epochs / 3600
    g2 <- function(M, col) tt[[col]][tt$model == M]
    L <- c(L, "\\midrule", "\\multicolumn{3}{l}{\\textit{One-time cost of DeepIMC (CPU)}} \\\\",
           row("Simulating 1,000,000 training epidemics (CPU-hours)", fmt(g2("SEIR", "simulate_cpu_hours_1m")), fmt(g2("SIR", "simulate_cpu_hours_1m"))),
           row("Estimates for 100,000 calibration windows (hours, 1 core)", sprintf("%.1f", calib_h[["SEIR"]]), sprintf("%.1f", calib_h[["SIR"]])),
           row(sprintf("Training one network, 50 epochs (hours, %d cores, estimated)", tt$cores_for_training[1]),
               fmt(g2("SEIR", "training_hours_1m_50_epochs")), fmt(g2("SIR", "training_hours_1m_50_epochs"))),
           # As the networks for 15-365 days were trained: the SLURM time of each network's training jobs, summed
           # (several GPU types; two SEIR networks were moved from a slower GPU part way through).
           "\\midrule", "\\multicolumn{3}{l}{\\textit{For reference: training as done, on GPUs}} \\\\",
           row("Training one network, 50 epochs (hours, one GPU)", "5.8--11.4", "2.7--9.9"))
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "timing.tex")
}

# ---- Appendix: ABC on the whole curve or on summary statistics (216 windows of the 15-365-day test; means over
# the windows where all four versions finished) ---------------------------------------------------------------
af <- file.path(EX, "abc_distance_w15/results.csv")
if (file.exists(af)) {
  ad <- read.csv(af)
  ABC4 <- c("ABC-MCMC", "ABC-MCMC-summary", "ABC-SMC", "ABC-SMC-summary")
  L <- c("\\begin{tabular}{lllrrrrrr}", "\\toprule",
         row("Model", "Method", "Compares", "Finished", "$R_0$ MAE", "State error", "Coverage (\\%)", "MAE", "Seconds"), "\\midrule")
  for (M in c("SEIR", "SIR")) {
    x <- ad[ad$model == M, ]
    both <- Reduce(intersect, lapply(ABC4, function(m) x$id[x$method == m & !x$failed]))
    for (m in ABC4) {
      d <- x[x$method == m & x$id %in% both, ]
      L <- c(L, row(if (m == "ABC-MCMC") M else "", sub("-summary", "", m), if (grepl("summary", m)) "summary statistics" else "whole curve",
                    sprintf("%d of %d", sum(x$method == m & !x$failed), length(unique(x$id))),
                    f(mean(d$R0_error), 2), f(mean(d$state_error), 1), f(mean(d$new_coverage), 1), f(mean(d$new_MAE), 1), f(mean(d$seconds), 0)))
    }
    if (M == "SEIR") L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "abc_sensitivity.tex")
}

# ---- From DeepIMC's estimates to curves: 500 model runs or the renewal equation (10,000 windows) ----
rf <- list.files(SIM_DIR, "^renewal_[0-9]+\\.csv$", full.names = TRUE)
if (length(rf) && exists("sim")) {
  keep <- c("model", "id", "approach", "seconds", "state_error", "new_coverage", "new_MAE", "infected_coverage",
            "infected_MAE", "future_coverage", "future_MAE")
  ren <- do.call(rbind, lapply(rf, function(x) read.csv(x)[keep]))
  b <- sim[sim$method == "BiLSTM", ]
  L <- c("\\begin{tabular}{llrrrrrrrr}", "\\toprule",
         row("", "", "State", "\\multicolumn{2}{c}{New infections}", "\\multicolumn{2}{c}{Currently infectious}",
             "\\multicolumn{2}{c}{Next 14 days}", "Seconds"),
         "\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}\\cmidrule(lr){8-9}",
         row("Model", "Curves from", "error", "Cov.", "MAE", "Cov.", "MAE", "Cov.", "MAE", "per window"), "\\midrule")
  for (M in c("SEIR", "SIR")) {
    x <- b[b$model == M, ]
    L <- c(L, row(M, "model, 500 runs", f(mean(x$state_error), 1), f(mean(x$new_coverage, na.rm = TRUE), 1), f(mean(x$new_MAE, na.rm = TRUE), 1),
                  f(mean(x$infected_coverage, na.rm = TRUE), 1), f(mean(x$infected_MAE, na.rm = TRUE), 1), f(mean(x$future_coverage, na.rm = TRUE), 1),
                  f(mean(x$future_MAE, na.rm = TRUE), 1), f(mean(x$band_seconds, na.rm = TRUE), 1)))
    for (a in c("renewal A", "renewal B")) {
      y <- ren[ren$model == M & ren$approach == a, ]
      L <- c(L, row("", a, f(mean(y$state_error), 1), f(mean(y$new_coverage), 1), f(mean(y$new_MAE), 1),
                    f(mean(y$infected_coverage), 1), f(mean(y$infected_MAE), 1), f(mean(y$future_coverage, na.rm = TRUE), 1),
                    f(mean(y$future_MAE, na.rm = TRUE), 1), f(mean(y$seconds), if (mean(y$seconds) < 10) 2 else 1)))
    }
    if (M == "SEIR") L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "renewal.tex")
}

# ---- Appendix: versions of the networks (R0, ptran and crate errors): three for windows of 30-365 days and the
# final settings trained for 15-365 days (the networks of the paper); 15-day windows only for the last ------------
vf <- file.path(EX, "versions_params/versions_summary.csv")
if (file.exists(vf)) {
  v <- read.csv(vf)
  v$version <- factor(v$version, levels = c("current", "tuned", "final", "final15"))
  v <- v[order(v$model, v$version), ]
  if (!"w15_R0" %in% names(v)) v$w15_R0 <- NA
  L <- c("\\begin{tabular}{lllrrrrrrr}", "\\toprule",
         row("", "", "Trained", "\\multicolumn{3}{c}{61-day windows from the beginning}", "\\multicolumn{3}{c}{Windows of 30--365 days}",
             "15 days"), "\\cmidrule(lr){4-6}\\cmidrule(lr){7-9}\\cmidrule(lr){10-10}",
         row("Model", "Version", "on (days)", "$R_0$", "$p_{tran}$", "$c_{rate}$", "$R_0$", "$p_{tran}$", "$c_{rate}$", "$R_0$"), "\\midrule")
  for (M in c("SEIR", "SIR")) {
    x <- v[v$model == M, ]
    for (k in seq_len(nrow(x))) L <- c(L, row(if (k == 1) M else "",
      c(current = "Current", tuned = "Tuned", final = "Final", final15 = "Final")[as.character(x$version[k])],
      if (x$version[k] == "final15") "15--365" else "30--365",
      f(x$beginning61_R0[k], 3), f(x$beginning61_ptran[k], 4), f(x$beginning61_crate[k], 2),
      f(x$all_R0[k], 3), f(x$all_ptran[k], 4), f(x$all_crate[k], 2), f(x$w15_R0[k], 3)))
    if (M == "SEIR") L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "versions.tex")
}

# ---- Sensitivity: DE with a fixed seed (as in the paper) or a new seed for every model run -------------
df <- file.path(EX, "de_typical_user_w15/results.csv")   # 1,000 windows of the 15-365-day test
if (file.exists(df)) {
  de <- read.csv(df); de <- de[!de$failed, ]
  rd <- real[real$method %in% c("DE", "DE-random-seed") & real$main, ]
  L <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
         row("", "", "\\multicolumn{4}{c}{1,000 simulated windows}", "\\multicolumn{2}{c}{Real data (window)}"),
         "\\cmidrule(lr){3-6}\\cmidrule(lr){7-8}",
         row("Model", "Method", "$R_0$ MAE", "State error", "MAE, new inf.", "Cov., infectious (\\%)", "Cov. (\\%)", "MAE"), "\\midrule")
  for (M in c("SEIR", "SIR")) {
    for (m in c("BiLSTM", "DE", "DE-random-seed")) {
      x <- de[de$model == M & de$method == m, ]; y <- rd[rd$model == M & rd$method == m, ]
      L <- c(L, row(if (m == "BiLSTM") M else "", LAB[[m]], f(mean(x$R0_error), 2), f(mean(x$state_error), 1), f(mean(x$new_MAE), 1),
                    f(mean(x$infected_coverage), 1), if (nrow(y)) f(mean(y$window_coverage), 1) else "--",
                    if (nrow(y)) f(mean(y$window_MAE), 1) else "--"))
    }
    if (M == "SEIR") L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "de_typical.tex")
}

# ---- Sensitivity: ABC-SMC with 6, 12 or 20 generations (120 windows of the 15-365-day test) -----
gf <- file.path(EX, "abc_smc_generations_w15/results.csv")
if (file.exists(gf)) {
  g <- read.csv(gf)
  L <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
         row("Model", "Generations", "Finished", "$R_0$ MAE", "Range has true (\\%)", "Range width", "Model runs", "Seconds"), "\\midrule")
  for (M in c("SEIR", "SIR")) {
    for (k in c(6, 12, 20)) {
      x <- g[g$model == M & g$generations == k, ]; ok <- x[x$done & !x$failed, ]
      L <- c(L, row(if (k == 6) M else "", k, sprintf("%d of %d", nrow(ok), nrow(x)),
                    if (nrow(ok) >= 10) f(mean(ok$R0_error), 2) else "--", if (nrow(ok) >= 10) f(100 * mean(ok$inside), 0) else "--",
                    if (nrow(ok) >= 10) f(mean(ok$width), 2) else "--", if (nrow(ok)) f(mean(ok$model_runs), 0) else "--",
                    if (nrow(ok)) f(mean(ok$seconds), 0) else "--"))
    }
    if (M == "SEIR") L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "smc_generations.tex")
}

# ---- Supplement: every way of reading the counts for DeepIMC on the real data (raw and 11 smoothers),
# each without and with weighting by the data; window WIS and coverage over the five datasets (both models),
# and the weighted WIS of each dataset ------------------------------------------------------------------------
SMOOTHERS <- c(`Raw counts` = "BiLSTM", `Gaussian kernel, 20\\%` = "BiLSTM-k20", `Gaussian kernel, 50\\%` = "BiLSTM-k50",
               `Gaussian kernel, 2.5 days` = "BiLSTM-g2.5", `7-day moving average` = "BiLSTM-box7",
               `Epanechnikov kernel` = "BiLSTM-epan", `Loess, 20\\%` = "BiLSTM-lowess20", `Loess, 50\\%` = "BiLSTM-lowess50",
               `Robust lowess, 20\\%` = "BiLSTM-rlowess20", `Robust lowess, 50\\%` = "BiLSTM-rlowess50",
               `7-day running median` = "BiLSTM-med7", `Automatic` = "BiLSTM-auto")
if (all(c(SMOOTHERS, paste0(SMOOTHERS[-1], "-weighted"), "BiLSTM-weighted") %in% real$method)) {
  rm_ <- real[real$main, ]
  DS5 <- DATASETS[DATASETS != "Utah COVID-19 cases"]
  short <- c(`Utah COVID-19 admissions` = "COVID-19", `Utah influenza admissions` = "Influenza", `Baltimore influenza 1918` = "Baltimore",
             `Hong Kong SARS 2003` = "SARS", `Sierra Leone Ebola 2014-15` = "Ebola")
  mw <- function(m) if (m == "BiLSTM") "BiLSTM-weighted" else paste0(m, "-weighted")
  mean_of <- function(m, col, ds = NULL) { x <- rm_[rm_$method == m & (is.null(ds) | rm_$dataset %in% ds), col]; mean(x, na.rm = TRUE) }
  stats <- t(sapply(SMOOTHERS, function(m) c(mean_of(m, "window_WIS"), mean_of(mw(m), "window_WIS"), mean_of(m, "window_coverage"),
                                             mean_of(mw(m), "window_coverage"), sapply(DS5, function(ds) mean_of(mw(m), "window_WIS", ds)))))
  L <- c("\\begin{tabular}{lrrrrrrrrr}", "\\toprule",
         row("", "\\multicolumn{2}{c}{WIS}", "\\multicolumn{2}{c}{Coverage (\\%)}", "\\multicolumn{5}{c}{WIS weighted, by dataset}"),
         "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-10}",
         row("DeepIMC reads", "not w.", "weighted", "not w.", "weighted", short[DS5]), "\\midrule")
  cols <- c(list(bold_min(stats[, 1], 2), bold_min(stats[, 2], 2), f(stats[, 3], 1), f(stats[, 4], 1)),
            lapply(5:9, function(j) bold_min(stats[, j], 2)))
  for (k in seq_along(SMOOTHERS)) L <- c(L, row(names(SMOOTHERS)[k], sapply(cols, `[`, k)))
  L <- c(L, "\\midrule")
  for (m in c("ABC-SMC-summary", "DE"))
    L <- c(L, row(LAB[[m]], f(mean_of(m, "window_WIS"), 2), "", f(mean_of(m, "window_coverage"), 1), "",
                  sapply(DS5, function(ds) f(mean_of(m, "window_WIS", ds), 2))))
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "real_smoothing.tex")
}

# ---- Fit check: does any of DeepIMC's draws reproduce the window? (distance of the closest draw; above 1 means
# further than twice the Poisson noise on the square-root scale) --------------------------------------------
if ("fit_best" %in% names(real) && exists("sim") && "fit_best" %in% names(sim)) {
  rw_ <- real[real$main & real$method == "BiLSTM-weighted", c("id", "dataset", "fit_best")]
  ratio <- merge(merge(rw_, real[real$method == "BiLSTM", c("id", "window_WIS")], by = "id"),
                 real[real$method == "DE", c("id", "window_WIS")], by = "id", suffixes = c("_d", "_de"))
  ratio$flag <- ratio$fit_best > 1
  L <- c("\\begin{tabular}{lrrrr}", "\\toprule",
         row("", "Windows", "Not reproduced (\\%)", "\\multicolumn{2}{c}{DeepIMC WIS / DE WIS}"), "\\cmidrule(lr){4-5}",
         row("Data", "", "", "reproduced", "not reproduced"), "\\midrule")
  sw_ <- sim[sim$method == "BiLSTM-weighted", ]
  L <- c(L, row("Simulated (10,000 windows)", f(nrow(sw_), 0), f(100 * mean(sw_$fit_best > 1, na.rm = TRUE), 1), "", ""))
  for (ds in DATASETS[DATASETS != "Utah COVID-19 cases"]) {
    x <- ratio[ratio$dataset == ds, ]
    med <- function(z) if (length(z)) f(median(z), 2) else "--"
    L <- c(L, row(ds, nrow(x), f(100 * mean(x$flag), 0), med(x$window_WIS_d[!x$flag] / x$window_WIS_de[!x$flag]),
                  med(x$window_WIS_d[x$flag] / x$window_WIS_de[x$flag])))
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "fit_check.tex")
}

# ---- Supplement: six more outbreaks (a separate check, not in the five-dataset averages), with Hong Kong SARS
# for comparison: the results by outbreak, the noise of the counts and the fit check, and Hong Kong's windows
# before, across and after its largest one-day jump ------------------------------------------------------------
EXTRA <- c("Toronto SARS 2003", "South Korea MERS 2015", "Kosovo smallpox 1972", "Pennsylvania school influenza 2009",
           "Hagelloch measles 1861", "Kikwit Ebola 1995")
# The SARS cases are those of all of Canada (most of them in Toronto).
EXTRA_LAB <- c(`Toronto SARS 2003` = "Canada SARS 2003", `Pennsylvania school influenza 2009` = "Pennsylvania influenza 2009")
EXTRA_METHODS <- c("BiLSTM", "BiLSTM-weighted", "BiLSTM-k20-weighted", "BiLSTM-k50-weighted", METHODS[-1])
if (all(EXTRA %in% real$dataset) && "fit_best" %in% names(real)) {
  real_table("window", "real_extra.tex",
             list(list(label = "Coverage of the observed counts in the window (\\%)", col = "coverage", scale = 1, digits = 0),
                  list(label = "WIS in the window", col = "WIS", scale = 1, digits = 2),
                  list(label = "WIS in the next 14 days", col = "WIS", scale = 1, digits = 2, prefix = "future")),
             datasets = c("Hong Kong SARS 2003", EXTRA), methods = EXTRA_METHODS, labels = EXTRA_LAB)

  rw <- readRDS(file.path(EX, "real_five/windows.rds"))
  # Noise of the counts around a centred 7-day mean (variance over mean; the agent-based model's own curves give
  # about 0.9), as dispersion_index() in examples/methods_common.R.
  disp <- function(y) { m <- stats::filter(y, rep(1 / 7, 7), sides = 2); ok <- !is.na(m) & m > 0.5
                        if (sum(ok) < 3) NA else sum((y[ok] - m[ok])^2) / sum(m[ok]) }
  info <- do.call(rbind, lapply(names(rw), function(id) { w <- rw[[id]]
    data.frame(id = id, dataset = w$dataset, start = as.numeric(min(w$time)), end = as.numeric(max(w$time)),
               length = w$length, peak = max(w$y), disp = disp(w$y)) }))
  wis_of_m <- function(m) { x <- real[real$method == m, ]; x$window_WIS[match(info$id, x$id)] }
  info$fit_best <- { x <- real[real$method == "BiLSTM-weighted", ]; x$fit_best[match(info$id, x$id)] }
  info$w <- wis_of_m("BiLSTM-weighted"); info$de <- wis_of_m("DE"); info$smc <- wis_of_m("ABC-SMC-summary")
  ok <- function(x) x[is.finite(x)]
  check_row <- function(name, x) row(name, nrow(x), if (length(unique(x$dataset)) == 1) f(max(x$peak), 0) else "",
                                     f(median(x$disp[x$length >= 30], na.rm = TRUE), 2), f(100 * mean(x$fit_best > 1, na.rm = TRUE), 0),
                                     f(median(ok(x$w / x$de)), 2), f(median(ok(x$w / x$smc)), 2))
  L <- c("\\begin{tabular}{lrrrrrr}", "\\toprule",
         row("", "Windows", "Peak", "Dispersion", "Not repro-", "\\multicolumn{2}{c}{Weighted DeepIMC WIS /}"), "\\cmidrule(lr){6-7}",
         row("Outbreak", "", "count", "index", "duced (\\%)", "DE WIS", "ABC-SMC WIS"), "\\midrule",
         check_row("Hong Kong SARS 2003", info[info$dataset == "Hong Kong SARS 2003", ]), "\\midrule")
  for (ds in EXTRA) L <- c(L, check_row(if (ds %in% names(EXTRA_LAB)) EXTRA_LAB[[ds]] else ds, info[info$dataset == ds, ]))
  L <- c(L, "\\midrule", check_row("All six", info[info$dataset %in% EXTRA, ]))
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "real_extra_check.tex")

  # Hong Kong: the largest one-day increase of the daily counts (rebuilt from the windows), and the windows that
  # end before it, contain both of its days, or start after it.
  hk <- info[info$dataset == "Hong Kong SARS 2003", ]
  series <- do.call(rbind, lapply(rw[hk$id], function(w) data.frame(t = as.numeric(w$time), y = w$y)))
  series <- series[!duplicated(series$t), ]; series <- series[order(series$t), ]
  J <- series$t[which.max(diff(series$y)) + 1]
  hk$part <- ifelse(hk$end < J, "before", ifelse(hk$start >= J, "after", "across"))
  HK_METHODS <- EXTRA_METHODS[EXTRA_METHODS %in% real$method]
  parts <- c(before = "end before", across = "contain", after = "start after")
  xs <- lapply(names(parts), function(p) hk[hk$part == p, ])
  wis <- sapply(xs, function(x) sapply(HK_METHODS, function(m) { y <- real[real$method == m, ]; mean(y$window_WIS[match(x$id, y$id)], na.rm = TRUE) }))
  cols <- lapply(seq_along(parts), function(j) bold_min(wis[, j], 2))
  L <- c("\\begin{tabular}{lrrr}", "\\toprule",
         row("", "\\multicolumn{3}{c}{Windows that \\ldots\\ the jump}"), "\\cmidrule(lr){2-4}", row("", parts), "\\midrule",
         row("Windows (both models)", sapply(xs, nrow)),
         row("Not reproduced (\\%)", sapply(xs, function(x) f(100 * mean(x$fit_best > 1, na.rm = TRUE), 0))), "\\midrule",
         "\\multicolumn{4}{l}{\\textit{WIS in the window}} \\\\")
  for (k in seq_along(HK_METHODS)) L <- c(L, row(LAB[[HK_METHODS[k]]], sapply(cols, `[`, k)))
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "hk_jump.tex")
  cat("Hong Kong jump: day", J, "from", series$y[series$t == J - 1], "to", series$y[series$t == J], "cases\n")
}
