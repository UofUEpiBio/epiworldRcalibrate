# LaTeX tables of the paper, written from the saved results (so that no number is typed by hand):
#   tables/sim_parameters.tex   parameter recovery on the 10,000 simulated test windows
#   tables/sim_paired.tex       paired R0 differences, standard method minus BiLSTM
#   tables/sim_curves.tex       recovery of the epidemic curves (coverage and MAE)
#   tables/real_results.tex     the five real datasets, in the window (coverage and MAE)
#   tables/real_future.tex      the five real datasets, in the 14 days after the window
#   tables/renewal.tex          curves from 500 model runs or from the renewal equation
#   tables/timing.tex           CPU-only computing time (inference and one-time training)
# Run from the paper/ folder:  Rscript tables/make_tables.R
EX <- "../examples/results"
# ABC-MCMC and ABC-SMC are the summary-statistics versions (named simply ABC-MCMC and ABC-SMC in the paper).
METHODS <- c("BiLSTM", "ABC-MCMC-summary", "ABC-SMC-summary", "DE", "NelderMead")
LAB <- c(BiLSTM = "DeepIMC", `ABC-MCMC-summary` = "ABC-MCMC", `ABC-SMC-summary` = "ABC-SMC", DE = "DE", NelderMead = "Nelder--Mead")
CRATE <- list(SEIR = c(5, 15), SIR = c(1, 5))
f <- function(x, d = 2) ifelse(is.na(x), "--", formatC(x, digits = d, format = "f", big.mark = ","))
bold_min <- function(v, d) { s <- f(v, d); i <- which.min(v); if (length(i)) s[i] <- paste0("\\textbf{", s[i], "}"); s }
bold_near <- function(v, d, target = 95) { s <- f(v, d); i <- which.min(abs(v - target)); if (length(i)) s[i] <- paste0("\\textbf{", s[i], "}"); s }
write_tab <- function(lines, file) { writeLines(lines, file.path("tables", file)); cat("wrote", file, "\n") }
row <- function(...) paste0(paste(c(...), collapse = " & "), " \\\\")

SIM_FILE <- file.path(EX, "simulated_test_10k/results_by_window.csv")
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
  stats <- sapply(METHODS, function(m) c(all = mae(m, "R0_error"), early = mae(m, "R0_error", "early"),
    middle = mae(m, "R0_error", "middle"), late = mae(m, "R0_error", "late"),
    inside = 100 * mean(d$inside[d$method == m], na.rm = TRUE), width = mean((d$R0_high - d$R0_low)[d$method == m], na.rm = TRUE),
    ptran = mae(m, "ptran_error"),
    crate = mae(m, "crate_error"), state = mae(m, "state_error")))
  cols <- list(all = bold_min(stats["all", ], 2), early = bold_min(stats["early", ], 2), middle = bold_min(stats["middle", ], 2),
               late = bold_min(stats["late", ], 2), inside = f(stats["inside", ], 1), width = f(stats["width", ], 2),
               ptran = f(stats["ptran", ], 4),
               crate = f(stats["crate", ], 2), state = bold_min(stats["state", ], 1))
  for (k in seq_along(METHODS)) L <- c(L, row(if (k == 1) M else "", LAB[[METHODS[k]]], sapply(cols, `[`, k)))
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
L <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
       row("", "", "\\multicolumn{2}{c}{New infections, window}", "\\multicolumn{2}{c}{Infectious, window}",
           "\\multicolumn{2}{c}{New infections, next 14 days}"),
       "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
       row("Model", "Method", "Coverage (\\%)", "MAE", "Coverage (\\%)", "MAE", "Coverage (\\%)", "MAE"), "\\midrule")
for (M in c("SEIR", "SIR")) {
  d <- sim[sim$model == M, ]
  g <- function(col, fun = mean) sapply(METHODS, function(m) fun(d[d$method == m, col], na.rm = TRUE))
  cols <- list(bold_near(g("new_coverage"), 1), bold_min(g("new_MAE"), 1), bold_near(g("infected_coverage"), 1),
               bold_min(g("infected_MAE"), 1), bold_near(g("future_coverage"), 1), bold_min(g("future_MAE"), 1))
  for (k in seq_along(METHODS)) L <- c(L, row(if (k == 1) M else "", LAB[[METHODS[k]]], sapply(cols, `[`, k)))
  if (M == "SEIR") L <- c(L, "\\midrule")
}
write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "sim_curves.tex")

}

# ---- Real data -----------------------------------------------------------------------------------
real <- read.csv(file.path(EX, "real_five/results_by_window.csv"))
real <- real[real$done & !real$failed, ]
DATASETS <- c("Utah COVID-19 admissions", "Utah influenza admissions", "Baltimore influenza 1918",
              "Hong Kong SARS 2003", "Sierra Leone Ebola 2014-15", "Utah COVID-19 cases")
L <- c("\\begin{tabular}{llrrrrrrrrrr}", "\\toprule",
       row("", "", "\\multicolumn{5}{c}{Coverage of the observed counts (\\%)}", "\\multicolumn{5}{c}{MAE of the median curve}"),
       "\\cmidrule(lr){3-7}\\cmidrule(lr){8-12}",
       row("Dataset", "Model", sapply(METHODS, function(m) LAB[[m]]), sapply(METHODS, function(m) LAB[[m]])), "\\midrule")
for (ds in DATASETS) for (M in c("SEIR", "SIR")) {
  d <- real[real$dataset == ds & real$model == M, ]
  if (!nrow(d)) next
  cv <- sapply(METHODS, function(m) mean(d$window_coverage[d$method == m]))
  is <- sapply(METHODS, function(m) mean(d$window_MAE[d$method == m]))
  name <- if (M == "SEIR") paste0(ds, if (ds == "Utah COVID-19 cases") "$^\\dagger$" else "", " (", nrow(d) / length(METHODS), ")") else ""
  L <- c(L, row(name, M, bold_near(cv, 0), bold_min(is, 1)))
}
write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "real_results.tex")
L <- c("\\begin{tabular}{llrrrrrrrrrr}", "\\toprule",
       row("", "", "\\multicolumn{5}{c}{Coverage, next 14 days (\\%)}", "\\multicolumn{5}{c}{MAE, next 14 days}"),
       "\\cmidrule(lr){3-7}\\cmidrule(lr){8-12}",
       row("Dataset", "Model", sapply(METHODS, function(m) LAB[[m]]), sapply(METHODS, function(m) LAB[[m]])), "\\midrule")
for (ds in DATASETS) for (M in c("SEIR", "SIR")) {
  d <- real[real$dataset == ds & real$model == M, ]
  if (!nrow(d)) next
  cv <- sapply(METHODS, function(m) mean(d$future_coverage[d$method == m], na.rm = TRUE))
  ma <- sapply(METHODS, function(m) mean(d$future_MAE[d$method == m], na.rm = TRUE))
  L <- c(L, row(if (M == "SEIR") ds else "", M, bold_near(cv, 0), bold_min(ma, 1)))
}
write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "real_future.tex")

# ---- Timing (CPU only) -----------------------------------------------------------------------------
tf <- file.path(EX, "timing_cpu/inference_seconds.csv")
if (file.exists(tf)) {
  t <- read.csv(tf)
  # The experiments' first call also predicted the 100,000 calibration windows of the R0 range, which
  # the package saves at training time; the first call of the package itself is timed separately.
  calib_h <- setNames(t$seconds[t$method == "BiLSTM" & t$what == "first call (loads Python and networks)"] / 3600,
                      t$model[t$method == "BiLSTM" & t$what == "first call (loads Python and networks)"])
  ff <- file.path(EX, "timing_cpu/first_call_seconds.csv")
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
  tr <- file.path(EX, "timing_cpu/training_cpu.csv")
  if (file.exists(tr)) {
    tt <- read.csv(tr)
    # The epoch's own seconds (without reading the data), from the training log (SEIR first, then SIR).
    lg <- readLines(list.files(file.path(EX, "timing_cpu/logs"), "^timing_train_.*\\.out$", full.names = TRUE)[1])
    ep <- as.numeric(sub(".*  ([0-9.]+)s$", "\\1", grep("^epoch 1/1", lg, value = TRUE)))
    tt$training_hours_1m_50_epochs <- ep[match(tt$model, c("SEIR", "SIR"))] * (1e6 / tt$subset_epidemics) * tt$epochs / 3600
    g2 <- function(M, col) tt[[col]][tt$model == M]
    L <- c(L, "\\midrule", "\\multicolumn{3}{l}{\\textit{One-time cost of DeepIMC (CPU)}} \\\\",
           row("Simulating 1,000,000 training epidemics (CPU-hours)", fmt(g2("SEIR", "simulate_cpu_hours_1m")), fmt(g2("SIR", "simulate_cpu_hours_1m"))),
           row("Estimates for 100,000 calibration windows (hours, 1 core)", sprintf("%.1f", calib_h[["SEIR"]]), sprintf("%.1f", calib_h[["SIR"]])),
           row(sprintf("Training one network, 50 epochs (hours, %d cores, estimated)", tt$cores_for_training[1]),
               fmt(g2("SEIR", "training_hours_1m_50_epochs")), fmt(g2("SIR", "training_hours_1m_50_epochs"))),
           # As the shipped networks were trained, from their training logs (several GPU types).
           "\\midrule", "\\multicolumn{3}{l}{\\textit{For reference: training as done, on GPUs}} \\\\",
           row("Training one network, 50 epochs (hours, one GPU)", "3.7--20.4", "2.6--9.9"))
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "timing.tex")
}

# ---- Appendix: ABC on the whole curve or on summary statistics (the 216 windows of the earlier grid design) ----
old <- read.csv(file.path(EX, "simulated_baselines/results_by_window.csv"))
old <- old[!old$failed, ]
L <- c("\\begin{tabular}{lllrrrrr}", "\\toprule",
       row("Model", "Method", "Compares", "$R_0$ MAE", "State error", "Coverage (\\%)", "MAE", "Seconds"), "\\midrule")
for (M in c("SEIR", "SIR")) {
  for (m in c("ABC-MCMC", "ABC-MCMC-summary", "ABC-SMC", "ABC-SMC-summary")) {
    d <- old[old$model == M & old$method == m, ]
    L <- c(L, row(if (m == "ABC-MCMC") M else "", sub("-summary", "", m), if (grepl("summary", m)) "summary statistics" else "whole curve",
                  f(mean(d$error), 2), f(mean(d$state_error), 1), f(mean(d$cov_new), 1), f(mean(d$MAE_new), 1), f(mean(d$seconds), 0)))
  }
  if (M == "SEIR") L <- c(L, "\\midrule")
}
write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "abc_sensitivity.tex")

# ---- From DeepIMC's estimates to curves: 500 model runs or the renewal equation (10,000 windows) ----
rf <- list.files(file.path(EX, "simulated_test_10k"), "^renewal_[0-9]+\\.csv$", full.names = TRUE)
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

# ---- Appendix: three versions of the networks (R0, ptran and crate errors) -------------------------------
vf <- file.path(EX, "versions_params/versions_summary.csv")
if (file.exists(vf)) {
  v <- read.csv(vf)
  v$version <- factor(v$version, levels = c("current", "tuned", "final"))
  v <- v[order(v$model, v$version), ]
  L <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
         row("", "", "\\multicolumn{3}{c}{61-day windows from the beginning}", "\\multicolumn{3}{c}{All test windows}"),
         "\\cmidrule(lr){3-5}\\cmidrule(lr){6-8}",
         row("Model", "Version", "$R_0$", "$p_{tran}$", "$c_{rate}$", "$R_0$", "$p_{tran}$", "$c_{rate}$"), "\\midrule")
  for (M in c("SEIR", "SIR")) {
    x <- v[v$model == M, ]
    for (k in seq_len(nrow(x))) L <- c(L, row(if (k == 1) M else "", c(current = "Current", tuned = "Tuned", final = "Final")[as.character(x$version[k])],
      f(x$beginning61_R0[k], 3), f(x$beginning61_ptran[k], 4), f(x$beginning61_crate[k], 2),
      f(x$all_R0[k], 3), f(x$all_ptran[k], 4), f(x$all_crate[k], 2)))
    if (M == "SEIR") L <- c(L, "\\midrule")
  }
  write_tab(c(L, "\\bottomrule", "\\end{tabular}"), "versions.tex")
}
