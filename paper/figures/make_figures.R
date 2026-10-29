# Figures of the paper (ggplot2, ggridges, patchwork), from the saved results:
#   examples/results/simulated_test_15_365/results_by_window.csv   (10,000 simulated windows of 15-365 days)
#   examples/results/real_five/results_by_window.csv        (five real datasets, and six more outbreaks)
# Run from the paper/ folder:  Rscript figures/make_figures.R
suppressPackageStartupMessages({ library(ggplot2); library(ggridges); library(patchwork) })
EX <- "../examples/results"
OUT <- "figures"
# ABC-MCMC and ABC-SMC are the summary-statistics versions (named simply ABC-MCMC and ABC-SMC in the paper).
METHODS <- c("BiLSTM", "ABC-MCMC-summary", "ABC-SMC-summary", "DE", "NelderMead")
LABELS <- c(BiLSTM = "DeepIMC", `ABC-MCMC-summary` = "ABC-MCMC", `ABC-SMC-summary` = "ABC-SMC", DE = "DE", NelderMead = "Nelder-Mead",
            `BiLSTM-k20` = "DeepIMC, 20% smoothing", `BiLSTM-k50` = "DeepIMC, 50% smoothing",
            `BiLSTM-auto` = "DeepIMC, automatic", `BiLSTM-weighted` = "DeepIMC, weighted",
            `BiLSTM-k20-weighted` = "DeepIMC, 20% smoothing, weighted", `BiLSTM-k50-weighted` = "DeepIMC, 50% smoothing, weighted")
# On real data DeepIMC also reads counts smoothed with a kernel of 20% or 50% of the window length, each version
# with and without weighting its draws by the data (always against the raw counts).
DEEPIMC6 <- c("BiLSTM", "BiLSTM-k20", "BiLSTM-k50", "BiLSTM-weighted", "BiLSTM-k20-weighted", "BiLSTM-k50-weighted")
REAL_METHODS <- c(DEEPIMC6, METHODS[-1])
SIM_METHODS <- c("BiLSTM", "BiLSTM-weighted", METHODS[-1])
# Validated categorical palette (fixed order; the method name is always on the axis, so colour is
# never the only cue).
COL <- c(BiLSTM = "#2a78d6", `ABC-MCMC-summary` = "#eb6834", `ABC-SMC-summary` = "#1baf7a", DE = "#eda100", NelderMead = "#e87ba4",
         `BiLSTM-k20` = "#8db8ee", `BiLSTM-k50` = "#173f78", `BiLSTM-auto` = "#7b3fbf", `BiLSTM-weighted` = "#7b3fbf",
         `BiLSTM-k20-weighted` = "#b393e3", `BiLSTM-k50-weighted` = "#3f1d6e")
INK <- "#0b0b0b"; MUTED <- "#52514e"; GRID <- "#e4e3df"
theme_paper <- function(base = 9) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
          axis.text = element_text(colour = MUTED), axis.title = element_text(colour = INK),
          strip.text = element_text(face = "bold", colour = INK, hjust = 0), legend.position = "none",
          plot.title = element_text(face = "bold", size = base + 1, colour = INK),
          plot.subtitle = element_text(colour = MUTED, size = base))
}
save_fig <- function(p, name, w, h) {
  ggsave(file.path(OUT, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
}
meth <- function(x, methods = METHODS) factor(x, levels = rev(methods), labels = LABELS[rev(methods)])

SIM_DIR <- file.path(EX, "simulated_test_15_365")
SIM_FILE <- file.path(SIM_DIR, "results_by_window.csv")
HAVE_SIM <- file.exists(SIM_FILE)
if (HAVE_SIM) {
  sim <- read.csv(SIM_FILE)
  sim <- sim[sim$done & !sim$failed & sim$method %in% SIM_METHODS, ]
  sim$position <- factor(sim$position, levels = c("early", "middle", "late"))
}
real <- read.csv(file.path(EX, "real_five/results_by_window.csv"))
real <- real[real$done & !real$failed & real$method %in% REAL_METHODS, ]
DATASETS <- c("Utah COVID-19 admissions", "Utah influenza admissions", "Baltimore influenza 1918",
              "Hong Kong SARS 2003", "Sierra Leone Ebola 2014-15")
real_main <- real[real$dataset %in% DATASETS, ]
real_main$dataset <- factor(real_main$dataset, levels = DATASETS)

# ---- Figure: coverage ridgelines -------------------------------------------------------------
ridge_data <- function(d, cols, labels, methods = METHODS) {
  do.call(rbind, lapply(seq_along(cols), function(k) data.frame(model = d$model, method = meth(d$method, methods),
                                                                 measure = labels[k], value = d[[cols[k]]])))
}
if (HAVE_SIM) {
r_sim <- ridge_data(sim, c("new_coverage", "infected_coverage", "future_coverage"),
                    c("New infections, in the window", "Infectious (not observed)", "New infections, next 14 days"))
r_sim$measure <- factor(r_sim$measure, levels = unique(r_sim$measure))
}
ridge_plot <- function(d, title, subtitle) {
  ggplot(d[is.finite(d$value), ], aes(x = value, y = method, fill = method, colour = method)) +
    geom_vline(xintercept = 95, linetype = "dashed", colour = MUTED, linewidth = 0.4) +
    geom_density_ridges(alpha = 0.55, scale = 1.6, linewidth = 0.4, from = 0, to = 100, bandwidth = 4,
                        quantile_lines = TRUE, quantiles = 2, rel_min_height = 0.005) +
    scale_fill_manual(values = setNames(COL, LABELS[names(COL)])) +
    scale_colour_manual(values = setNames(COL, LABELS[names(COL)])) +
    scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 95), expand = c(0, 0)) +
    labs(x = "Coverage of the 95% band (% of days)", y = NULL, title = title, subtitle = subtitle) +
    theme_paper()
}
p1 <- if (HAVE_SIM) ridge_plot(r_sim, "Simulated test windows (truth known)",
                 "Distribution over 5,000 windows per model; line: median; dashed: 95%") +
  facet_grid(model ~ measure) else NULL
r_real <- ridge_data(real_main, c("window_coverage", "future_coverage"), c("Observed counts, in the window", "Observed counts, next 14 days"),
                     REAL_METHODS)
r_real$measure <- factor(r_real$measure, levels = unique(r_real$measure))
p2 <- ridge_plot(r_real, "Five real datasets", "Distribution over the windows of the five datasets; line: median") +
  facet_grid(model ~ measure)
if (HAVE_SIM) save_fig(p1 / p2 + plot_layout(heights = c(1, 1.3)), "coverage_ridgelines", 7.2, 9)
save_fig(p2, "coverage_ridgelines_real", 7.2, 5)

# ---- Figure: coverage, MAE and WIS by method and scenario -----------------------------------------
# Adapted from a heatmap design. Rows: scenarios; columns: methods. (a) mean coverage (%), bold: closest
# to 95%. (b) mean absolute error and (c) mean weighted interval score, bold: lowest; shading by the
# value relative to the best method of the row.
# Scenarios in reading order: early, middle, late, or the datasets in the order of DATASETS.
scenario_factor <- function(x) {
  u <- unique(x); factor(x, levels = u[order(match(sub("^.*\\| ", "", u), c("early", "middle", "late", DATASETS)))])
}
heat_rows <- function(d, cov_col, mae_col, wis_col, methods) {
  do.call(rbind, lapply(split(d, scenario_factor(d$scenario), drop = TRUE), function(x) {
    mean_of <- function(col, m) if (is.null(col)) NA else mean(x[[col]][x$method == m], na.rm = TRUE)
    data.frame(scenario = x$scenario[1], section = x$section[1], method = methods,
               coverage = vapply(methods, function(m) mean_of(cov_col, m), 0),
               mae = vapply(methods, function(m) mean_of(mae_col, m), 0),
               wis = vapply(methods, function(m) mean_of(wis_col, m), 0))
  }))
}
# Short method names for the heat maps' axes ("w." = weighted by the data; 20% / 50% = kernel smoothing).
SHORT <- c(BiLSTM = "DeepIMC", `BiLSTM-k20` = "DeepIMC\n20%", `BiLSTM-k50` = "DeepIMC\n50%", `BiLSTM-weighted` = "DeepIMC\nweighted",
           `BiLSTM-k20-weighted` = "DeepIMC w.\n20%", `BiLSTM-k50-weighted` = "DeepIMC w.\n50%", `ABC-MCMC-summary` = "ABC-MCMC",
           `ABC-SMC-summary` = "ABC-SMC", DE = "DE", NelderMead = "Nelder-\nMead")
heat_figure <- function(blocks, name, title, h, mae_label, methods = METHODS, n_deepimc = 1, w = 7.6) {
  heat <- do.call(rbind, lapply(blocks, function(b) heat_rows(b$data, b$cov, b$mae, b$wis, methods)))
  lv <- unique(heat$scenario)
  heat$scenario <- factor(heat$scenario, levels = rev(lv))
  heat$section <- factor(heat$section, levels = unique(heat$section))
  heat$method <- factor(heat$method, levels = methods, labels = SHORT[methods])
  heat$best_cov <- ave(ifelse(is.na(heat$coverage), Inf, abs(heat$coverage - 95)), heat$scenario, FUN = function(z) z == min(z))
  low <- function(v) ave(v, heat$scenario, FUN = function(z) if (all(is.na(z))) z == Inf else z == min(z, na.rm = TRUE))
  rel <- function(v) v / ave(v, heat$scenario, FUN = function(z) if (all(is.na(z))) NA else min(z, na.rm = TRUE))
  heat$best_mae <- low(heat$mae) %in% TRUE; heat$rel_mae <- rel(heat$mae)
  heat$best_wis <- low(heat$wis) %in% TRUE; heat$rel_wis <- rel(heat$wis)
  fmt_v <- function(x) ifelse(is.na(x), "", ifelse(x >= 10, sprintf("%.0f", x), ifelse(x >= 1, sprintf("%.1f", x), sprintf("%.2f", x))))
  strip_label <- function(x) sub("^.*\\| ", "", x)
  base_heat <- function(p, ttl) p + geom_tile(colour = "white", linewidth = 0.6) +
    scale_y_discrete(labels = strip_label) +
    facet_grid(section ~ ., scales = "free_y", space = "free_y", switch = "y") +
    geom_vline(xintercept = n_deepimc + 0.5, colour = "#c2581a", linewidth = 0.8) +
    labs(x = NULL, y = NULL, title = ttl) +
    theme_paper(8) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, colour = INK, size = 6.5), panel.grid = element_blank(),
                           strip.placement = "outside", strip.text.y.left = element_text(angle = 0, face = "bold"))
  rel_panel <- function(fill, value, best, ttl) base_heat(ggplot(heat, aes(x = method, y = scenario, fill = pmin({{ fill }}, 4))), ttl) +
    geom_text(aes(label = fmt_v({{ value }}), fontface = ifelse({{ best }}, "bold", "plain"), colour = {{ fill }} > 2.5), size = 2.1) +
    scale_fill_gradient(low = "#fdf1e9", high = "#c2581a", limits = c(1, 4), na.value = "#f1f0ec", guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = INK), guide = "none", na.value = INK) +
    theme(axis.text.y = element_blank(), strip.text.y.left = element_blank(), strip.text = element_blank())
  pa <- base_heat(ggplot(heat, aes(x = method, y = scenario, fill = coverage)), "(a) Coverage (%)") +
    geom_text(aes(label = ifelse(is.na(coverage), "-", sprintf("%.0f", coverage)), fontface = ifelse(best_cov, "bold", "plain"),
                  colour = !is.na(coverage) & coverage > 80), size = 2.1) +
    scale_fill_gradient(low = "#eef4fc", high = "#1c5aa6", limits = c(0, 100), na.value = "#f1f0ec", guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = INK), guide = "none")
  pb <- rel_panel(rel_mae, mae, best_mae, paste0("(b) ", mae_label))
  pc <- rel_panel(rel_wis, wis, best_wis, "(c) WIS")
  save_fig(pa + pb + pc + plot_layout(widths = c(1.25, 1, 1)) + plot_annotation(title = title, theme = theme_paper()), name, w, h)
  invisible(heat)
}

# ---- Figure: DeepIMC against every other method, as ratios ----------------------------------------
# For each scenario and comparison method: DeepIMC's mean MAE, mean coverage and mean WIS divided by the
# method's, on the same windows, with a 95% bootstrap interval (resampling windows). MAE and WIS ratios
# below 1: DeepIMC's error is lower. Coverage ratio above 1: DeepIMC's band contains more of the days.
ratio_rows <- function(blocks, ref, others) {
  out <- list()
  for (b in blocks) for (x in split(b$data, scenario_factor(b$data$scenario), drop = TRUE)) {
    for (metric in c("MAE", "Coverage", "WIS")) {
      col <- switch(metric, MAE = b$mae, Coverage = b$cov, WIS = b$wis)
      if (is.null(col)) next
      r <- x[x$method == ref, c("id", col)]
      for (m in others) {
        mm <- merge(r, x[x$method == m, c("id", col)], by = "id")
        mm <- mm[is.finite(mm[[2]]) & is.finite(mm[[3]]), ]
        if (nrow(mm) < 3 || mean(mm[[3]]) == 0) next
        rat <- function(i) mean(mm[[2]][i]) / mean(mm[[3]][i])
        set.seed(1); bt <- replicate(1000, rat(sample(nrow(mm), replace = TRUE)))
        out[[length(out) + 1]] <- data.frame(scenario = x$scenario[1], section = x$section[1], metric = metric, method = m,
          ratio = rat(seq_len(nrow(mm))), lo = quantile(bt, 0.025, names = FALSE), hi = quantile(bt, 0.975, names = FALSE))
      }
    }
  }
  do.call(rbind, out)
}
ratio_figure <- function(blocks, ref, others, name, title, h, fine = TRUE) {
  d <- ratio_rows(blocks, ref, others)
  lv <- unique(d$scenario)
  d$scenario <- factor(d$scenario, levels = rev(lv)); d$section <- factor(d$section, levels = unique(d$section))
  d$metric <- factor(d$metric, levels = c("MAE", "Coverage", "WIS"),
                     labels = c("MAE ratio\n(below 1: DeepIMC better)", "Coverage ratio\n(above 1: DeepIMC covers more)",
                                "WIS ratio\n(below 1: DeepIMC better)"))
  d$method <- factor(d$method, levels = rev(others), labels = LABELS[rev(others)])
  brk <- if (fine) c(1/8, 1/4, 1/2, 0.8, 1, 1.25, 1.5, 2, 4, 8) else c(1/8, 1/4, 1/2, 1, 2, 4, 8)
  p <- ggplot(d, aes(x = ratio, y = scenario, colour = method)) +
    geom_vline(xintercept = 1, colour = MUTED, linewidth = 0.4) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", linewidth = 0.4, position = position_dodge(width = 0.75)) +
    geom_point(size = 1.2, position = position_dodge(width = 0.75)) +
    scale_colour_manual(values = setNames(COL[others], LABELS[others]), breaks = unname(LABELS[others]), name = NULL) +
    scale_x_log10(breaks = brk, labels = function(x) ifelse(x < 1 & abs(1 / x - round(1 / x)) < 1e-6, paste0("1/", round(1 / x)), format(x, drop0trailing = TRUE)),
                  minor_breaks = NULL) +
    scale_y_discrete(labels = function(x) sub("^.*\\| ", "", x)) +
    facet_grid(section ~ metric, scales = "free", space = "free_y", switch = "y") +
    labs(x = paste0(LABELS[[ref]], " / comparison method (log scale)"), y = NULL, title = title) +
    theme_paper(8) + theme(legend.position = "bottom", strip.placement = "outside",
                           strip.text.y.left = element_text(angle = 0, face = "bold"), panel.spacing.x = unit(0.8, "lines"),
                           plot.title.position = "plot")
  save_fig(p, name, 7.6, h)
  invisible(d)
}

if (HAVE_SIM) {
s2 <- sim; s2$scenario0 <- as.character(s2$position)
sec <- function(d, label, scen) transform(d, section = paste0(d$model, "\n", label), scenario = paste0(d$model, ", ", label, " | ", scen))
blocks_sim <- list(
  list(data = sec(s2, "R0", s2$scenario0), cov = "inside_pct", mae = "R0_error", wis = NULL),
  list(data = sec(s2, "new infections", s2$scenario0), cov = "new_coverage", mae = "new_MAE", wis = "new_WIS"),
  list(data = sec(s2, "infectious", s2$scenario0), cov = "infected_coverage", mae = "infected_MAE", wis = "infected_WIS"),
  list(data = sec(s2, "next 14 days", s2$scenario0), cov = "future_coverage", mae = "future_MAE", wis = "future_WIS"))
blocks_sim[[1]]$data$inside_pct <- 100 * blocks_sim[[1]]$data$inside
blocks_sim <- lapply(c("SEIR", "SIR"), function(M) lapply(blocks_sim, function(b) { b$data <- b$data[b$data$model == M, ]; b })) |>
  unlist(recursive = FALSE)
hs <- heat_figure(blocks_sim, "coverage_mae_simulated", "Simulated test windows: coverage, mean absolute error and WIS", 9.5, "MAE",
                  methods = SIM_METHODS[SIM_METHODS %in% sim$method], n_deepimc = sum(SIM_METHODS[1:2] %in% sim$method))
rs <- ratio_figure(blocks_sim, "BiLSTM", METHODS[-1], "ratios_simulated", "Simulated test windows: DeepIMC against each method", 10)
} else { hs <- NULL; rs <- NULL }
r2 <- real_main
secr <- function(d, label) transform(d, section = paste0(d$model, "\n", label), scenario = paste0(d$model, ", ", label, " | ", d$dataset))
blocks_real <- lapply(c("SEIR", "SIR"), function(M) list(
  list(data = secr(r2[r2$model == M, ], "in the window"), cov = "window_coverage", mae = "window_MAE", wis = "window_WIS"),
  list(data = secr(r2[r2$model == M, ], "next 14 days"), cov = "future_coverage", mae = "future_MAE", wis = "future_WIS"))) |>
  unlist(recursive = FALSE)
hr <- heat_figure(blocks_real, "coverage_mae_real", "Five real datasets: coverage, mean absolute error and WIS", 7.2,
                  "MAE (counts)", methods = REAL_METHODS[REAL_METHODS %in% real$method], n_deepimc = sum(grepl("^BiLSTM", REAL_METHODS[REAL_METHODS %in% real$method])),
                  w = 8.8)
rr0 <- ratio_figure(blocks_real, "BiLSTM", METHODS[-1], "ratios_real", "Five real datasets: DeepIMC against each method", 7.5, fine = FALSE)
rr20 <- ratio_figure(blocks_real, "BiLSTM-k20", METHODS[-1], "ratios_real_k20", "Five real datasets: DeepIMC (20% smoothing) against each method", 7.5, fine = FALSE)
rr50 <- ratio_figure(blocks_real, "BiLSTM-k50", METHODS[-1], "ratios_real_k50", "Five real datasets: DeepIMC (50% smoothing) against each method", 7.5, fine = FALSE)
rra <- if ("BiLSTM-weighted" %in% real$method) ratio_figure(blocks_real, "BiLSTM-weighted", METHODS[-1], "ratios_real_weighted",
  "Five real datasets: DeepIMC (weighted) against each method", 7.5, fine = FALSE) else NULL
rrw20 <- ratio_figure(blocks_real, "BiLSTM-k20-weighted", METHODS[-1], "ratios_real_weighted_k20",
  "Five real datasets: DeepIMC (20% smoothing, weighted) against each method", 7.5, fine = FALSE)
rrw50 <- ratio_figure(blocks_real, "BiLSTM-k50-weighted", METHODS[-1], "ratios_real_weighted_k50",
  "Five real datasets: DeepIMC (50% smoothing, weighted) against each method", 7.5, fine = FALSE)
saveRDS(list(sim = hs, real = hr, ratios_sim = rs, ratios_real = rr0, ratios_real_k20 = rr20, ratios_real_k50 = rr50, ratios_real_weighted = rra,
             ratios_real_weighted_k20 = rrw20, ratios_real_weighted_k50 = rrw50),
        file.path(OUT, "heatmap_values.rds"))

if (HAVE_SIM) {
# ---- Figure: recovery of the simulated epidemic curves ----------------------------------------
r_mae <- ridge_data(sim, c("new_MAE", "infected_MAE", "future_MAE"),
                    c("New infections, in the window", "Infectious (not observed)", "New infections, next 14 days"))
r_mae$measure <- factor(r_mae$measure, levels = unique(r_mae$measure))
r_mae <- r_mae[is.finite(r_mae$value) & r_mae$value > 0, ]
p_mae <- ggplot(r_mae, aes(x = value, y = method, fill = method, colour = method)) +
  geom_density_ridges(alpha = 0.55, scale = 1.6, linewidth = 0.4, quantile_lines = TRUE, quantiles = 2, rel_min_height = 0.005) +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), labels = c("0.1", "1", "10", "100", "1,000")) +
  scale_fill_manual(values = setNames(COL, LABELS[names(COL)])) + scale_colour_manual(values = setNames(COL, LABELS[names(COL)])) +
  facet_grid(model ~ measure) +
  labs(x = "MAE of the regenerated curve (median of 500 runs) vs the true curve (log scale)", y = NULL,
       title = "Recovery of the simulated epidemic curves", subtitle = "5,000 test windows per model; line: median") +
  theme_paper()
# Example windows: the true curve, the BiLSTM band and every method's median curve.
ex_ids <- local({
  w <- sim[sim$method == "BiLSTM" & sim$model == "SEIR" & sim$length >= 60, ]
  vapply(c("early", "middle", "late"), function(p) { x <- w[w$position == p, ]; x$id[order(abs(x$new_NRMSE - median(x$new_NRMSE)))][1] }, "")
})
win <- readRDS(file.path(SIM_DIR, "windows.rds"))
ex <- do.call(rbind, lapply(ex_ids, function(id) do.call(rbind, lapply(METHODS, function(m) {
  f <- file.path(SIM_DIR, "curves", sprintf("%s_%s.rds", id, m)); if (!file.exists(f)) return(NULL)
  b <- readRDS(f)$band$new; w <- win[[id]]
  data.frame(id = id, position = w$position, method = m, day = seq_len(nrow(b)), median = b$median, lower = b$lower,
             upper = b$upper, truth = c(w$y, w$future)[seq_len(nrow(b))], L = w$length)
}))))
ex$position <- factor(ex$position, levels = c("early", "middle", "late"), labels = c("Early window", "Middle window", "Late window"))
ex$method <- factor(ex$method, levels = METHODS)
p_ex <- ggplot(ex, aes(x = day)) +
  geom_ribbon(data = ex[ex$method == "BiLSTM", ], aes(ymin = lower, ymax = upper), fill = COL[["BiLSTM"]], alpha = 0.2) +
  geom_line(aes(y = median, colour = method), linewidth = 0.55) +
  geom_point(data = ex[ex$method == "BiLSTM", ], aes(y = truth), size = 0.55, colour = INK) +
  geom_vline(data = unique(ex[, c("position", "L")]), aes(xintercept = L + 0.5), linetype = "dashed", colour = MUTED, linewidth = 0.4) +
  scale_colour_manual(values = COL, labels = LABELS, name = NULL) +
  facet_wrap(~ position, scales = "free") +
  labs(x = "Day of the window (dashed: end of the window; then 14 days ahead)", y = "Daily new infections",
       title = "Example SEIR windows", subtitle = "points: true curve; band: DeepIMC 95%; lines: median regenerated curve of each method") +
  theme_paper() + theme(legend.position = "bottom")
save_fig(p_ex / p_mae + plot_layout(heights = c(1, 1.6)), "curve_recovery", 7.2, 7.5)

# ---- Figure: parameter recovery (R0, ptran, crate) --------------------------------------------
pr <- do.call(rbind, lapply(c("R0", "ptran", "crate"), function(v) data.frame(model = sim$model, method = factor(sim$method, levels = METHODS, labels = LABELS[METHODS]),
  parameter = v, truth = sim[[paste0(v, "_true")]], estimate = sim[[v]])))
pr$parameter <- factor(pr$parameter, levels = c("R0", "ptran", "crate"), labels = c("R[0]", "p[tran]", "c[rate]"))

# One row per model and parameter, each with its own scale (the same for both axes, so the dashed
# line of perfect agreement is the diagonal).
PAR_LAB <- c(R0 = "R[0]", ptran = "p[tran]", crate = "c[rate]")
mk <- function(M, v, first) {
  d <- pr[pr$model == M & pr$parameter == PAR_LAB[[v]], ]
  lim <- range(c(d$truth, d$estimate), na.rm = TRUE)
  ggplot(d, aes(truth, estimate, colour = method)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = MUTED, linewidth = 0.4) +
    geom_point(size = 0.2, alpha = 0.2) + scale_colour_manual(values = setNames(COL, LABELS[names(COL)]), guide = "none") +
    coord_cartesian(xlim = lim, ylim = lim) + facet_grid(. ~ method) +
    labs(x = NULL, y = parse(text = paste0("'", M, "'~", PAR_LAB[[v]]))) + theme_paper(8) +
    theme(strip.text = if (first) element_text(face = "bold") else element_blank(), panel.spacing.x = unit(0.6, "lines"))
}
rows <- unlist(lapply(c("SEIR", "SIR"), function(M) lapply(names(PAR_LAB), function(v) mk(M, v, M == "SEIR" && v == "R0"))), recursive = FALSE)
save_fig(wrap_plots(rows, ncol = 1) + plot_annotation(title = "Estimates against true values (5,000 test windows per model)",
                                                      caption = "x: true value; y: estimate; dashed: perfect agreement", theme = theme_paper()),
         "parameter_recovery", 7.2, 9)

}

# ---- Figure: real-data examples (every method) ------------------------------------------------
# The window around the peak of each dataset (60 days, or the longest shorter one where the outbreak is
# shorter): rows are datasets, columns methods.
rw <- readRDS(file.path(EX, "real_five/windows.rds"))
# The versions of DeepIMC that read smoothed counts, and the Gaussian kernel they used (as smooth_counts() in
# examples/methods_common.R: bandwidth = fraction x window length).
SMOOTH_FRAC <- c(`BiLSTM-k20` = 0.2, `BiLSTM-k50` = 0.5, `BiLSTM-k20-weighted` = 0.2, `BiLSTM-k50-weighted` = 0.5)
smooth_counts <- function(y, frac) { x <- seq_along(y); stats::ksmooth(x, y, kernel = "normal", bandwidth = frac * length(y), x.points = x)$y }
MAIN_LABELS <- c("Utah COVID-19\nadmissions", "Utah influenza\nadmissions", "Baltimore\ninfluenza 1918", "Hong Kong\nSARS 2003",
                 "Sierra Leone\nEbola 2014-15")
real_examples <- function(M, name, datasets = DATASETS, labels = MAIN_LABELS, methods = REAL_METHODS, h = 7.2, w = 8.2,
                          xlab = "Day of the 60-day window around the peak (dashed: end of the window; then the next 14 days)",
                          title = paste0("Real data: ", M, " fits of every method around the peak of each dataset")) {
  pick <- vapply(datasets, function(ds) {
    x <- real[real$model == M & real$dataset == ds & real$length <= 60 & real$position == "middle" & real$method == "BiLSTM", ]
    x$id[x$length == max(x$length)][1] }, "")
  rex <- do.call(rbind, lapply(pick[!is.na(pick)], function(id) do.call(rbind, lapply(methods, function(m) {
    f <- file.path(EX, "real_five/curves", sprintf("%s_%s.rds", id, m)); if (!file.exists(f)) return(NULL)
    b <- readRDS(f)$band; w <- rw[[id]]
    data.frame(dataset = w$dataset, method = m, day = seq_len(nrow(b)), median = b$median, lower = b$lower, upper = b$upper,
               observed = c(w$y, w$future)[seq_len(nrow(b))], L = w$length)
  }))))
  # What the smoothed versions read: the smoothed counts of the window. The points stay the raw counts, against
  # which every version is scored (grey in the smoothed columns).
  sm <- do.call(rbind, lapply(pick[!is.na(pick)], function(id) { w <- rw[[id]]
    do.call(rbind, lapply(intersect(methods, names(SMOOTH_FRAC)), function(m)
      data.frame(dataset = w$dataset, method = m, day = seq_len(w$length), smoothed = smooth_counts(w$y, SMOOTH_FRAC[[m]])))) }))
  rex$read_smoothed <- rex$method %in% names(SMOOTH_FRAC)
  as_facets <- function(d) { d$dataset <- factor(d$dataset, levels = datasets, labels = labels)
                             d$method <- factor(d$method, levels = methods, labels = gsub(", ", ",\n", LABELS[methods])); d }
  rex <- as_facets(rex); if (!is.null(sm)) sm <- as_facets(sm)
  cap <- ave(rex$observed, rex$dataset, FUN = function(z) 1.6 * max(z, na.rm = TRUE))
  rex$upper <- pmin(rex$upper, cap); rex$median <- pmin(rex$median, cap); rex$lower <- pmin(rex$lower, cap)
  p <- ggplot(rex, aes(x = day)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = method), alpha = 0.25) +
    geom_line(aes(y = median, colour = method), linewidth = 0.5) +
    geom_point(data = rex[!rex$read_smoothed, ], aes(y = observed), size = 0.35, colour = INK) +
    geom_point(data = rex[rex$read_smoothed, ], aes(y = observed), size = 0.35, colour = "#a3a19b") +
    { if (!is.null(sm)) geom_line(data = sm, aes(y = smoothed), colour = INK, linewidth = 0.6) } +
    geom_vline(data = unique(rex[, c("dataset", "L")]), aes(xintercept = L + 0.5), linetype = "dashed", colour = MUTED, linewidth = 0.35) +
    geom_blank(data = unique(data.frame(dataset = rex$dataset, day = 1, cap = cap)), aes(y = cap)) +
    scale_colour_manual(values = setNames(COL, gsub(", ", ",\n", LABELS[names(COL)]))) +
    scale_fill_manual(values = setNames(COL, gsub(", ", ",\n", LABELS[names(COL)]))) +
    facet_grid(dataset ~ method, scales = "free_y") +
    labs(x = xlab, y = "Daily count", title = title,
         subtitle = paste0("points: observed counts (all methods are scored against these); black line, in the smoothing columns: ",
                           "the smoothed counts DeepIMC read\ncoloured line and band: median and 95% band of 500 model runs ",
                           "(cut at 1.6 times the largest count)")) +
    theme_paper(8) + theme(strip.text.y = element_text(angle = 0, hjust = 0))
  save_fig(p, name, w, h)
}
# The main figure (a landscape page): the six versions of DeepIMC (raw, 20% and 50% smoothing, each without and
# with weighting) and the comparison methods.
real_examples("SEIR", "real_examples", methods = REAL_METHODS, w = 11)
real_examples("SIR", "real_examples_sir", methods = REAL_METHODS, w = 11)
# The six versions of DeepIMC (raw, 20% and 50% smoothing, each without and with weighting), with differential
# evolution for reference.
for (M in c("SEIR", "SIR"))
  real_examples(M, paste0("real_examples_deepimc", if (M == "SIR") "_sir" else ""), methods = c(DEEPIMC6, "DE"),
                title = paste0("Real data: the six versions of DeepIMC (", M, "), with differential evolution for reference"))

# Six more outbreaks (a separate check), with Hong Kong SARS for comparison.
EXTRA <- c("Hong Kong SARS 2003", "Toronto SARS 2003", "South Korea MERS 2015", "Kosovo smallpox 1972",
           "Pennsylvania school influenza 2009", "Hagelloch measles 1861", "Kikwit Ebola 1995")
EXTRA_LABELS <- c("Hong Kong\nSARS 2003", "Canada\nSARS 2003", "South Korea\nMERS 2015", "Kosovo\nsmallpox 1972",
                  "Pennsylvania\ninfluenza 2009", "Hagelloch\nmeasles 1861", "Kikwit\nEbola 1995")
EXTRA_METHODS <- c("BiLSTM", "BiLSTM-weighted", "BiLSTM-k20-weighted", "BiLSTM-k50-weighted", METHODS[-1])
if (all(EXTRA %in% real$dataset)) {
  real_examples("SEIR", "real_extra_examples", EXTRA, EXTRA_LABELS, EXTRA_METHODS, h = 9.2,
                xlab = "Day of the window around the peak (60 days, or 30 where the outbreak is shorter; dashed: end of the window; then the next 14 days)",
                title = "Six more outbreaks and Hong Kong SARS: SEIR fits of every method around the peak")
  # The daily counts of each outbreak (rebuilt from its windows), with Hong Kong's largest one-day jump marked.
  sr <- do.call(rbind, lapply(EXTRA, function(ds) {
    ws <- rw[vapply(rw, function(w) w$dataset == ds && w$model == "SEIR", logical(1))]
    d <- do.call(rbind, lapply(ws, function(w) data.frame(t = as.numeric(w$time), y = w$y)))
    d <- d[!duplicated(d$t), ]; d <- d[order(d$t), ]
    data.frame(dataset = ds, day = d$t - min(d$t) + 1, y = d$y)
  }))
  sr$dataset <- factor(sr$dataset, levels = EXTRA, labels = sub("\n", " ", EXTRA_LABELS))
  hk <- sr[sr$dataset == levels(sr$dataset)[1], ]; J <- hk$day[which.max(diff(hk$y)) + 1]
  p <- ggplot(sr, aes(day, y)) + geom_col(width = 1, fill = COL[["BiLSTM"]], colour = NA) +
    geom_vline(data = data.frame(dataset = factor(levels(sr$dataset)[1], levels = levels(sr$dataset)), day = J - 0.5),
               aes(xintercept = day), linetype = "dashed", colour = COL[["ABC-MCMC-summary"]], linewidth = 0.4) +
    facet_wrap(~ dataset, scales = "free", ncol = 4) +
    labs(x = "Day of the outbreak", y = "Daily count", title = "Daily counts of the six more outbreaks and of Hong Kong SARS",
         subtitle = sprintf("Hong Kong SARS: the counts jump from %d to %d on day %d (dashed)", hk$y[hk$day == J - 1], hk$y[hk$day == J], J)) +
    theme_paper(8)
  save_fig(p, "real_extra_series", 7.4, 3.6)
}

# Every window of one wave of each dataset (SEIR): rows are window lengths, columns positions; the
# observed counts, DeepIMC's 95% band and the median curve of every method.
real_windows <- function(ds, wave, name) {
  ids <- names(rw)[vapply(rw, function(w) w$model == "SEIR" && w$dataset == ds && w$wave == wave, logical(1))]
  d <- do.call(rbind, lapply(ids, function(id) do.call(rbind, lapply(REAL_METHODS, function(m) {
    f <- file.path(EX, "real_five/curves", sprintf("%s_%s.rds", id, m)); if (!file.exists(f)) return(NULL)
    s <- readRDS(f); if (isTRUE(s$failed)) return(NULL); b <- s$band; w <- rw[[id]]
    data.frame(length = w$length, position = w$position, method = m, day = seq_len(nrow(b)), median = b$median,
               lower = b$lower, upper = b$upper, observed = c(w$y, w$future)[seq_len(nrow(b))], L = w$length)
  }))))
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  d$length <- factor(paste(d$length, "days"), levels = paste(sort(unique(d$length)), "days"))
  d$position <- factor(d$position, levels = c("beginning", "middle", "end"))
  d$method <- factor(d$method, levels = rev(REAL_METHODS), labels = LABELS[rev(REAL_METHODS)])
  cap <- 1.6 * max(d$observed, na.rm = TRUE)
  d$upper <- pmin(d$upper, cap); d$median <- pmin(d$median, cap); d$lower <- pmin(d$lower, cap)
  p <- ggplot(d, aes(x = day)) +
    geom_ribbon(data = d[d$method == "DeepIMC", ], aes(ymin = lower, ymax = upper), fill = COL[["BiLSTM"]], alpha = 0.2) +
    geom_point(data = d[d$method == "DeepIMC", ], aes(y = observed), size = 0.3, colour = INK) +
    geom_line(aes(y = median, colour = method), linewidth = 0.45) +
    geom_vline(data = unique(d[, c("length", "position", "L")]), aes(xintercept = L + 0.5), linetype = "dashed",
               colour = MUTED, linewidth = 0.3) +
    scale_colour_manual(values = setNames(COL, LABELS[names(COL)]), breaks = unname(LABELS[REAL_METHODS]), name = NULL) +
    facet_grid(length ~ position, scales = "free_x") + coord_cartesian(ylim = c(0, cap)) +
    labs(x = "Day of the window (dashed: end of the window; then the next 14 days)", y = "Daily count",
         title = paste0(ds, " (", wave, "): SEIR fits of every method to every window"),
         subtitle = "points: observed counts; band: DeepIMC 95%; lines: median regenerated curve of each method") +
    theme_paper(8) + theme(legend.position = "bottom", strip.text.y = element_text(angle = 0))
  save_fig(p, name, 7.4, 1.4 * nlevels(d$length) + 1.6)
}
WAVES <- list(c("Utah COVID-19 admissions", "Delta 2021", "real_windows_covid"),
              c("Utah influenza admissions", "2022-23", "real_windows_flu"),
              c("Baltimore influenza 1918", "1918", "real_windows_baltimore"),
              c("Hong Kong SARS 2003", "2003", "real_windows_sars"),
              c("Sierra Leone Ebola 2014-15", "2014-15", "real_windows_ebola"))
for (x in WAVES) real_windows(x[1], x[2], x[3])

# ---- Figures: accuracy by window length and position -------------------------------------------
POS_COL <- c(beginning = "#2a78d6", early = "#2a78d6", middle = "#1baf7a", end = "#eb6834", late = "#eb6834")
# DeepIMC alone, windows of 15 to 365 days at the beginning, middle and end of 1,000 test epidemics per model.
bl <- do.call(rbind, lapply(c("SEIR", "SIR"), function(M) {
  d <- file.path(EX, paste0(tolower(M), "_clean_w15"))
  a <- read.csv(file.path(d, "R0_MAE_by_length_and_position.csv")); b <- read.csv(file.path(d, "coverage_by_length_and_position.csv"))
  m <- merge(a, b, by = c("length", "position"))
  rbind(data.frame(model = M, length = m$length, position = m$position, measure = "R0 MAE", value = m$R0_abs_error),
        data.frame(model = M, length = m$length, position = m$position, measure = "Coverage, new infections (%)", value = m$new_infections),
        data.frame(model = M, length = m$length, position = m$position, measure = "Coverage, infectious (%)", value = m$currently_infected))
}))
bl$measure <- factor(bl$measure, levels = c("R0 MAE", "Coverage, new infections (%)", "Coverage, infectious (%)"),
                     labels = c("R[0]~MAE", "Coverage~of~new~infections~('%')", "Coverage~of~infectious~('%')"))
bl$position <- factor(bl$position, levels = c("beginning", "middle", "end"))
p_bl <- ggplot(bl, aes(length, value, colour = position)) +
  geom_hline(data = data.frame(measure = factor(levels(bl$measure)[2:3], levels = levels(bl$measure)), y = 95), aes(yintercept = y),
             linetype = "dashed", colour = MUTED, linewidth = 0.35) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
  scale_colour_manual(values = POS_COL, name = "Window starts at the") +
  scale_x_continuous(breaks = c(15, 61, 90, 120, 180, 270, 365)) +
  facet_grid(measure ~ model, scales = "free_y", labeller = labeller(measure = label_parsed)) +
  labs(x = "Window length (days)", y = NULL, title = "DeepIMC by window length and position",
       subtitle = "1,000 test epidemics per model (coverage: 100 per length and position); dashed: 95%") +
  theme_paper() + theme(legend.position = "bottom")
save_fig(p_bl, "deepimc_by_window", 7.2, 6.4)

if (HAVE_SIM) {
# All methods on the 10,000 simulated windows, by window length and position.
LENGTH_GROUPS <- c(15, 30, 60, 120, 180, 366)
LENGTH_LABELS <- c("15-29", "30-59", "60-119", "120-179", "180-365")
sim$length_group <- findInterval(sim$length, LENGTH_GROUPS)
by_win <- function(col, label, fun = mean) {
  a <- aggregate(sim[[col]], list(model = sim$model, position = sim$position, length = sim$length_group, method = sim$method),
                 function(z) fun(z, na.rm = TRUE))
  data.frame(a[, 1:4], measure = label, value = a$x)
}
sw <- rbind(by_win("R0_error", "R0 MAE"), by_win("new_coverage", "Coverage,\nnew infections (%)"),
            by_win("new_MAE", "MAE,\nnew infections"), by_win("new_WIS", "WIS,\nnew infections"),
            by_win("infected_MAE", "MAE, infectious\n(not observed)"))
sw$measure <- factor(sw$measure, levels = unique(sw$measure))
sw$method <- factor(sw$method, levels = METHODS, labels = LABELS[METHODS])
sw$position <- factor(sw$position, levels = c("early", "middle", "late"))
p_sw <- ggplot(sw, aes(length, value, colour = method)) +
  geom_hline(data = data.frame(measure = factor("Coverage,\nnew infections (%)", levels = levels(sw$measure)), y = 95),
             aes(yintercept = y), linetype = "dashed", colour = MUTED, linewidth = 0.35) +
  geom_line(linewidth = 0.55) + geom_point(size = 1.1) +
  scale_colour_manual(values = setNames(COL, LABELS[names(COL)]), name = NULL) +
  scale_x_continuous(breaks = seq_along(LENGTH_LABELS), labels = LENGTH_LABELS) +
  facet_grid(measure ~ model + position, scales = "free_y") +
  labs(x = "Window length (days)", y = NULL, title = "All methods by window length and position (10,000 simulated windows)",
       subtitle = "Mean over the windows of each group; dashed: 95%") +
  theme_paper(8) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p_sw, "methods_by_window_simulated", 7.6, 8.6)
}

# All methods on the real data, by window length and position. MAE and WIS are divided by the lowest
# value of the methods on the same window, so that datasets of different size can be averaged.
rr <- real_main
for (part in c("window", "future")) for (sc in c("MAE", "WIS")) {
  best <- ave(rr[[paste0(part, "_", sc)]], rr$id, FUN = function(z) min(z, na.rm = TRUE))
  rr[[paste0(part, "_rel", sc)]] <- rr[[paste0(part, "_", sc)]] / best
}
by_real <- function(col, label) {
  a <- aggregate(rr[[col]], list(model = rr$model, position = rr$position, length = rr$length, method = rr$method),
                 function(z) mean(z[is.finite(z)]))
  data.frame(a[, 1:4], measure = label, value = a$x)
}
rwin <- rbind(by_real("window_coverage", "Coverage,\nwindow (%)"), by_real("window_relMAE", "Relative MAE,\nwindow"),
              by_real("window_relWIS", "Relative WIS,\nwindow"),
              by_real("future_coverage", "Coverage,\nnext 14 days (%)"), by_real("future_relMAE", "Relative MAE,\nnext 14 days"),
              by_real("future_relWIS", "Relative WIS,\nnext 14 days"))
rwin$measure <- factor(rwin$measure, levels = unique(rwin$measure))
rwin$method <- factor(rwin$method, levels = REAL_METHODS, labels = LABELS[REAL_METHODS])
rwin$position <- factor(rwin$position, levels = c("beginning", "middle", "end"))
p_rw <- ggplot(rwin, aes(length, value, colour = method)) +
  geom_hline(data = data.frame(measure = factor(c("Coverage,\nwindow (%)", "Coverage,\nnext 14 days (%)"), levels = levels(rwin$measure)), y = 95),
             aes(yintercept = y), linetype = "dashed", colour = MUTED, linewidth = 0.35) +
  geom_hline(data = data.frame(measure = factor(c("Relative MAE,\nwindow", "Relative WIS,\nwindow", "Relative MAE,\nnext 14 days",
                                                  "Relative WIS,\nnext 14 days"), levels = levels(rwin$measure)), y = 1),
             aes(yintercept = y), colour = MUTED, linewidth = 0.35) +
  geom_line(linewidth = 0.55) + geom_point(size = 1.2) +
  scale_colour_manual(values = setNames(COL, LABELS[names(COL)]), name = NULL) +
  scale_x_continuous(breaks = c(15, 30, 60, 90, 120, 180), trans = "log10") +
  facet_grid(measure ~ model + position, scales = "free_y") +
  labs(x = "Window length (days)", y = NULL, title = "All methods by window length and position (five real datasets)",
       subtitle = "Mean over datasets and waves; relative MAE or WIS = value / lowest value of the methods on the same window (1 = best)") +
  theme_paper(8) + theme(legend.position = "bottom")
save_fig(p_rw, "methods_by_window_real", 7.6, 9)

cat("figures written to", normalizePath(OUT), "\n")
