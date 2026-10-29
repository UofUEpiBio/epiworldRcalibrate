# Figures of the paper (ggplot2, ggridges, patchwork), from the saved results:
#   examples/results/simulated_test_10k/results_by_window.csv   (10,000 simulated windows)
#   examples/results/real_five/results_by_window.csv        (five real datasets)
# Run from the paper/ folder:  Rscript figures/make_figures.R
suppressPackageStartupMessages({ library(ggplot2); library(ggridges); library(patchwork) })
EX <- "../examples/results"
OUT <- "figures"
# ABC-MCMC and ABC-SMC are the summary-statistics versions (named simply ABC-MCMC and ABC-SMC in the paper).
METHODS <- c("BiLSTM", "ABC-MCMC-summary", "ABC-SMC-summary", "DE", "NelderMead")
LABELS <- c(BiLSTM = "DeepIMC", `ABC-MCMC-summary` = "ABC-MCMC", `ABC-SMC-summary` = "ABC-SMC", DE = "DE", NelderMead = "Nelder-Mead")
# Validated categorical palette (fixed order; the method name is always on the axis, so colour is
# never the only cue).
COL <- c(BiLSTM = "#2a78d6", `ABC-MCMC-summary` = "#eb6834", `ABC-SMC-summary` = "#1baf7a", DE = "#eda100", NelderMead = "#e87ba4")
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
meth <- function(x) factor(x, levels = rev(METHODS), labels = LABELS[rev(METHODS)])

SIM_FILE <- file.path(EX, "simulated_test_10k/results_by_window.csv")
HAVE_SIM <- file.exists(SIM_FILE)
if (HAVE_SIM) {
  sim <- read.csv(SIM_FILE)
  sim <- sim[sim$done & !sim$failed, ]
  sim$position <- factor(sim$position, levels = c("early", "middle", "late"))
}
real <- read.csv(file.path(EX, "real_five/results_by_window.csv"))
real <- real[real$done & !real$failed, ]
DATASETS <- c("Utah COVID-19 admissions", "Utah influenza admissions", "Baltimore influenza 1918",
              "Hong Kong SARS 2003", "Sierra Leone Ebola 2014-15")
real_main <- real[real$dataset %in% DATASETS, ]
real_main$dataset <- factor(real_main$dataset, levels = DATASETS)

# ---- Figure: coverage ridgelines -------------------------------------------------------------
ridge_data <- function(d, cols, labels) {
  do.call(rbind, lapply(seq_along(cols), function(k) data.frame(model = d$model, method = meth(d$method),
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
r_real <- ridge_data(real_main, c("window_coverage", "future_coverage"), c("Observed counts, in the window", "Observed counts, next 14 days"))
r_real$measure <- factor(r_real$measure, levels = unique(r_real$measure))
p2 <- ridge_plot(r_real, "Five real datasets", "Distribution over the windows of the five datasets; line: median") +
  facet_grid(model ~ measure)
if (HAVE_SIM) save_fig(p1 / p2 + plot_layout(heights = c(1, 1)), "coverage_ridgelines", 7.2, 8)
save_fig(p2, "coverage_ridgelines_real", 7.2, 4.2)

# ---- Figure: coverage and MAE by method and scenario, with a paired MAE comparison ----------------
# Adapted from a heatmap-plus-forest design. Rows: scenarios; columns: methods.
# (a) mean coverage (%), bold: closest to 95%. (b) mean absolute error, bold: lowest; shading by
# the error relative to the best method of the row. (c) MAE of DeepIMC divided by that of the best
# comparison method of the row, with a paired bootstrap 95% interval (resampling windows).
# Scenarios in reading order: early, middle, late, or the datasets in the order of DATASETS.
scenario_factor <- function(x) {
  u <- unique(x); factor(x, levels = u[order(match(sub("^.*\\| ", "", u), c("early", "middle", "late", DATASETS)))])
}
heat_rows <- function(d, cov_col, mae_col) {
  do.call(rbind, lapply(split(d, scenario_factor(d$scenario), drop = TRUE), function(x) {
    mean_of <- function(col, m) if (is.null(col)) NA else mean(x[[col]][x$method == m], na.rm = TRUE)
    data.frame(scenario = x$scenario[1], section = x$section[1], method = METHODS,
               coverage = vapply(METHODS, function(m) mean_of(cov_col, m), 0),
               mae = vapply(METHODS, function(m) mean_of(mae_col, m), 0))
  }))
}
paired_ratio <- function(d, col, best) {
  b <- d[d$method == "BiLSTM", c("id", col)]; o <- d[d$method == best, c("id", col)]
  m <- merge(b, o, by = "id", suffixes = c("_b", "_o")); m <- m[is.finite(m[[2]]) & is.finite(m[[3]]), ]
  if (nrow(m) < 3) return(c(NA, NA, NA))
  ratio <- function(i) mean(m[[2]][i]) / mean(m[[3]][i])
  set.seed(1); bt <- replicate(2000, ratio(sample(nrow(m), replace = TRUE)))
  c(ratio(seq_len(nrow(m))), quantile(bt, c(0.025, 0.975), names = FALSE))
}
heat_figure <- function(blocks, name, title, h, mae_label) {
  heat <- list(); pair <- list()
  for (b in blocks) {
    heat[[length(heat) + 1]] <- heat_rows(b$data, b$cov, b$mae)
    pair[[length(pair) + 1]] <- do.call(rbind, lapply(split(b$data, scenario_factor(b$data$scenario), drop = TRUE), function(x) {
      sc <- vapply(METHODS[-1], function(m) mean(x[[b$mae]][x$method == m], na.rm = TRUE), 0)
      best <- names(which.min(sc)); r <- paired_ratio(x, b$mae, best)
      data.frame(scenario = x$scenario[1], section = x$section[1], best = best, ratio = r[1], lo = r[2], hi = r[3])
    }))
  }
  heat <- do.call(rbind, heat); pair <- do.call(rbind, pair)
  lv <- unique(heat$scenario)
  heat$scenario <- factor(heat$scenario, levels = rev(lv)); pair$scenario <- factor(pair$scenario, levels = rev(lv))
  heat$section <- factor(heat$section, levels = unique(heat$section)); pair$section <- factor(pair$section, levels = levels(heat$section))
  heat$method <- factor(heat$method, levels = METHODS, labels = LABELS[METHODS])
  heat$best_cov <- ave(ifelse(is.na(heat$coverage), Inf, abs(heat$coverage - 95)), heat$scenario, FUN = function(z) z == min(z))
  heat$best_mae <- ave(heat$mae, heat$scenario, FUN = function(z) z == min(z, na.rm = TRUE))
  heat$rel_mae <- heat$mae / ave(heat$mae, heat$scenario, FUN = function(z) min(z, na.rm = TRUE))
  fmt_mae <- function(x) ifelse(is.na(x), "", ifelse(x >= 100, sprintf("%.0f", x), ifelse(x >= 1, sprintf("%.1f", x), sprintf("%.2f", x))))
  strip_label <- function(x) sub("^.*\\| ", "", x)
  base_heat <- function(p, ttl) p + geom_tile(colour = "white", linewidth = 0.6) +
    scale_y_discrete(labels = strip_label) +
    facet_grid(section ~ ., scales = "free_y", space = "free_y", switch = "y") +
    geom_vline(xintercept = 1.5, colour = "#c2581a", linewidth = 0.8) +
    labs(x = NULL, y = NULL, title = ttl) +
    theme_paper(8) + theme(axis.text.x = element_text(angle = 35, hjust = 1, colour = INK), panel.grid = element_blank(),
                           strip.placement = "outside", strip.text.y.left = element_text(angle = 0, face = "bold"))
  pa <- base_heat(ggplot(heat, aes(x = method, y = scenario, fill = coverage)), "(a) Coverage (%)") +
    geom_text(aes(label = ifelse(is.na(coverage), "-", sprintf("%.0f", coverage)), fontface = ifelse(best_cov, "bold", "plain"),
                  colour = !is.na(coverage) & coverage > 80), size = 2.3) +
    scale_fill_gradient(low = "#eef4fc", high = "#1c5aa6", limits = c(0, 100), na.value = "#f1f0ec", guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = INK), guide = "none")
  pb <- base_heat(ggplot(heat, aes(x = method, y = scenario, fill = pmin(rel_mae, 4))), paste0("(b) ", mae_label)) +
    geom_text(aes(label = fmt_mae(mae), fontface = ifelse(best_mae, "bold", "plain"), colour = rel_mae > 2.5), size = 2.3) +
    scale_fill_gradient(low = "#fdf1e9", high = "#c2581a", limits = c(1, 4), na.value = "#f1f0ec", guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = INK), guide = "none") +
    theme(axis.text.y = element_blank(), strip.text.y.left = element_blank(), strip.text = element_blank())
  pc <- ggplot(pair, aes(x = ratio, y = scenario)) +
    geom_vline(xintercept = 1, colour = MUTED, linewidth = 0.4) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", colour = MUTED, linewidth = 0.5) +
    geom_point(aes(fill = ratio < 1), shape = 21, size = 2, colour = "white", stroke = 0.5) +
    scale_fill_manual(values = c(`TRUE` = COL[["BiLSTM"]], `FALSE` = "#e34948"), guide = "none") +
    scale_x_log10(breaks = c(1/8, 1/4, 1/2, 1, 2, 4, 8), labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
                  minor_breaks = NULL) + expand_limits(x = c(0.25, 1.2)) +
    facet_grid(section ~ ., scales = "free_y", space = "free_y") +
    labs(x = "DeepIMC / best\nother (log)", y = NULL, title = "(c) MAE ratio") +
    theme_paper(8) + theme(axis.text.y = element_blank(), strip.text = element_blank(), plot.margin = margin(5.5, 12, 5.5, 5.5))
  save_fig(pa + pb + pc + plot_layout(widths = c(1.35, 1.35, 0.8)) + plot_annotation(title = title, theme = theme_paper()), name, 7.4, h)
  invisible(list(heat = heat, pair = pair))
}
if (HAVE_SIM) {
s2 <- sim; s2$scenario0 <- as.character(s2$position)
sec <- function(d, label, scen) transform(d, section = paste0(d$model, "\n", label), scenario = paste0(d$model, ", ", label, " | ", scen))
blocks_sim <- list(
  list(data = sec(s2, "R0", s2$scenario0), cov = NULL, mae = "R0_error"),
  list(data = sec(s2, "new infections", s2$scenario0), cov = "new_coverage", mae = "new_MAE"),
  list(data = sec(s2, "infectious", s2$scenario0), cov = "infected_coverage", mae = "infected_MAE"),
  list(data = sec(s2, "next 14 days", s2$scenario0), cov = "future_coverage", mae = "future_MAE"))
blocks_sim[[1]]$data$inside_pct <- 100 * blocks_sim[[1]]$data$inside
blocks_sim[[1]]$cov <- "inside_pct"
blocks_sim <- c(blocks_sim[1:4])
hs <- heat_figure(lapply(c("SEIR", "SIR"), function(M) lapply(blocks_sim, function(b) { b$data <- b$data[b$data$model == M, ]; b })) |> unlist(recursive = FALSE),
                  "coverage_mae_simulated", "Simulated test windows: coverage and mean absolute error", 9.5, "MAE")
} else hs <- NULL
r2 <- real_main
secr <- function(d, label) transform(d, section = paste0(d$model, "\n", label), scenario = paste0(d$model, ", ", label, " | ", d$dataset))
blocks_real <- lapply(c("SEIR", "SIR"), function(M) list(
  list(data = secr(r2[r2$model == M, ], "in the window"), cov = "window_coverage", mae = "window_MAE"),
  list(data = secr(r2[r2$model == M, ], "next 14 days"), cov = "future_coverage", mae = "future_MAE"))) |> unlist(recursive = FALSE)
hr <- heat_figure(blocks_real, "coverage_mae_real", "Five real datasets: coverage and mean absolute error", 6.8, "MAE (counts)")
saveRDS(list(sim = hs, real = hr), file.path(OUT, "heatmap_values.rds"))

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
win <- readRDS(file.path(EX, "simulated_test_10k/windows.rds"))
ex <- do.call(rbind, lapply(ex_ids, function(id) do.call(rbind, lapply(METHODS, function(m) {
  f <- file.path(EX, "simulated_test_10k/curves", sprintf("%s_%s.rds", id, m)); if (!file.exists(f)) return(NULL)
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
# The 60-day window around the peak of each dataset: rows are methods, columns datasets.
rw <- readRDS(file.path(EX, "real_five/windows.rds"))
real_examples <- function(M, name) {
  pick <- vapply(DATASETS, function(ds) {
    x <- real_main[real_main$model == M & real_main$dataset == ds & real_main$length == 60 & real_main$position == "middle" &
                     real_main$method == "BiLSTM", ]
    x$id[1] }, "")
  rex <- do.call(rbind, lapply(pick[!is.na(pick)], function(id) do.call(rbind, lapply(METHODS, function(m) {
    f <- file.path(EX, "real_five/curves", sprintf("%s_%s.rds", id, m)); if (!file.exists(f)) return(NULL)
    b <- readRDS(f)$band; w <- rw[[id]]
    data.frame(dataset = w$dataset, method = m, day = seq_len(nrow(b)), median = b$median, lower = b$lower, upper = b$upper,
               observed = c(w$y, w$future)[seq_len(nrow(b))], L = w$length)
  }))))
  rex$dataset <- factor(rex$dataset, levels = DATASETS, labels = c("Utah COVID-19\nadmissions", "Utah influenza\nadmissions",
                                                                   "Baltimore\ninfluenza 1918", "Hong Kong\nSARS 2003", "Sierra Leone\nEbola 2014-15"))
  rex$method <- factor(rex$method, levels = METHODS, labels = LABELS[METHODS])
  cap <- ave(rex$observed, rex$dataset, FUN = function(z) 1.6 * max(z, na.rm = TRUE))
  rex$upper <- pmin(rex$upper, cap); rex$median <- pmin(rex$median, cap); rex$lower <- pmin(rex$lower, cap)
  p <- ggplot(rex, aes(x = day)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = method), alpha = 0.25) +
    geom_line(aes(y = median, colour = method), linewidth = 0.5) +
    geom_point(aes(y = observed), size = 0.35, colour = INK) +
    geom_vline(data = unique(rex[, c("dataset", "L")]), aes(xintercept = L + 0.5), linetype = "dashed", colour = MUTED, linewidth = 0.35) +
    geom_blank(data = unique(data.frame(dataset = rex$dataset, day = 1, cap = cap)), aes(y = cap)) +
    scale_colour_manual(values = setNames(COL, LABELS[names(COL)])) + scale_fill_manual(values = setNames(COL, LABELS[names(COL)])) +
    facet_grid(dataset ~ method, scales = "free_y") +
    labs(x = "Day of the 60-day window around the peak (dashed: end of the window; then the next 14 days)", y = "Daily count",
         title = paste0("Real data: ", M, " fits of every method around the peak of each dataset"),
         subtitle = "points: observed counts; line and band: median and 95% band of 500 model runs (cut at 1.6 times the largest count)") +
    theme_paper(8) + theme(strip.text.y = element_text(angle = 0, hjust = 0))
  save_fig(p, name, 7.4, 7.2)
}
real_examples("SEIR", "real_examples")
real_examples("SIR", "real_examples_sir")

# Every window of one wave of each dataset (SEIR): rows are window lengths, columns positions; the
# observed counts, DeepIMC's 95% band and the median curve of every method.
real_windows <- function(ds, wave, name) {
  ids <- names(rw)[vapply(rw, function(w) w$model == "SEIR" && w$dataset == ds && w$wave == wave, logical(1))]
  d <- do.call(rbind, lapply(ids, function(id) do.call(rbind, lapply(METHODS, function(m) {
    f <- file.path(EX, "real_five/curves", sprintf("%s_%s.rds", id, m)); if (!file.exists(f)) return(NULL)
    s <- readRDS(f); if (isTRUE(s$failed)) return(NULL); b <- s$band; w <- rw[[id]]
    data.frame(length = w$length, position = w$position, method = m, day = seq_len(nrow(b)), median = b$median,
               lower = b$lower, upper = b$upper, observed = c(w$y, w$future)[seq_len(nrow(b))], L = w$length)
  }))))
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  d$length <- factor(paste(d$length, "days"), levels = paste(sort(unique(d$length)), "days"))
  d$position <- factor(d$position, levels = c("beginning", "middle", "end"))
  d$method <- factor(d$method, levels = rev(METHODS), labels = LABELS[rev(METHODS)])
  cap <- 1.6 * max(d$observed, na.rm = TRUE)
  d$upper <- pmin(d$upper, cap); d$median <- pmin(d$median, cap); d$lower <- pmin(d$lower, cap)
  p <- ggplot(d, aes(x = day)) +
    geom_ribbon(data = d[d$method == "DeepIMC", ], aes(ymin = lower, ymax = upper), fill = COL[["BiLSTM"]], alpha = 0.2) +
    geom_point(data = d[d$method == "DeepIMC", ], aes(y = observed), size = 0.3, colour = INK) +
    geom_line(aes(y = median, colour = method), linewidth = 0.45) +
    geom_vline(data = unique(d[, c("length", "position", "L")]), aes(xintercept = L + 0.5), linetype = "dashed",
               colour = MUTED, linewidth = 0.3) +
    scale_colour_manual(values = setNames(COL, LABELS[names(COL)]), breaks = unname(LABELS[METHODS]), name = NULL) +
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
# DeepIMC alone, windows of 30 to 365 days at the beginning, middle and end of 1,000 test epidemics per model.
bl <- do.call(rbind, lapply(c("SEIR", "SIR"), function(M) {
  d <- file.path(EX, paste0(tolower(M), "_clean_big"))
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
  scale_x_continuous(breaks = c(30, 61, 90, 120, 180, 270, 365)) +
  facet_grid(measure ~ model, scales = "free_y", labeller = labeller(measure = label_parsed)) +
  labs(x = "Window length (days)", y = NULL, title = "DeepIMC by window length and position",
       subtitle = "1,000 test epidemics per model (coverage: 100 per length and position); dashed: 95%") +
  theme_paper() + theme(legend.position = "bottom")
save_fig(p_bl, "deepimc_by_window", 7.2, 6.4)

if (HAVE_SIM) {
# All methods on the 10,000 simulated windows, by window length (30-day groups) and position.
sim$length_group <- pmin(floor((sim$length - 30) / 30), 4) * 30 + 45
by_win <- function(col, label, fun = mean) {
  a <- aggregate(sim[[col]], list(model = sim$model, position = sim$position, length = sim$length_group, method = sim$method),
                 function(z) fun(z, na.rm = TRUE))
  data.frame(a[, 1:4], measure = label, value = a$x)
}
sw <- rbind(by_win("R0_error", "R0 MAE"), by_win("new_coverage", "Coverage,\nnew infections (%)"),
            by_win("new_MAE", "MAE,\nnew infections"), by_win("infected_MAE", "MAE, infectious\n(not observed)"))
sw$measure <- factor(sw$measure, levels = unique(sw$measure))
sw$method <- factor(sw$method, levels = METHODS, labels = LABELS[METHODS])
sw$position <- factor(sw$position, levels = c("early", "middle", "late"))
p_sw <- ggplot(sw, aes(length, value, colour = method)) +
  geom_hline(data = data.frame(measure = factor("Coverage,\nnew infections (%)", levels = levels(sw$measure)), y = 95),
             aes(yintercept = y), linetype = "dashed", colour = MUTED, linewidth = 0.35) +
  geom_line(linewidth = 0.55) + geom_point(size = 1.1) +
  scale_colour_manual(values = setNames(COL, LABELS[names(COL)]), name = NULL) +
  scale_x_continuous(breaks = c(45, 75, 105, 135, 165), labels = c("30-59", "60-89", "90-119", "120-149", "150-180")) +
  facet_grid(measure ~ model + position, scales = "free_y") +
  labs(x = "Window length (days)", y = NULL, title = "All methods by window length and position (10,000 simulated windows)",
       subtitle = "Mean over the windows of each group; dashed: 95%") +
  theme_paper(8) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p_sw, "methods_by_window_simulated", 7.4, 7.6)
}

# All methods on the real data, by window length (30, 60, 90 days) and position. MAE is divided by the
# lowest MAE of the five methods on the same window, so that datasets of different size can be averaged.
rr <- real_main
for (part in c("window", "future")) {
  best <- ave(rr[[paste0(part, "_MAE")]], rr$id, FUN = function(z) min(z, na.rm = TRUE))
  rr[[paste0(part, "_relMAE")]] <- rr[[paste0(part, "_MAE")]] / best
}
by_real <- function(col, label) {
  a <- aggregate(rr[[col]], list(model = rr$model, position = rr$position, length = rr$length, method = rr$method),
                 function(z) mean(z[is.finite(z)]))
  data.frame(a[, 1:4], measure = label, value = a$x)
}
rwin <- rbind(by_real("window_coverage", "Coverage,\nwindow (%)"), by_real("window_relMAE", "Relative MAE,\nwindow"),
              by_real("future_coverage", "Coverage,\nnext 14 days (%)"), by_real("future_relMAE", "Relative MAE,\nnext 14 days"))
rwin$measure <- factor(rwin$measure, levels = unique(rwin$measure))
rwin$method <- factor(rwin$method, levels = METHODS, labels = LABELS[METHODS])
rwin$position <- factor(rwin$position, levels = c("beginning", "middle", "end"))
p_rw <- ggplot(rwin, aes(length, value, colour = method)) +
  geom_hline(data = data.frame(measure = factor(c("Coverage,\nwindow (%)", "Coverage,\nnext 14 days (%)"), levels = levels(rwin$measure)), y = 95),
             aes(yintercept = y), linetype = "dashed", colour = MUTED, linewidth = 0.35) +
  geom_hline(data = data.frame(measure = factor(c("Relative MAE,\nwindow", "Relative MAE,\nnext 14 days"), levels = levels(rwin$measure)), y = 1),
             aes(yintercept = y), colour = MUTED, linewidth = 0.35) +
  geom_line(linewidth = 0.55) + geom_point(size = 1.2) +
  scale_colour_manual(values = setNames(COL, LABELS[names(COL)]), name = NULL) +
  scale_x_continuous(breaks = c(30, 60, 90)) +
  facet_grid(measure ~ model + position, scales = "free_y") +
  labs(x = "Window length (days)", y = NULL, title = "All methods by window length and position (five real datasets)",
       subtitle = "Mean over datasets and waves; relative MAE = MAE / lowest MAE of the five methods on the same window (1 = best)") +
  theme_paper(8) + theme(legend.position = "bottom")
save_fig(p_rw, "methods_by_window_real", 7.4, 7.2)

cat("figures written to", normalizePath(OUT), "\n")
