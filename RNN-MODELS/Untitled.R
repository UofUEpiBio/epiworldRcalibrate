# --- Load Libraries ---
library(data.table)
library(epiworldR)
library(keras3)
library(tensorflow)
library(ggplot2)

# --- Helper Functions ---

# Run simulation and return results.
run_sim <- function(model, ndays = 60, nsims = 100, threads = 8) {
  saver <- make_saver("total_hist", "transmission", "transition", "reproductive", "generation")
  run_multiple(model, ndays = ndays, nsims = nsims, saver = saver, nthreads = threads)
  run_multiple_get_results(model)
}

# Compute summary stats (mean, sd, se, 95% CI) for the "Infected" state over a date range.
get_stats <- function(res, drange) {
  dt <- as.data.table(res$total_hist)[state == "Infected"]
  stats <- dt[, .(mean = mean(counts, na.rm = TRUE),
                  sd   = sd(counts, na.rm = TRUE),
                  n    = .N), by = date]
  stats[, se := ifelse(n > 1, sd/sqrt(n), 0)]
  stats[, `:=` (lower = mean - 1.96 * se, upper = mean + 1.96 * se)]
  stats[date >= drange[1] & date <= drange[2]]
}

# Pad/truncate an incidence vector to length L, reshape and predict parameters.
predict_params <- function(inc_vec, model, L = 59, n = 8000, prev = 0.25) {
  if (length(inc_vec) < L) {
    inc_vec <- c(inc_vec, rep(-1, L - length(inc_vec)))
  } else {
    inc_vec <- inc_vec[1:L]
  }
  ts_input <- array(inc_vec, dim = c(1, L, 1))
  meta_input <- matrix(c(n / 1e6, prev), nrow = 1)
  pred <- model %>% predict(list(ts_input, meta_input))
  colnames(pred) <- c("prec", "crate", "ptran")
  data.frame(prec = pred[1, "prec"],
             crate = pred[1, "crate"],
             ptran = pred[1, "ptran"],
             True_crate = qlogis(pred[1, "crate"]) * 10)
}

# Create a new simulation model using predicted parameters.
create_model <- function(name, pred, n = 8000, prev = 0.25) {
  epiworldR::ModelSIRCONN(name, prevalence = prev, contact_rate = pred$True_crate,
                          transmission_rate = pred$ptran, recovery_rate = pred$prec, n = n)
}

# --- Main Script ---

n <- 8000; ndays <- 60

# Initial simulation.
init_mod <- epiworldR::ModelSIRCONN("mycon", prevalence = 0.25, contact_rate = 20,
                                    transmission_rate = 0.021, recovery_rate = 0.09, n = n)
run(init_mod, ndays = ndays)
incidence <- epiworldR::plot_incidence(init_mod, plot = TRUE)$Infected

# Baseline simulation.
base_res <- run_sim(init_mod, ndays, nsims = 1)
base_stats <- as.data.table(base_res$total_hist)[state == "Infected", .(total = mean(counts, na.rm = TRUE)), by = date]

# Split the incidence vector.
parts <- list(
  part1 = incidence[1:20],
  part2 = incidence[10:40],
  part3 = incidence[30:59]
)

# Load Keras model.
kmodel <- tensorflow::tf$keras$models$load_model(normalizePath("~/epiworldRcalibrate/epiworldRcalibrate/RNN-MODELS/RNN_model_with_metadata_10k.keras"))

# Process each incidence part.
for (i in seq_along(parts)) {
  pred <- predict_params(parts[[i]], kmodel)
  cat("Predicted parameters for part", i, ":\n")
  print(pred)

  sim_mod <- create_model(paste0("mycon_part", i), pred)
  res <- run_sim(sim_mod, ndays, nsims = 100)

  # Define date ranges (adjust as needed)
  drange <- switch(i,
                   "1" = c(2, 21),
                   "2" = c(11, 40),
                   "3" = c(31, 60))
  stats <- get_stats(res, drange)

  p <- ggplot() +
    geom_ribbon(data = stats, aes(x = date, ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
    geom_line(data = stats, aes(x = date, y = mean), color = "blue", size = 0.6) +
    geom_line(data = base_stats, aes(x = date, y = total), color = "red", linetype = "dashed", size = 0.8) +
    labs(title = paste("Comparison for Part", i), x = "Date", y = "Infected Count") +
    theme_minimal()
  print(p)
}
