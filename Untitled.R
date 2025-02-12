theta
library(data.table)
library(epiworldR)
library(parallel)
library(keras3)
library(tensorflow)
library(abind)


m <- epiworldR:: ModelSIRCONN(
  "mycon",
  prevalence        = 0.11,
  contact_rate      = 9,
  transmission_rate = 0.17,
  recovery_rate     = 0.46,
  n                 = 8000
)
ndays=60
ans_array
run(m, ndays = ndays)
incidence = epiworldR::plot_incidence(m, plot = TRUE)
plot(m)
incidence=incidence$Infected
# Load necessary library
library(dplyr)

# Read your data (if not already in R)
# incidence <- read.csv("your_data.csv")
saver<-
  make_saver(
    "total_hist",
    "transmission",
    "transition",
    "reproductive",
    "generation"
  )
max_days=ndays

    ans <- list(
      incidence = epiworldR::plot_incidence(m, plot = FALSE)
    )
    ans <- lapply(ans, data.table::as.data.table)

    ans$incidence <- ans$incidence[as.integer(rownames(ans$incidence)) <= (max_days + 1), ]

    ref_table <- data.table::data.table(date = 0:max_days)


    ans <- data.table::data.table(
      infected    = ans[["incidence"]][["Infected"]]

    )

    dprep <- t(diff(as.matrix(ans[-1, ])))
    ans_array <- array(dim = c(1, dim(dprep)))
    ans_array[1, , ] <- dprep

    tensorflow::array_reshape(ans_array, dim = c(1, dim(dprep)))


      # Each augmented time series is inserted as a 1 x time_series_length matrix.
      arrays_1d[1, , ] <- ans_array
      time_series_input=arrays_1d
      predicted_output1 <- model %>% predict(list(time_series_input))
      colnames(predicted_output1)=c("crate","ptran","prec")
      # Print predicted values
      print(predicted_output1)
      True_crate=qlogis(predicted_output1[1,1])*10
      predicted_output1=data.frame(predicted_output1,True_crate)
      rownames(predicted_output1)="predicted"
# Proceed with running the simulation as before
run_multiple(
  m, ndays = 60, nsims = 1, saver = saver,
  nthreads = 8)
run_multiple_get_results(m)
res=run_multiple_get_results(m)
Infected=res$total_hist
Infected$state
Infected_total= Infected %>% filter(state == "Infected")



model_1<- epiworldR:: ModelSIRCONN(
  "mycon_part1",
  prevalence        = 0.11,
  contact_rate      = predicted_output1$True_crate,
  transmission_rate = predicted_output1$ptran,
  recovery_rate     = predicted_output1$prec,
  n                 = 8000
)
ndays=60


saver <- make_saver(
  "total_hist",
  "transmission",
  "transition",
  "reproductive",
  "generation"
)
# Proceed with running the simulation as before
run_multiple(
  model_1, ndays = 60, nsims = 100, saver = saver,
  nthreads = 8)
res1=run_multiple_get_results(model_1)
Infected1=res1$total_hist
library(dplyr)
library(ggplot2)

# Step 1: Filter only 'Infected' cases
infected_data_1 <- Infected1 %>%
  filter(state == "Infected")


# Step 2: Compute Mean and CI for Each Date
summary_stats_1 <- infected_data_1 %>%
  group_by(date) %>%
  summarise(
    mean_count = mean(counts, na.rm = TRUE),  # Compute mean
    sd_count = sd(counts, na.rm = TRUE),  # Compute standard deviation
    n = n(),  # Number of simulations
    se = ifelse(n > 1, sd_count / sqrt(n), 0),  # Standard error (avoid NaN for n=1)
    lower_ci = mean_count - 1.96 * se,  # Lower bound (95% CI)
    upper_ci = mean_count + 1.96 * se   # Upper bound (95% CI)
  ) %>%
  ungroup()


infected_total_summary <- Infected_total %>%
  group_by(date) %>%
  summarise(total_count = mean(counts, na.rm = TRUE)) %>%  # Take mean if multiple values exist
  ungroup()

# Step 4: Plot Both `summary_stats` and `Infected_total`
ggplot() +

  # Add confidence interval ribbon for mean infected counts
  geom_ribbon(data = summary_stats_1, aes(x = date, ymin = lower_ci, ymax = upper_ci, fill = "Mean CI"), alpha = 0.4) +

  # Add mean infected line from `summary_stats`
  geom_line(data = summary_stats_1, aes(x = date, y = mean_count, color = "Mean Infected"), size = 0.6) +

  # Add total infected line from `Infected_total`
  geom_line(data = infected_total_summary, aes(x = date, y = total_count, color = "Total Infected"), size = 0.8, linetype = "dashed") +

  # Adjust y-axis breaks for better CI visibility
  # scale_y_continuous(breaks = seq(min(summary_stats_1$lower_ci), max(summary_stats_1$upper_ci), by = 500)) +

  # Define manual colors for lines
  scale_color_manual(
    name = "Infected Counts",
    values = c("Mean Infected" = "blue", "Total Infected" = "red")
  ) +

  # Define manual fills for ribbons
  scale_fill_manual(
    name = "Confidence Interval",
    values = c("Mean CI" = "lightblue")
  ) +

  # Add labels and title
  labs(
    title = "Comparison of Mean Infected and Total Infected Counts",
    x = "Date",
    y = "Infected Count"
  ) +

  # Apply a minimal theme for better readability
  theme_minimal() +

  # Additional customizations
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
plot(model_1)
plot(m)
