library(data.table)
library(epiworldR)
library(parallel)
library(keras3)
library(tensorflow)
library(abind)


m <- epiworldR:: ModelSIRCONN(
  "mycon",
  prevalence        = 0.25,
  contact_rate      = 20,
  transmission_rate = 0.021,
  recovery_rate     = 0.09,
  n                 = 8000
)
ndays=60

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
# Proceed with running the simulation as before
run_multiple(
  m, ndays = 60, nsims = 1, saver = saver,
  nthreads = 8)
run_multiple_get_results(m)
res=run_multiple_get_results(m)
Infected=res$total_hist
Infected_total= Infected %>%
  filter(state == "Infected")
# Split data into three parts
incidence_part1 <- incidence[1:20]  # Rows 1 to 20
incidence_part2 <- incidence[10:40] # Rows 21 to 40
incidence_part3 <- incidence[30:60] # Rows 41 to 59

# Print or inspect the parts
print(incidence_part1)
print(incidence_part2)
print(incidence_part3)

incidence_part1 <- c(incidence_part1 ,rep(-1,60-length(incidence_part1 )))
incidence_part2 <- c(incidence_part2 ,rep(-1,60-length(incidence_part2 )))
incidence_part3 <- c(incidence_part3 ,rep(-1,60-length(incidence_part3 )))

model=tensorflow::tf$keras$models$load_model(normalizePath("~/epiworldRcalibrate/epiworldRcalibrate/RNN-MODELS/RNN_model_with_metadata_10k_60days.keras"))


# Ensure `incidence_part1` is in the correct format: (batch_size=1, time_steps=59, features=1)
time_series_input <- array(incidence_part1, dim = c(1, 60, 1))  # Reshape as required

# Ensure metadata is shaped correctly: (batch_size=1, features=2)
metadata_input <- matrix(c(8000/1e6, 0.25), nrow = 1, ncol = 2)

# Predict output using the model
predicted_output1 <- model %>% predict(list(time_series_input, metadata_input))
colnames(predicted_output1)=c("prec","crate","ptran")
# Print predicted values
print(predicted_output1)
True_crate=qlogis(predicted_output1[1,2])*10
predicted_output1=data.frame(predicted_output1,True_crate)
rownames(predicted_output1)="predicted"

model_1<- epiworldR:: ModelSIRCONN(
  "mycon_part1",
  prevalence        = 0.25,
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
summary_stats_1= summary_stats_1[2:21,]


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
  scale_y_continuous(breaks = seq(min(summary_stats_1$lower_ci), max(summary_stats_1$upper_ci), by = 500)) +

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


####part2:

model=tensorflow::tf$keras$models$load_model(normalizePath("~/epiworldRcalibrate/epiworldRcalibrate/RNN-MODELS/RNN_model_with_metadata_10k.keras"))


# Ensure `incidence_part1` is in the correct format: (batch_size=1, time_steps=59, features=1)
time_series_input <- array(incidence_part2, dim = c(1, 59, 1))  # Reshape as required

# Ensure metadata is shaped correctly: (batch_size=1, features=2)
metadata_input <- matrix(c(8000/1e6, 0.25), nrow = 1, ncol = 2)

# Predict output using the model
predicted_output2 <- model %>% predict(list(time_series_input, metadata_input))
colnames(predicted_output2)=c("prec","crate","ptran")
# Print predicted values
print(predicted_output2)
True_crate=qlogis(predicted_output2[1,2])*10
predicted_output2=data.frame(predicted_output2,True_crate)
rownames(predicted_output2)="predicted"

model_2<- epiworldR:: ModelSIRCONN(
  "mycon_part2",
  prevalence        = 0.25,
  contact_rate      = predicted_output2$True_crate,
  transmission_rate = predicted_output2$ptran,
  recovery_rate     = predicted_output2$prec,
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
  model_2, ndays = 60, nsims = 100, saver = saver,
  nthreads = 8)
res2=run_multiple_get_results(model_2)
Infected2=res2$total_hist

infected_data_2<- Infected2 %>%
  filter(state == "Infected")


# Step 2: Compute Mean and CI for Each Date
summary_stats_2 <- infected_data_2 %>%
  group_by(date) %>%
  summarise(
    mean_count = mean(counts, na.rm = TRUE),  # Compute mean
    sd_count = sd(counts, na.rm = TRUE),  # Compute standard deviation
    n = n(),  # Number of simulations
    se = ifelse(n > 1, sd_count / sqrt(n), 0),  # Standard error
    lower_ci = mean_count - 1.96 * se,  # Lower bound (95% CI)
    upper_ci = mean_count + 1.96 * se   # Upper bound (95% CI)
  ) %>%
  ungroup()
summary_stats_2= summary_stats_2 [11:41,]

infected_total_summary <- Infected_total %>%
  group_by(date) %>%
  summarise(total_count = mean(counts, na.rm = TRUE)) %>%  # Take mean if multiple values exist
  ungroup()

# Step 4: Plot Both `summary_stats` and `Infected_total`
ggplot() +

  # Add confidence interval ribbon for mean infected counts
  geom_ribbon(data = summary_stats_2, aes(x = date, ymin = lower_ci, ymax = upper_ci, fill = "Mean CI"), alpha = 0.4) +

  # Add mean infected line from `summary_stats`
  geom_line(data = summary_stats_2, aes(x = date, y = mean_count, color = "Mean Infected"), size = 0.6) +

  # Add total infected line from `Infected_total`
  geom_line(data = infected_total_summary, aes(x = date, y = total_count, color = "Total Infected"), size = 0.8, linetype = "dashed") +

  # Adjust y-axis breaks for better CI visibility
  scale_y_continuous(breaks = seq(min(summary_stats$lower_ci), max(summary_stats$upper_ci), by = 500)) +

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

####part3:


model=tensorflow::tf$keras$models$load_model(normalizePath("~/epiworldRcalibrate/epiworldRcalibrate/RNN-MODELS/RNN_model_with_metadata_10k.keras"))


# Ensure `incidence_part1` is in the correct format: (batch_size=1, time_steps=59, features=1)
time_series_input <- array(incidence_part3, dim = c(1, 59, 1))  # Reshape as required

# Ensure metadata is shaped correctly: (batch_size=1, features=2)
metadata_input <- matrix(c(8000/1e6, 0.25), nrow = 1, ncol = 2)

# Predict output using the model
predicted_output3 <- model %>% predict(list(time_series_input, metadata_input))
colnames(predicted_output3)=c("prec","crate","ptran")
# Print predicted values
print(predicted_output3)
True_crate=qlogis(predicted_output3[1,2])*10
predicted_output3=data.frame(predicted_output3,True_crate)
rownames(predicted_output3)="predicted"
predicted_output3


model_3<- epiworldR:: ModelSIRCONN(
  "mycon_part2",
  prevalence        = 0.25,
  contact_rate      = predicted_output3$True_crate,
  transmission_rate = predicted_output3$ptran,
  recovery_rate     = predicted_output3$prec,
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
  model_3, ndays = 60, nsims = 100, saver = saver,
  nthreads = 8)
res3=run_multiple_get_results(model_3)
Infected3=res3$total_hist

infected_data_3<- Infected3 %>%
  filter(state == "Infected")


# Step 2: Compute Mean and CI for Each Date
summary_stats_3 <- infected_data_3 %>%
  group_by(date) %>%
  summarise(
    mean_count = mean(counts, na.rm = TRUE),  # Compute mean
    sd_count = sd(counts, na.rm = TRUE),  # Compute standard deviation
    n = n(),  # Number of simulations
    se = ifelse(n > 1, sd_count / sqrt(n), 0),  # Standard error
    lower_ci = mean_count - 1.96 * se,  # Lower bound (95% CI)
    upper_ci = mean_count + 1.96 * se   # Upper bound (95% CI)
  ) %>%
  ungroup()
summary_stats_3= summary_stats_3[31:61,]
# Step 3: Plot Mean with Confidence Interval
infected_total_summary <- Infected_total %>%
  group_by(date) %>%
  summarise(total_count = mean(counts, na.rm = TRUE)) %>%  # Take mean if multiple values exist
  ungroup()

# Step 4: Plot Both `summary_stats` and `Infected_total`

