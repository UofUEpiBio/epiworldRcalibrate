# Load necessary libraries
library(data.table)
library(epiworldR)
library(parallel)
library(keras3)
library(tensorflow)
library(abind)

# Set seed for reproducibility
set.seed(1231)

# Parameters

# Set the number of simulations
N <- 1e4

# Generate 'n' first
n_values <- sample(5000:10000, N, replace = TRUE)

# Now create 'theta' and use 'n_values' correctly
theta <- data.table(
  n      = n_values,  # Population size from U(5000,10000)
  preval = sample(100:2000, N, replace = TRUE) / n_values,  # Use the correct 'n' values
  crate  = runif(N, 5, 20),  # Contact rate from U(5,20)
  recov  = 1 / runif(N, 4, 14),  # Recovery rate as 1/U(4,14)
  R0     = runif(N, 1, 5)  # Basic reproduction number from U(1,5)
)

# Calculate transmission rate
theta[, ptran := R0 * recov / crate]

# Print first few rows to check
head(theta)


# Ensure valid ptran values (between 0.05 and 0.95)
# theta <- theta[ptran > 0.05 & ptran < 0.95]

# Function to Prepare Data for TensorFlow Model: General SIR Data Preparation
prepare_data <- function(m, max_days = 60) {
  ans <- tryCatch({
    ans <- list(
      repnum    = epiworldR::plot_reproductive_number(m, plot = FALSE),
      incidence = epiworldR::plot_incidence(m, plot = FALSE),
      gentime   = epiworldR::plot_generation_time(m, plot = FALSE)
    )

    ans <- lapply(ans, data.table::as.data.table)
    ans$repnum$avg <- data.table::nafill(ans$repnum$avg, type = "locf")
    ans$gentime$avg <- data.table::nafill(ans$gentime$avg, type = "locf")

    ans$repnum    <- ans$repnum[ans$repnum$date <= max_days, ]
    ans$gentime   <- ans$gentime[ans$gentime$date <= max_days, ]
    ans$incidence <- ans$incidence[as.integer(rownames(ans$incidence)) <= (max_days + 1), ]

    ref_table <- data.table::data.table(date = 0:max_days)

    ans[["repnum"]] <- data.table::merge.data.table(ref_table, ans[["repnum"]], by = "date", all.x = TRUE)
    ans[["gentime"]] <- data.table::merge.data.table(ref_table, ans[["gentime"]], by = "date", all.x = TRUE)

    ans <- data.table::data.table(
      infected    = ans[["incidence"]][["Infected"]],
      recovered   = ans[["incidence"]][["Recovered"]],
      repnum      = ans[["repnum"]][["avg"]],
      gentime     = ans[["gentime"]][["avg"]],
      repnum_sd   = ans[["repnum"]][["sd"]],
      gentime_sd  = ans[["gentime"]][["sd"]]
    )

    nafill_cols <- c("infected", "recovered", "repnum", "gentime", "repnum_sd", "gentime_sd")
    for (col in nafill_cols) {
      ans[[col]] <- data.table::nafill(ans[[col]], type = "locf")
    }

    dprep <- t((as.matrix(ans[-1, ])))
    ans_array <- array(dim = c(1, dim(dprep)))
    ans_array[1, , ] <- dprep

    tensorflow::array_reshape(ans_array, dim = c(1, dim(dprep)))
  }, error = function(e) e)

  if (inherits(ans, "error")) {
    return(ans)
  }

  return(ans_array)
}

# Function to Run SIR Model Simulations in Parallel
run_simulations <- function(N, theta) {
  ncores <- parallel::detectCores() - 1
  seeds <- sample(1:1e6, N, replace = FALSE)

  matrices <- parallel::mclapply(1:N, function(i) {
    set.seed(seeds[i])
    m <- epiworldR::ModelSIRCONN(
      "mycon",
      prevalence        = theta$preval[i],
      contact_rate      = theta$crate[i],
      transmission_rate = theta$ptran[i],
      recovery_rate     = theta$recov[i],
      n                 = theta$n[i]
    )

    verbose_off(m)
    run(m, ndays = 60)
    ans <- prepare_data(m, max_days = 60)

    return(ans)
  }, mc.cores = ncores)

  return(matrices)
}

# Run Simulations
matrices <- run_simulations(N, theta)
# Remove NULL values from matrices
valid_indices <- which(!sapply(matrices, is.null))
matrices <- matrices[valid_indices]
theta <- theta[valid_indices, ]  # Ensure theta stays in sync with matrices
# Now

# Filter out simulations with errors or NA values
is_not_null <- intersect(
  which(!sapply(matrices, inherits, what = "error")),
  which(!sapply(matrices, function(x) any(is.na(x))))
)
matrices <- matrices[is_not_null]
theta    <- theta[is_not_null,]
length(matrices)

data <- list()
for (i in seq_along(matrices)) {
  data[[i]] <- matrices[[i]][, 1, , drop = FALSE]  # Extract first column across all layers
}

data2 <- lapply(data, function(mat) c(mat))
augment_ts <- function(
    x, n,
    min_size = NULL,
    max_size = NULL,
    fill = -1,
    ncpus = parallel::detectCores() - 1L
) {
  # If x is a list, apply augment_ts to each vector in parallel
  if (is.list(x)) {
    return(parallel::mclapply(
      x, augment_ts, n = n,
      min_size = min_size, max_size = max_size,
      fill = fill, ncpus = 1L,
      mc.cores = ncpus
    ))
  }

  # Ensure x is a numeric vector
  if (!is.numeric(x)) stop("x must be a numeric vector.")

  len_x <- length(x)  # Get the length of the time series

  # Set default window sizes
  if (is.null(min_size)) min_size <- max(10, floor(len_x / 2))  # Ensure min_size is at least 10
  if (is.null(max_size)) max_size <- len_x - 1L  # Ensure max_size does not exceed len_x

  # Prevent invalid sizes
  if (min_size >= len_x) min_size <- floor(len_x / 2)
  if (max_size >= len_x) max_size <- len_x - 1L
  if (min_size > max_size) min_size <- max_size - 1L

  # 1. Generate random sizes
  sizes <- sample(min_size:max_size, n, replace = TRUE)

  # 2. Generate safe start indices to prevent exceeding vector length
  idxs <- sapply(sizes, function(size) {
    sample(1:(len_x - size + 1), 1)  # Ensure start index allows a full window
  })

  # 3. Extract the windows and pad with `fill`
  windows <- lapply(seq_along(idxs), function(i) {
    idx <- idxs[i]
    size <- sizes[i]
    window <- x[idx:(idx + size - 1)]  # Extract valid sub-sequence
    padded <- c(window, rep(fill, len_x - size))  # Correctly pad with `fill`
    return(padded)
  })

  return(windows)
}

augmented_data <- augment_ts(data2,n=10, min_size = 15,max_size = 59)
# x <- replicate(1000, 1:100, simplify = FALSE)

split_pad=unlist(augmented_data,recursive=FALSE)

theta_expanded <- theta[rep(seq_len(.N), each = 10)]
theta_pad <- theta_expanded

# Save processed dataset and thetas
saveRDS(list(data = split_pad, theta = theta_pad), file = "RNN-MODELS/split_pad_with_theta.rds", compress = TRUE)

# Check the number of datasets after splitting
cat("Total datasets after splitting:", length(split_pad), "\n")
cat("Total theta entries after splitting:", nrow(theta_pad), "\n")

# Example: View some of the first dataset splits and their thetas
print(split_pad[[1]])  # First split dataset
print(split_pad[[10]]) # Another split dataset
print(theta_pad[1, ])  # First corresponding theta
print(theta_pad[10, ]) # Another corresponding theta


# Prepare data for TensorFlow
input_data2 <- array(unlist(split_pad), dim = c(length(split_pad), 59, 1))
# target_data <- as.matrix(theta_pad[,c(3,4,6)])
# target_data[,1] = plogis(target_data[,1]/10)
# hist(target_data[,1])
# hist(as.matrix(theta_pad[,3]))
# Split Data into Train and Test Sets
set.seed(123)
# Filter only required columns for model training
theta_filtered <- theta_pad[, .(n, preval)]  # Model Inputs
theta_target   <- theta_pad[, .(recov, crate, ptran)]  # Model Outputs
theta_target[,2] <- plogis(as.matrix(theta_target[,2]/10))
# Convert to matrices for TensorFlow
input_data <- array(unlist(split_pad), dim = c(length(split_pad), 59, 1))
target_data <- as.matrix(theta_target)
input_data <- split_pad
# Ensure input metadata is properly used
input_metadata <- as.matrix(theta_filtered)
data2=list(split_pad, theta_filtered)
# Split into Training and Testing Sets (80-20 split)
set.seed(123)


# Normalize Metadata Features (n, preval) Using Min-Max Scaling
# min_values <- apply(data2[[2]], 2, min)
# max_values <- apply(data2[[2]], 2, max)
# scaled_metadata <- scale(data2[[2]], center = min_values, scale = max_values - min_values)
# scaled_metadata=data2[[2]]/1e6
scaled_metadata=as.matrix(data2[[2]])
scaled_metadata[,1]=scaled_metadata[,1]/1e6
# Convert `data2[[1]]` into an array format (assuming it's a list of numeric vectors)
time_series_data <- array(unlist(data2[[1]]), dim = c(length(data2[[1]]), 59, 1))

# Split Data into Train/Test Sets (80-20)
set.seed(123)
num_samples <- nrow(target_data)
train_indices <- sample(1:num_samples, size = floor(0.8 * num_samples))

# Training & Testing Data (Passing both time-series and metadata)
train_x <- list(time_series_data[train_indices, , , drop = FALSE], scaled_metadata[train_indices, , drop = FALSE])
test_x  <- list(time_series_data[-train_indices, , , drop = FALSE], scaled_metadata[-train_indices, , drop = FALSE])

train_y <- target_data[train_indices, , drop = FALSE]
test_y  <- target_data[-train_indices, , drop = FALSE]

# Define Temporal Input (Time-Series Data)
temporal_input <- layer_input(shape = c(59, 1), name = "temporal_input")

# Apply Masking to Ignore Padded Timesteps (-1)
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = -1)

# RNN Layer for Processing Time-Series Data
rnn_output <- masked_temporal_input %>%
  layer_simple_rnn(units = 32, activation = "tanh", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.2)

# Define Metadata Input (Static Features: n, preval)
metadata_input <- layer_input(shape = c(2), name = "metadata_input")

# Concatenate RNN and Metadata Processing
merged <- layer_concatenate(list(rnn_output, metadata_input))

# Final Output Layer (Regression Task)
final_output <- merged %>%
  layer_dense(units = 3, activation = "linear", name = "output")

# Define the Model (Now it accepts two inputs)
model <- keras_model(inputs = list(temporal_input, metadata_input), outputs = final_output)

# Compile the Model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",
  metrics = c("mae", "mse")
)

# Train the Model
 model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate Model
evaluation_RNN <- model %>% evaluate(test_x, test_y)
print(evaluation_RNN)

# Predict Using the Model
predictions_RNN <- model %>% predict(test_x)

MAEs_RNN <- abs(predictions_RNN - as.matrix(test_y)) |>
  colMeans() |>
  print()
# Save Model
model$save('RNN_model_with_metadata_10k.keras')

# Save Results
saveRDS(predictions_RNN, file = "predictions_RNN_with_metadata_10k.rds")
saveRDS(evaluation_RNN, file = "evaluation_RNN_with_metadata_10k.rds")
saveRDS(MAEs_RNN, file = "MAEs_RNN_with_metadata_10k.rds")


pred=as.data.table(predictions_RNN)

pred[, id := 1L:.N]
pred=as.matrix(pred)
pred[, 2] <- qlogis(as.numeric(pred[, 2]))

pred <- as.data.table(pred)

# Convert `id` column to integer (if needed)
pred[, id := as.integer(id)]
names(pred)=c("recov","crate","ptran","id")
# Melt properly using `data.table::melt()`
pred_long <- melt(pred, id.vars = "id",, value.name = "value")



theta_long <- test_y |> as.data.table()
setnames(theta_long, names(theta_long))
theta_long[, id := 1L:.N]
theta_long[, crate := qlogis(crate)]
theta_long <- melt(theta_long, id.vars = "id")

alldat <- rbind(
  cbind(pred_long, Type = "Predicted"),
  cbind(theta_long, Type = "Observed")
)

library(ggplot2)
ggplot(alldat, aes(x = value, colour = Type)) +
  facet_wrap(~variable, scales = "free") +
  geom_boxplot()

alldat_wide <- dcast(alldat, id + variable ~ Type, value.var = "value")

vnames <- data.table(
  variable = c("recov","crate","ptran"),
  Name     = paste(
    c("P(recovery)", "Contact Rate", "P(transmit)"),
    sprintf("(MAE: %.2f)", MAEs_RNN)
  )
)

alldat_wide <- merge(alldat_wide, vnames, by = "variable")
N_train=train_indices
N=length(split_pad)
ggplot(alldat_wide, aes(x = Observed, y = Predicted)) +
  facet_wrap(~ Name, scales = "free") +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = .2) +
  labs(
    title    = "Observed vs Predicted (validation set)",
    subtitle = sprintf(
      "The model includes %i simulated datasets, of which %i were used for training.",
      N,
      N_train
    ),
    caption  = "Predictions made using a CNN as implemented with loss function MAE."

  )



tensorflow::tf$keras$backend$clear_session()
### LSTM


# Define Temporal Input (Time-Series Data)
temporal_input <- layer_input(shape = c(59, 1), name = "temporal_input")

# Apply Masking to Ignore Padded Timesteps (-1)
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = -1)

# **Replace SimpleRNN with LSTM**
lstm_output <- masked_temporal_input %>%
  layer_lstm(units = 64, activation = "tanh", return_sequences = FALSE, kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.3)  # Increased dropout for better generalization

# Define Metadata Input (Static Features: n, preval)
metadata_input <- layer_input(shape = c(2), name = "metadata_input")

# Concatenate LSTM and Metadata Processing
merged <- layer_concatenate(list(lstm_output, metadata_input))

# Fully Connected Layers After Concatenation
final_output <- merged %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 3, activation = "linear", name = "output")  # Predicting (recov, crate, ptran)

# Define the Model
model <- keras_model(inputs = list(temporal_input, metadata_input), outputs = final_output)

# Compile the Model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",
  metrics = c("mae", "mse")
)

# Train the Model
history <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 50,  # Adjust based on loss
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate Model
evaluation_LSTM <- model %>% evaluate(test_x, test_y)
print(evaluation_LSTM)

# Predict Using the Model
predictions_LSTM <- model %>% predict(test_x)

# Calculate Mean Absolute Errors (MAE)
MAEs_LSTM <- abs(predictions_LSTM - as.matrix(test_y)) |>
  colMeans() |>
  print()

# Save Model
model$save('LSTM_model_with_metadata_10k.keras')

# Save Results
saveRDS(predictions_LSTM, file = "predictions_LSTM_with_metadata_10k.rds")
saveRDS(evaluation_LSTM, file = "evaluation_LSTM_with_metadata_10k.rds")
saveRDS(MAEs_LSTM, file = "MAEs_LSTM_with_metadata_10k.rds")

pred=as.data.table(predictions_LSTM)
pred=as.matrix(pred)
pred[, id := 1L:.N]
pred[, 2] <- qlogis(as.numeric(pred[, 2]))

pred <- as.data.table(pred)

# Convert `id` column to integer (if needed)
pred[, id := as.integer(id)]
names(pred)=c("recov","crate","ptran","id")
# Melt properly using `data.table::melt()`
pred_long <- melt(pred, id.vars = "id",, value.name = "value")



theta_long <- test_y |> as.data.table()
setnames(theta_long, names(theta_long))
theta_long[, id := 1L:.N]
theta_long[, crate := qlogis(crate)]
theta_long <- melt(theta_long, id.vars = "id")

alldat <- rbind(
  cbind(pred_long, Type = "Predicted"),
  cbind(theta_long, Type = "Observed")
)

library(ggplot2)
ggplot(alldat, aes(x = value, colour = Type)) +
  facet_wrap(~variable, scales = "free") +
  geom_boxplot()

alldat_wide <- dcast(alldat, id + variable ~ Type, value.var = "value")

vnames <- data.table(
  variable = c("recov","crate","ptran"),
  Name     = paste(
    c("P(recovery)", "Contact Rate", "P(transmit)"),
    sprintf("(MAE: %.2f)", MAEs_LSTM)
  )
)

alldat_wide <- merge(alldat_wide, vnames, by = "variable")
N_train=train_indices
N=length(split_pad)
ggplot(alldat_wide, aes(x = Observed, y = Predicted)) +
  facet_wrap(~ Name, scales = "free") +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = .2) +
  labs(
    title    = "Observed vs Predicted (validation set)",
    subtitle = sprintf(
      "The model includes %i simulated datasets, of which %i were used for training.",
      N,
      N_train
    ),
    caption  = "Predictions made using a CNN as implemented with loss function MAE."

  )

ggsave(filename = "calibration/sir_infections_only.png", width = 1280, height = 800, units = "px", scale = 3)

library(ggplot2)
library(data.table)

# Ensure `alldat_wide` is a data.table
setDT(alldat_wide)

library(ggplot2)
library(data.table)

# Ensure `alldat_wide` is a data.table
setDT(alldat_wide)

# Create the plot with better visibility
ggplot(alldat_wide, aes(x = Observed, y = Predicted, color = variable)) +
  geom_point(alpha = 0.6, size = 1.5) +  # Scatter plot with larger points
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black", size = 1.2) +  # Thick dashed line
  facet_wrap(~ Name, scales = "free") +  # Separate plots per variable with MAE
  labs(title = "Observed vs. Predicted Values",
       x = "Observed",
       y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "bottom")

