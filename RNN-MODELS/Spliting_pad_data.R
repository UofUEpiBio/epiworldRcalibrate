# Load necessary libraries
library(data.table)
library(epiworldR)
library(parallel)
library(keras3)
library(tensorflow)
library(abind)


# Function to Generate Theta Parameters for SIR Model Simulation
generate_theta <- function(N, n) {
  set.seed(1231)
  theta <- data.table::data.table(
    preval = sample((100:2000) / n, N, replace = TRUE),
    crate  = stats::rgamma(N, shape = 5, rate = 1),
    ptran  = stats::rbeta(N, shape1 = 3, shape2 = 7),
    prec   = stats::rbeta(N, shape1 = 10, shape2 = 10)
  )
  return(theta)
}

# Function to Prepare Data for TensorFlow Model: General SIR Data Preparation
prepare_data <- function(m, max_days = 60) {
  err <- tryCatch({
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

    ref_table <- data.table::data.table(
      date = 0:max_days
    )

    ans[["repnum"]] <- data.table::merge.data.table(
      ref_table, ans[["repnum"]], by = "date", all.x = TRUE
    )
    ans[["gentime"]] <- data.table::merge.data.table(
      ref_table, ans[["gentime"]], by = "date", all.x = TRUE
    )

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

    # Compute first differences to capture changes
    dprep <- t(diff(as.matrix(ans[-1, ])))
    ans_array <- array(dim = c(1, dim(dprep)))
    ans_array[1, , ] <- dprep

    # Reshape for TensorFlow
    tensorflow::array_reshape(
      ans_array,
      dim = c(1, dim(dprep))
    )
  }, error = function(e) e)

  if (inherits(err, "error")) {
    return(err)
  }

  return(ans_array)
}

# Function to Run SIR Model Simulations in Parallel
run_simulations <- function(N, n, ndays, ncores, theta, seeds) {
  matrices <- parallel::mclapply(1:N, FUN = function(i) {
    set.seed(seeds[i])
    m <- epiworldR::ModelSIRCONN(
      "mycon",
      prevalence        = theta$preval[i],
      contact_rate      = theta$crate[i],
      transmission_rate = theta$ptran[i],
      recovery_rate     = theta$prec[i],
      n                 = n
    )

    verbose_off(m)
    run(m, ndays = ndays)
    ans <- prepare_data(m, max_days = ndays)

    return(ans)
  }, mc.cores = ncores)

  return(matrices)
}

# Parameters
N <- 2e4          # Number of simulations
n <- 5000          # Population size per simulation
ndays <- 60        # Number of days to simulate
ncores <- 20       # Number of cores for parallel processing
seeds <- sample(1:1e6, N, replace = FALSE)  # Unique seeds for reproducibility

# Generate theta and run simulations
theta <- generate_theta(N, n)
matrices <- run_simulations(N, n, ndays, ncores, theta, seeds)


# Filter out simulations that resulted in errors or contain NA values
is_not_null <- intersect(
  which(!sapply(matrices, inherits, what = "error")),
  which(!sapply(matrices, function(x) any(is.na(x))))
)
matrices <- matrices[is_not_null]
# spliting datasets:
length(matrices)
# 19994 dataset of 1,6,59 ya 19994,6, 59
dim(matrices[[1]])
theta    <- theta[is_not_null,]


N <- length(is_not_null)

# Setting up the data for tensorflow. Need to figure out how we would configure
# this to store an array of shape 3 x 100 (three rows, S I R) and create the
# convolution.

# Convolutional Neural Network
library(keras3)

# (N obs, rows, cols)
# Important note, it is better for the model to handle changes rather than
# total numbers. For the next step, we need to do it using % change, maybe...
arrays_1d <- array(dim = c(N, dim(matrices[[1]][1,,])))
for (i in seq_along(matrices))
  arrays_1d[i,,] <- matrices[[i]][1,,]
#   t(matrices[[i]][-nrow(matrices[[i]]),]) + 1e-20
# )[,1:49]

# t(diff(t(matrices[[i]])))/(
#   matrices[[i]][,-ncol(matrices[[i]])] + 1e-20
# )[,1:50]

theta2 <- copy(theta)
theta2[, crate := plogis(crate / 10)]

# Saving the data
saveRDS(
  list(
    theta = theta2,
    simulations = arrays_1d
  ),
  file = "RNN-MODELS/sir.rds",
  compress = TRUE
)
sim_results=readRDS("RNN-MODELS/sir.rds")
theta <- sim_results$theta
arrays_1d <- sim_results$simulations
data <- list()
for (i in seq_along(matrices)) {
  data[[i]] <- matrices[[i]][, 1, , drop = FALSE]  # Extract first column across all layers
}

data2 <- lapply(data, function(mat) c(mat))

N     <- dim(arrays_1d)[1]


# George's Function:
#' Randomly Augment Time Series Data
#' @param x Either a vector or a list.
#' @param n Integer. The number of augmented time series to generate.
#' @param min_size Integer. The minimum size of the augmented time series.
#' @param max_size Integer. The maximum size of the augmented time series.
#' @param fill Numeric. The value to fill the augmented time series with.
#' @param ncpus Integer. The number of CPU cores to use for parallel processing.

augment_ts <- function(
    x, n,
    min_size = NULL, #floor(length(x)/2),
    max_size = NULL, #length(x),
    fill = -1,
    ncpus = parallel::detectCores() - 1L
) {

  if (inherits(x, "list"))
    return(
      unlist(parallel::mclapply(
        x, augment_ts, n = n,
        min_size = min_size, max_size = max_size,
        fill = fill, ncpus = 1L,
        mc.cores = ncpus
      ), recursive = FALSE
      )
    )

  if (is.null(min_size)) min_size <- floor(length(x)/2)
  if (is.null(max_size)) max_size <- length(x) - 1L

  # 1. Generate random sizes
  sizes <- sample(min_size:max_size, n, replace = TRUE)

  # 2. Generate random start indices
  idxs <- floor(runif(n, 1, max = length(x) - sizes))

  # 3. Extract the windows
  Map(\(idx, size) {
    window <- idx:(idx + size - 1)
    x[1:size] <- x[window]
    x[(size + 1):length(x)] <- fill
    x
  }, idx = idxs, size = sizes)

}




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

  # Set default window sizes based on vector length
  if (is.null(min_size)) min_size <- max(10, floor(length(x) / 2))  # Ensure min_size is at least 10
  if (is.null(max_size)) max_size <- length(x) - 1L  # Avoid max_size being full length

  # Avoid invalid window sizes
  if (min_size >= length(x)) min_size <- floor(length(x) / 2)
  if (max_size >= length(x)) max_size <- length(x) - 1L
  if (min_size > max_size) min_size <- max_size - 1L

  # 1. Generate random sizes
  sizes <- sample(min_size:max_size, n, replace = TRUE)

  # 2. Generate random start indices
  idxs <- sample(1:(length(x) - min(sizes)), n, replace = TRUE)

  # 3. Extract the windows and pad with `fill`
  windows <- lapply(seq_along(idxs), function(i) {
    idx <- idxs[i]
    size <- sizes[i]
    window <- x[idx:(idx + size - 1)]  # Extract sub-sequence
    padded <- c(window, rep(fill, length(x) - size))  # Pad with `fill`
    return(padded)
  })

  return(windows)
}
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

augmented_data <- augment_ts(data2, min_size = 15,max_size = 59)
 # x <- replicate(1000, 1:100, simplify = FALSE)

split_pad=augmented_data

saveRDS(split_pad, file = "RNN-MODELS/split_pad.rds", compress = TRUE)
length(split_pad)
input_data=split_pad


input_data2 <- array(unlist(input_data), dim = c(length(input_data), 59, 1))

# Extract the target data
target_data <- rbind(theta)

# Check the shapes
print(dim(input_data2))
print(dim(target_data))

# Split into Training and Testing Sets (80-20 split)
# Set seed for reproducibility
set.seed(123)

# Get the number of samples
num_samples <- nrow(target_data)

# Create a random training set (80% of data)
train_indices <- sample(1:num_samples, size = floor(0.8 * num_samples))

# Split the data into training and testing sets
train_x <- input_data2[train_indices, , , drop = FALSE]  # Training input
test_x <- input_data2[-train_indices, , , drop = FALSE]  # Test input

train_y <- target_data[train_indices, , drop = FALSE]  # Training targets (all 4 columns)
test_y <- target_data[-train_indices, , drop = FALSE]  # Test targets (all 4 columns)

# Print Shapes to Verify
cat("Train X shape:", dim(train_x), "\n")  # Expected: (N_train, 59, 1)
cat("Train Y shape:", dim(train_y), "\n")  # Expected: (N_train, 4)
cat("Test X shape:", dim(test_x), "\n")  # Expected: (N_test, 59, 1)
cat("Test Y shape:", dim(test_y), "\n")  # Expected: (N_test, 4)
train_y <- as.matrix(train_y)  # Ensure it's a proper matrix
test_y <- as.matrix(test_y)  # Ensure test_y is also correct
#first simple model:
# Define Temporal Input (Variable-Length Time-Series Data)
temporal_input <- layer_input(shape = c(59, 1), name = "temporal_input")  # Input: 59 time steps, 1 feature

# Apply Masking to Ignore Padded Timesteps
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = -1)  # Ignores -1 values as padding

# Define RNN Layer (Handles Variable-Length Inputs)
rnn_output <- masked_temporal_input %>%
  layer_simple_rnn(units = 32, activation = "tanh",
                   kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.2)  # Dropout for Regularization

# Final Output Layer (Regression Task)
final_output <- rnn_output %>%
  layer_dense(units = 4, activation = "linear", name = "output")  # Output: Single continuous value

# Define the Model

# Define the Model
model <- keras_model(inputs = temporal_input, outputs = final_output)

# Compile the Model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",  # Mean Squared Error for regression
  metrics = c("mae", "mse")  # Use MAE and MSE instead of "accuracy"
)

# Train the Model
model %>% fit(
  x = train_x,  # Your prepared time-series training set
  y = train_y,  # Corresponding target labels
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2  # Optional: Splitting training data for validation
)

# Plot training history
evaluation_RNN <- model %>% evaluate(test_x, test_y)
print(evaluation_RNN)
predictions_RNN <- model %>% predict(test_x)

# Print the first few predictions
print(head(predictions_RNN))
summary(model)
model$save('RNN_model_2e4.keras')
MAEs_RNN <- abs(predictions_RNN - as.matrix(test_y)) |>
  colMeans() |>
  print()

saveRDS(MAEs_RNN, file = "MAEs_RNN.rds")
saveRDS(predictions_RNN, file = "predictions_RNN.rds")
saveRDS(evaluation_RNN, file = "evaluation_RNN.rds")

#second complex RNN



# Define Temporal Input (Variable-Length Time-Series Data)
temporal_input <- layer_input(shape = c(59, 1), name = "temporal_input")  # 59 time steps, 1 feature

# Apply Masking to Ignore Padded Timesteps
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = -1)  # Ignores -1 values as padding

# **First RNN Layer with Regularization**
rnn_output1 <- masked_temporal_input %>%
  layer_simple_rnn(units = 64, return_sequences = TRUE, activation = "tanh",
                   kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.3)

# **Second RNN Layer (Captures Deeper Patterns)**
rnn_output2 <- rnn_output1 %>%
  layer_simple_rnn(units = 32, return_sequences = FALSE, activation = "tanh",
                   kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.3)

# **Fully Connected Layers for Feature Learning**
dense_output <- rnn_output2 %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 32, activation = "relu")

# **Residual Connection for Stability**
residual_output <- layer_add(list(dense_output, rnn_output2))

# **Final Output Layer (Predicts 4 Targets)**
final_output <- residual_output %>%
  layer_dense(units = 4, activation = "linear", name = "output")

# **Define the Model**
model3 <- keras_model(inputs = temporal_input, outputs = final_output)

# **Compile the Model**
model3 %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",  # Mean Squared Error for regression
  metrics = c("mae", "mse")
)

# **Train the Model**
 model3 %>% fit(
  x = train_x,
  y = train_y,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)
  )
)

# **Evaluate Model Performance**
evaluation_RNN2 <- model3 %>% evaluate(test_x, test_y)
print(evaluation_RNN2)

# **Make Predictions**
predictions_RNN2 <- model3 %>% predict(test_x)

# **Print the first few predictions**
print(head(predictions_RNN2))

# **Save the Model**
model3$save("RNN_Improved.keras")

# **Calculate MAE for Each Target**
MAEs_RNN2 <- abs(predictions_RNN2 - as.matrix(test_y)) |> colMeans() |> print()

# **Save Results**
saveRDS(MAEs_RNN2, file = "MAEs_RNN2.rds")
saveRDS(predictions_RNN2, file = "predictions_RNN2.rds")
saveRDS(evaluation_RNN2, file = "evaluation_RNN2.rds")






#second model (LSTM)

# Define Temporal Input (Variable-Length Time-Series Data)
temporal_input <- layer_input(shape = c(59, 1), name = "temporal_input")  # Input: 59 time steps, 1 feature

# Apply Masking to Ignore Padded Timesteps
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = -1)  # Ignores -1 values as padding

# **First LSTM Layer with Regularization**
lstm_output1 <- masked_temporal_input %>%
  layer_lstm(units = 64, return_sequences = TRUE, activation = "tanh",
             recurrent_dropout = 0.2, kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization()

# **Second LSTM Layer (Extracts Deeper Patterns)**
lstm_output2 <- lstm_output1 %>%
  layer_lstm(units = 32, return_sequences = FALSE, activation = "tanh",
             recurrent_dropout = 0.2, kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_batch_normalization()

# **Fully Connected Layers for Feature Learning**
dense_output <- lstm_output2 %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%  # More dropout for regularization
  layer_dense(units = 32, activation = "relu")

# **Residual Connection for Stability**
residual_output <- layer_add(list(dense_output, lstm_output2))

# **Final Output Layer (Predicts 4 Targets)**
final_output <- residual_output %>%
  layer_dense(units = 4, activation = "linear", name = "output")

# **Define the Model**
model2 <- keras_model(inputs = temporal_input, outputs = final_output)

# **Compile the Model**
model2 %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",  # Mean Squared Error for multi-output regression
  metrics = c("mae", "mse")
)

# **Train the Model**
history <- model2 %>% fit(
  x = train_x,
  y = train_y,
  epochs = 100,  # Increased epochs for better learning
  batch_size = 32,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)
  )
)

# **Evaluate Model Performance**
evaluation_LSTM <- model2 %>% evaluate(test_x, test_y)
print(evaluation_LSTM)

# **Make Predictions**
predictions_LSTM <- model2 %>% predict(test_x)

# **Print the first few predictions**
print(head(predictions_LSTM))

# **Save the Model**
model2$save("LSTM_model.keras")

# **Calculate MAE for Each Target**
MAEs_LSTM <- abs(predictions_LSTM - as.matrix(test_y)) |> colMeans() |> print()

# **Save Results**
saveRDS(MAEs_LSTM, file = "MAEs_LSTM.rds")
saveRDS(predictions_LSTM, file = "predictions_LSTM.rds")
saveRDS(evaluation_LSTM, file = "evaluation_LSTM.rds")
#
# model <- keras_model_sequential() %>%
#   layer_lstm(units = 50, input_shape = c(59, 1)) %>%
#   layer_dense(units = 4)
#
# # Compile the model
# model %>% compile(
#   optimizer = 'adam',
#   loss = 'mse'
# )
#
# summary(model)
#
# set.seed(123)
#
#
# N <- dim(input_data2)[1]
# shuffled_indices <- sample(N)
#
# # 3. Shuffle data
# shuffled_input  <- input_data2[shuffled_indices, , , drop = FALSE]
# shuffled_target <- target_data[shuffled_indices, , drop = FALSE]
#
# # 4. Define train/test split
# train_ratio <- 0.8
# train_size <- floor(train_ratio * N)
#
# # 5. Split
# train_input  <- shuffled_input[1:train_size, , , drop = FALSE]
# train_target <- shuffled_target[1:train_size, , drop = FALSE]
#
# test_input  <- shuffled_input[(train_size + 1):N, , , drop = FALSE]
# test_target <- shuffled_target[(train_size + 1):N, , drop = FALSE]
#
# train_target <- as.matrix(train_target)
# test_target  <- as.matrix(test_target)
# # 6. Confirm shapes
# dim(train_input)   # e.g. c(train_size, 59, 1)
# dim(train_target)  # e.g. c(train_size, num_targets)
# dim(test_input)    # e.g. c(N - train_size, 59, 1)
# dim(test_target)
#
# model <- keras3::keras_model_sequential() %>%
#   keras3::layer_lstm(units = 50, input_shape = c(59, 1)) %>%  # LSTM layer with 50 units
#   keras3::layer_dense(units = 4)  # Output layer with 4 units
#
# # Compile the model
# model %>% compile(
#   optimizer = 'adam',
#   loss = 'mse',
#   metric    = 'accuracy'
# )
#
# # Train the model
# history <- model %>% fit(
#   x = train_input,
#   y = train_target,
#   epochs = 3,  # Number of epochs
#   batch_size = 32,  # Batch size
#   validation_data = list(test_input, test_target)
# )
# evaluation <- model %>% evaluate(test_input, test_target)
# print(evaluation)
# predictions <- model %>% predict(test_input)
#
# # Print the first few predictions
# print(head(predictions))
# summary(model)
# model$save('lstm_model_2e3.keras')
# MAEs <- abs(predictions - as.matrix(test_target)) |>
#   colMeans() |>
#   print()
#
# saveRDS(MAEs, file = "MAEs.rds")
# saveRDS(predictions, file = "predictions.rds")
# saveRDS(evaluation, file = "evaluation.rds")
#
# model$export('lstm_model_2e3.keras')
#
#
# #complex model
# library(keras3)
#
# model2 <- keras_model_sequential() %>%
#   # 1) First LSTM layer
#   layer_lstm(
#     units = 64,
#     input_shape = c(59, 1),  # 59 timesteps, 1 feature
#     return_sequences = TRUE, # Keep output sequence for next LSTM
#     dropout = 0.2,           # Dropout on inputs
#     recurrent_dropout = 0.2  # Dropout on recurrent connections
#   ) %>%
#   # 2) Second LSTM layer
#   layer_lstm(
#     units = 32,
#     dropout = 0.2,
#     recurrent_dropout = 0.2
#   ) %>%
#   # 3) A dense hidden layer to combine features extracted by LSTMs
#   layer_dense(
#     units = 16,
#     activation = "relu"
#   ) %>%
#   # 4) Optional additional dropout
#   layer_dropout(rate = 0.2) %>%
#   # 5) Final output layer with 4 units
#   layer_dense(units = 4, activation = "linear")
#
# # Compile the model (example with MSE loss and MAE metric)
# model2 %>% compile(
#   optimizer = "adam",
#   loss = "mse",
#   metrics = "accuracy"
# )
# model2 %>% fit(
#   x = train_input,      # shape (num_samples, 59, 1)
#   y = train_target,     # shape (num_samples, 4)
#   epochs = 1,
#   batch_size = 32,
#   validation_data = list(test_input, test_target)
# )
#
# # Print summary to see layer details
# summary(model2)
# evaluation2 <- model2 %>% evaluate(test_input, test_target)
# print(evaluation)
# predictions2 <- model2 %>% predict(test_input)
#
# # Print the first few predictions
# print(head(predictions2))
# summary(model2)
# model2$save('lstm_model2_2e3.keras')
# MAEs2 <- abs(predictions2 - as.matrix(test_target)) |>
#   colMeans() |>
#   print()
#
# saveRDS(MAE2s, file = "MAE2s.rds")
# saveRDS(predictions2, file = "predictions2.rds")
# saveRDS(evaluation, file = "evaluation.rds")
#
# model2$export('lstm_model2_2e3.keras')
#
# ## model with masking:
#
#
# model3 <- keras_model_sequential() %>%
#   layer_masking(mask_value = -1, input_shape = c(59, 1)) %>% # Mask values of -1
#   layer_lstm(units = 50) %>%  # LSTM layer with 50 units
#   layer_dense(units = 4)       # Output layer with 4 units
#
# # Compile the model
# model3 %>% compile(
#   optimizer = 'adam',
#   loss = 'mse',  # Mean Squared Error for regression
#   metrics = "accuracy"
# )
#
# # Train the model
# model3 %>% fit(
#   x = train_input,
#   y = train_target,
#   epochs = 1,  # Number of epochs
#   batch_size = 32,  # Batch size
#   validation_data = list(test_input, test_target)
# )
#
# # Plot training history
# evaluation3 <- model3 %>% evaluate(test_input, test_target)
# print(evaluation3)
# predictions3 <- model3 %>% predict(test_input)
#
# # Print the first few predictions
# print(head(predictions3))
# summary(model3)
# model3$save('lstm_model3_2e3.keras')
# MAEs3 <- abs(predictions3 - as.matrix(test_target)) |>
#   colMeans() |>
#   print()
#
# saveRDS(MAEs3, file = "MAEs3.rds")
# saveRDS(predictions3, file = "predictions3.rds")
# saveRDS(evaluation3, file = "evaluation3.rds")
#
#
#
