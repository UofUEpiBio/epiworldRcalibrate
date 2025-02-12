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
ndays <- 60
min_days <- 40
nsplits <- 3
masking_default <- -1.5

# Generate 'n' first
n_values <- rep(10000, N) # sample(5000:10000, N, replace = TRUE)

# Now create 'theta' and use 'n_values' correctly
theta <- data.table(
  n      = n_values,  # Population size from U(5000,10000)
  preval = sample(100:2000, N, replace = TRUE) / n_values,  # Use the correct 'n' values
  crate  = runif(N, 5, 15)+rnorm(N,0,1),  # Contact rate from U(5,20)
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
prepare_data <- function(m, max_days = ndays) {
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

    dprep <- t(diff(as.matrix(ans[-1, ])))
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
  ncores <- Sys.getenv("SLURM_NTASKS") |> as.integer() # parallel::detectCores() - 1
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
    run(m, ndays = ndays)
    ans <- prepare_data(m, max_days = ndays)

    return(ans)
  }, mc.cores = ncores)

  return(matrices)
}



seeds <- sample.int(.Machine$integer.max, N, TRUE)
ncores=40

matrices <- parallel::mclapply(1:N, FUN = function(i) {


  set.seed(seeds[i])

  m <- theta[i,
             ModelSIRCONN(
               "mycon",
               prevalence        = preval,
               contact_rate      = crate,
               transmission_rate = ptran,
               recovery_rate     = recov,
               n                 = n
             )
  ]

  # Avoids printing
  verbose_off(m)

  run(m, ndays = ndays)

  # Using prepare_data
  ans <- prepare_data(m)

  ans

}, mc.cores = ncores)
# Run Simulations
 # matrices <- run_simulations(N, theta)
# # Remove NULL values from matrices
# valid_indices <- which(!sapply(matrices, is.null))
# matrices <- matrices[valid_indices]
#  theta <- theta[valid_indices, ]  # Ensure theta stays in sync with matrices
# Now

# Filter out simulations with errors or NA values
is_not_null <- intersect(
  which(!sapply(matrices, inherits, what = "error")),
  which(!sapply(matrices, function(x) any(is.na(x))))
)
matrices <- matrices[is_not_null]
theta    <- theta[is_not_null,]
N=length(matrices)


augment_ts <- function(
    x, n,
    min_size = NULL,
    max_size = NULL,
    fill = masking_default,
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
N <- length(is_not_null)

# Get dimensions from the first matrix, ignoring the first dimension

arrays_1d <- array(dim = c(N, dim(matrices[[1]][1,,])))
for (i in seq_along(matrices))
  arrays_1d[i,,] <- matrices[[i]][1,,]
arrays_1d <- arrays_1d[,1,,drop=FALSE]
N     <- dim(arrays_1d)[1]
# Convert to a list of time-series vectors
data_list <- lapply(1:N, function(i) as.vector(arrays_1d[i, , ]))

# Apply time series augmentation
augmented_data <- augment_ts(
  data_list,
  n = nsplits,
  min_size = floor(ndays/2),
  max_size = ndays
)

# Flatten augmented data
split_pad <- unlist(augmented_data, recursive = FALSE)

# Determine the number of augmented samples
N_aug <- length(split_pad)

# Determine the length of each augmented time-series vector.
# (This is analogous to using dim(matrices[[1]][1, , ]) in the teacher's code.)
time_series_length <- length(split_pad[[1]])

# Preallocate an array.
# Here we mimic teacher's structure: (number of samples, number of rows, number of columns).
arrays_1d <- array(dim = c(N_aug, 1, time_series_length))

# Fill the array with your augmented data.
for (i in seq_along(split_pad)) {
  # Each augmented time series is inserted as a 1 x time_series_length matrix.
  arrays_1d[i, , ] <- split_pad[[i]]
}

# Expand `theta` to match the augmented data
theta_expanded <- theta[rep(seq_len(.N), each = nsplits)]
theta_pad <- theta_expanded

set.seed(123)
# Filter only required columns for model training
theta_filtered <- theta_pad[, .(n, preval)]  # Model Inputs
theta_target   <- theta_pad[, .(recov, crate, ptran)]  # Model Outputs
theta_target[,2] <- plogis(as.matrix(theta_target[,2]/10))
# Convert to matrices for TensorFlow
# input_data <- array(unlist(split_pad), dim = c(length(split_pad), 60, 1))
target_data <- as.matrix(theta_target)



arrays_1d[1,,]
N_train <- floor(nrow(arrays_1d) * .7)
id_train <- 1:N_train
train <- list(
  x = array_reshape(
    arrays_1d[id_train,,], dim = c(N_train, dim(arrays_1d)[-1])
  ),
  y =  array_reshape(
    as.matrix(theta_target)[id_train,], dim = c(N_train, ncol(theta_target)))
)
train$y[1,]
train$x[1000,,]
train$x[999,,]
N_test <- nrow(arrays_1d) - N_train
id_test <- (N_train + 1):nrow(arrays_1d)

test <- list(
  x = array_reshape(arrays_1d[id_test,,], dim = c(N_test, dim(arrays_1d)[-1])),
  y = array_reshape(as.matrix(theta_target)[id_test,], dim = c(N_test, ncol(theta_target)))
)



tensorflow::tf$keras$backend$clear_session()
input_shape = c(dim(arrays_1d)[-1])
# Define Temporal Input (Time-Series Data)
temporal_input <- layer_input(shape = input_shape, name = "temporal_input")

# Apply Masking to Ignore Padded Timesteps (-1)
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = masking_default)

# RNN Layer for Processing Time-Series Data
rnn_output <- masked_temporal_input %>%
  layer_lstm(units = 64, activation = "tanh", return_sequences = FALSE) %>%
  layer_dropout(rate = 0.2)

# Define Metadata Input (Static Features: n, preval)
# metadata_input <- layer_input(shape = c(2), name = "metadata_input")

# Concatenate RNN and Metadata Processing
merged <- layer_concatenate(list(rnn_output))

# Final Output Layer (Regression Task)
final_output <- merged %>%
  layer_dense(units = 3, activation = "linear", name = "output")

# Define the Model (Now it accepts two inputs)
model <- keras_model(inputs = list(temporal_input), outputs = final_output)

# Compile the Model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",
  metrics = c("mae", "mse")
)

# Train the Model
model %>% fit(
  x = train$x,
  y = train$y,
  epochs = 50,
  # batch_size = 32,
  validation_split = 0.2
)

#LSTM Model
library(keras)

tensorflow::tf$keras$backend$clear_session()
# Define Temporal Input (Time-Series Data)
temporal_input <- layer_input(shape = input_shape, name = "temporal_input")  # 60 time steps, 1 feature

# Apply Masking to Ignore Padded Timesteps (-1.5)
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = -1.5)

# LSTM Layer for Processing Time-Series Data
lstm_output <- masked_temporal_input %>%
  layer_lstm(
    units = 128,  # Increased units for better learning capacity
    activation = "tanh",
    return_sequences = FALSE,  # We only need final hidden state
    kernel_regularizer = regularizer_l2(0.001)  # L2 Regularization to prevent overfitting
  ) %>%
  layer_dropout(rate = 0.2)  # Regularization for better generalization

# Final Output Layer (Regression Task)
final_output <- lstm_output %>%
  layer_dense(units = 64, activation = "relu") %>%  # Intermediate dense layer for better representation
  layer_dropout(rate = 0.1) %>%  # Slight dropout to stabilize training
  layer_dense(units = 3, activation = "linear", name = "output")  # Output layer for regression

# Define the Model
model <- keras_model(inputs = temporal_input, outputs = final_output)

# Compile the Model with MAE Loss
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0005),  # Lower LR for stability
  loss = "mae",  # Mean Absolute Error for more robust regression
  metrics = c("mae", "mse")
)

# Train the Model
model %>% fit(
  x = train$x,  # Only time-series input
  y = train$y,  # Targets (theta values)
  epochs = 20,  # Increased epochs for better convergence
  batch_size = 64,  # Larger batch size for stable updates
  validation_split = 0.2
)

##########new way of getting each param separately
tensorflow::tf$keras$backend$clear_session()
input_shape = c(dim(arrays_1d)[-1])
# Define Temporal Input (Time-Series Data)
temporal_input <- layer_input(shape = input_shape, name = "temporal_input")

# Apply Masking to Ignore Padded Timesteps (-1)
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = masking_default)

# RNN Layer for Processing Time-Series Data
rnn_output <- masked_temporal_input %>%
  layer_lstm(units = 64, activation = "tanh", return_sequences = FALSE) %>%
  layer_dropout(rate = 0.2)
merged <- layer_concatenate(list(rnn_output))
recov_output <- merged %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear", name = "recov_output")

pcrate_output <- merged %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear", name = "pcrate_output")

ptran_output <- merged %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear", name = "ptran_output")

model <- keras_model(
  inputs = list(temporal_input),
  outputs = list(recov_output, pcrate_output, ptran_output)
)

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = list("mse", "mse", "mse"),
  loss_weights = c(1, 2, 1), # Adjust weights to prioritize pcrate
  metrics = list(
    "recov_output" = "mae",   # MAE for recov_output
    "pcrate_output" = "mae",  # MAE for pcrate_output
    "ptran_output" = "mae"    # MAE for ptran_output
  )
)
callbacks <- list(
  callback_early_stopping(monitor = "val_loss", patience = 5, restore_best_weights = TRUE)
)
sima <- list(
  recov = train$y[, 1],    # First column for recov_output
  pcrate = train$y[, 2],   # Second column for pcrate_output
  ptran = train$y[, 3]     # Third column for ptran_output
)
str(sima)
model %>% fit(
  x = train$x,
  y = sima,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = callbacks
)


######################################################################
# Evaluate Model
test_sima=list(
  recov = test$y[, 1],    # First column for recov_output
  pcrate = test$y[, 2],   # Second column for pcrate_output
  ptran = test$y[, 3]     # Third column for ptran_output
)
str(test_sima)
evaluation_RNN <- model %>% evaluate(test$x, test_sima)

evaluation_RNN <- model %>% evaluate(test$x, test$y)
print(evaluation_RNN)
# Predict Using the Model
predictions_RNN <- model %>% predict(test$x)
max(predictions_RNN[,2])

MAEs_RNN <- abs(predictions_RNN - as.matrix(test$y)) |>
  colMeans() |>
  print()
# Save Model
model$save('RNN_model_with_metadata_10k_60days_corrected2.keras')
summary(model)

pred=as.data.table(predictions_RNN)

pred[, id := 1L:.N]
pred=as.matrix(pred)
# pred[, 2] <- qlogis(as.numeric(pred[, 2]))*10

pred <- as.data.table(pred)

# Convert `id` column to integer (if needed)
pred[, id := as.integer(id)]
names(pred)=c("recov","crate","ptran","id")
# Melt properly using `data.table::melt()`
pred_long <- melt(pred, id.vars = "id",, value.name = "value")



theta_long <- test$y |> as.data.table()
colnames(theta_long)=c("recov","crate","ptran")
setnames(theta_long, names(theta_long))
theta_long[, id := 1L:.N]
# theta_long[, crate := qlogis(crate)*10]
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
N_train=N_train
N=nrow(arrays_1d)
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
    caption  = "Predictions made using a RNN as implemented with loss function MAE."

  )


##mimicing CNN:

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
N <- 2e4
ndays <- 60
min_days <- 40
nsplits <- 3
masking_default <- -1.5

n <- 5000 # sample(5000:10000, N, replace = TRUE)
theta <- data.table(
  preval = sample((100:2000)/n, N, TRUE),
  crate  = rgamma(N, 5, 1),    # Mean 10
  ptran  = rbeta(N, 3, 7),         # Mean 3/(3 + 7) = 0.3
  prec   = rbeta(N, 10, 10*2 - 10) # Mean 10 / (10 * 2 - 10) = .5
)
# Print first few rows to check
head(theta)


# Function to Prepare Data for TensorFlow Model: General SIR Data Preparation
prepare_data <- function(m, max_days = ndays) {
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

    dprep <- t(diff(as.matrix(ans[-1, ])))
    ans_array <- array(dim = c(1, dim(dprep)))
    ans_array[1, , ] <- dprep

    tensorflow::array_reshape(ans_array, dim = c(1, dim(dprep)))
  }, error = function(e) e)

  if (inherits(ans, "error")) {
    return(ans)
  }

  return(ans_array)
}

seeds <- sample.int(.Machine$integer.max, N, TRUE)
ncores=40

matrices <- parallel::mclapply(1:N, FUN = function(i) {


  set.seed(seeds[i])

  m <- theta[i,
             ModelSIRCONN(
               "mycon",
               prevalence        = preval,
               contact_rate      = crate,
               transmission_rate = ptran,
               recovery_rate     = prec,
               n                 = n
             )
  ]

  # Avoids printing
  verbose_off(m)

  run(m, ndays = ndays)

  # Using prepare_data
  ans <- prepare_data(m)

  ans

}, mc.cores = ncores)
#
# # Function to Run SIR Model Simulations in Parallel
# run_simulations <- function(N, theta) {
#   ncores <- Sys.getenv("SLURM_NTASKS") |> as.integer() # parallel::detectCores() - 1
#   seeds <- sample(1:1e6, N, replace = FALSE)
#
#   matrices <- parallel::mclapply(1:N, function(i) {
#     set.seed(seeds[i])
#     m <- epiworldR::ModelSIRCONN(
#       "mycon",
#       prevalence        = theta$preval[i],
#       contact_rate      = theta$crate[i],
#       transmission_rate = theta$ptran[i],
#       recovery_rate     = theta$recov[i],
#       n                 = theta$n[i]
#     )
#
#     verbose_off(m)
#     run(m, ndays = ndays)
#     ans <- prepare_data(m, max_days = ndays)
#
#     return(ans)
#   }, mc.cores = ncores)
#
#   return(matrices)
# }

# Run Simulations
# matrices <- run_simulations(N, theta)
# Remove NULL values from matrices
# valid_indices <- which(!sapply(matrices, is.null))
# matrices <- matrices[valid_indices]
# theta <- theta[valid_indices, ]  # Ensure theta stays in sync with matrices
# Now

# Filter out simulations with errors or NA values
is_not_null <- intersect(
  which(!sapply(matrices, inherits, what = "error")),
  which(!sapply(matrices, function(x) any(is.na(x))))
)
matrices <- matrices[is_not_null]
theta    <- theta[is_not_null,]
N=length(matrices)


augment_ts <- function(
    x, n,
    min_size = NULL,
    max_size = NULL,
    fill = masking_default,
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

arrays_1d <- array(dim = c(N, dim(matrices[[1]][1,,])))
for (i in seq_along(matrices))
  arrays_1d[i,,] <- matrices[[i]][1,,]
arrays_1d <- arrays_1d[,1,,drop=FALSE]
N <- dim(arrays_1d)[1]

# Convert to a list of time-series vectors
data_list <- lapply(1:N, function(i) as.vector(arrays_1d[i, , ]))

# Apply time series augmentation
augmented_data <- augment_ts(
  data_list,
  n = nsplits,
  min_size = floor(ndays/2),
  max_size = ndays
)

# Flatten augmented data
split_pad <- unlist(augmented_data, recursive = FALSE)

# Determine the number of augmented samples
N_aug <- length(split_pad)

# Determine the length of each augmented time-series vector.
# (This is analogous to using dim(matrices[[1]][1, , ]) in the teacher's code.)
time_series_length <- length(split_pad[[1]])

# Preallocate an array.
# Here we mimic teacher's structure: (number of samples, number of rows, number of columns).
arrays_1d <- array(dim = c(N_aug, 1, time_series_length))

# Fill the array with your augmented data.
for (i in seq_along(split_pad)) {
  # Each augmented time series is inserted as a 1 x time_series_length matrix.
  arrays_1d[i, , ] <- split_pad[[i]]
}

# Expand `theta` to match the augmented data
theta_expanded <- theta[rep(seq_len(.N), each = nsplits)]
theta_pad <- theta_expanded

set.seed(123)
# Filter only required columns for model training
theta_target   <- theta_pad[, .( crate, ptran,prec)]  # Model Outputs
theta_target[,1] <- plogis(as.matrix(theta_target[,1]/10))
# Convert to matrices for TensorFlow
# input_data <- array(unlist(split_pad), dim = c(length(split_pad), 60, 1))
target_data <- as.matrix(theta_target)




N_train <- floor(nrow(arrays_1d) * .7)
id_train <- 1:N_train
train <- list(
  x = array_reshape(
    arrays_1d[id_train,,], dim = c(N_train, dim(arrays_1d)[-1])
  ),
  y =  array_reshape(
    as.matrix(theta_target)[id_train,], dim = c(N_train, ncol(theta_target)))
)
train$y[1,]
train$x[1,,]

hist(qlogis(train$y[,1])*10)
N_test <- nrow(arrays_1d) - N_train
id_test <- (N_train + 1):nrow(arrays_1d)

test <- list(
  x = array_reshape(arrays_1d[id_test,,], dim = c(N_test, dim(arrays_1d)[-1])),
  y = array_reshape(as.matrix(theta_target)[id_test,], dim = c(N_test, ncol(theta_target)))
)

# # Ensure train$x is correctly shaped (batch, 60, 1)
# train$x <- aperm(train$x, c(1, 3, 2))
#
# # Ensure test$x is correctly shaped (batch, 60, 1)
# test$x <- aperm(test$x, c(1, 3, 2))

tensorflow::tf$keras$backend$clear_session()
input_shape = c(dim(arrays_1d)[-1])
# Define Temporal Input (Time-Series Data)
temporal_input <- layer_input(shape =input_shape , name = "temporal_input")

# Apply Masking to Ignore Padded Timesteps (-1)
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = masking_default)

# RNN Layer for Processing Time-Series Data
rnn_output <- masked_temporal_input %>%
  layer_simple_rnn(units = 32, activation = "tanh", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.2)

# Define Metadata Input (Static Features: n, preval)
# metadata_input <- layer_input(shape = c(2), name = "metadata_input")

# Concatenate RNN and Metadata Processing
merged <- layer_concatenate(list(rnn_output))

# Final Output Layer (Regression Task)
final_output <- merged %>%
  layer_dense(units = 3, activation = "linear", name = "output")

# Define the Model (Now it accepts two inputs)
model <- keras_model(inputs = list(temporal_input), outputs = final_output)

# Compile the Model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",
  metrics = c("mae", "mse")
)

# Train the Model
model %>% fit(
  x = train$x,
  y = train$y,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2
)




library(keras)

tensorflow::tf$keras$backend$clear_session()
# Define Temporal Input (Time-Series Data)
temporal_input <- layer_input(shape = input_shape, name = "temporal_input")  # 60 time steps, 1 feature

# Apply Masking to Ignore Padded Timesteps (-1.5)
masked_temporal_input <- temporal_input %>%
  layer_masking(mask_value = -1.5)

# LSTM Layer for Processing Time-Series Data
lstm_output <- masked_temporal_input %>%
  layer_lstm(
    units = 128,  # Increased units for better learning capacity
    activation = "tanh",
    return_sequences = FALSE,  # We only need final hidden state
    kernel_regularizer = regularizer_l2(0.001)  # L2 Regularization to prevent overfitting
  ) %>%
  layer_dropout(rate = 0.2)  # Regularization for better generalization

# Final Output Layer (Regression Task)
final_output <- lstm_output %>%
  layer_dense(units = 64, activation = "relu") %>%  # Intermediate dense layer for better representation
  layer_dropout(rate = 0.1) %>%  # Slight dropout to stabilize training
  layer_dense(units = 3, activation = "linear", name = "output")  # Output layer for regression

# Define the Model
model <- keras_model(inputs = temporal_input, outputs = final_output)

# Compile the Model with MAE Loss
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0005),  # Lower LR for stability
  loss = "mae",  # Mean Absolute Error for more robust regression
  metrics = c("mae", "mse")
)

# Train the Model
model %>% fit(
  x = train$x,  # Only time-series input
  y = train$y,  # Targets (theta values)
  epochs = 100,  # Increased epochs for better convergence
  batch_size = 64,  # Larger batch size for stable updates
  validation_split = 0.2
)

# Evaluate Model
evaluation_RNN <- model %>% evaluate(test$x, test$y)
print(evaluation_RNN)
# Predict Using the Model
predictions_RNN <- model %>% predict(test$x)
max(predictions_RNN[,1])

MAEs_RNN <- abs(predictions_RNN - as.matrix(test$y)) |>
  colMeans() |>
  print()
# Save Model
model$save('LSTM_model_with_metadata_10k_60days_corrected.keras')
summary(model)

pred=as.data.table(predictions_RNN)

pred[, id := 1L:.N]
pred=as.matrix(pred)
pred[, 1] <- qlogis(as.numeric(pred[, 1]))*10

pred <- as.data.table(pred)

# Convert `id` column to integer (if needed)
pred[, id := as.integer(id)]
names(pred)=c("crate","ptran","prec","id")
# Melt properly using `data.table::melt()`
pred_long <- melt(pred, id.vars = "id",, value.name = "value")



theta_long <- test$y |> as.data.table()
colnames(theta_long)=c("crate","ptran","prec")
setnames(theta_long, names(theta_long))
theta_long[, id := 1L:.N]
theta_long[, crate := qlogis(crate)*10]
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
  variable = c("crate","ptran","prec"),
  Name     = paste(
    c("Contact Rate", "p(transmission)","P(recovery)"),
    sprintf("(MAE: %.2f)", MAEs_RNN)
  )
)

alldat_wide <- merge(alldat_wide, vnames, by = "variable")
N_train=N_train
N=nrow(arrays_1d)
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
    caption  = "Predictions made using a LSTM as implemented with loss function MAE."

  )



