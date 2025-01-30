# Load necessary libraries
library(data.table)
library(epiworldR)
library(parallel)
library(keras3)
library(tensorflow)
library(abind)
use_session_with_seed(331, disable_gpu = FALSE, disable_parallel_cpu = FALSE)

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
N <- 2e3          # Number of simulations
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

sources=theta
min_window_size=15
max_window_size=59



library(parallel)

# 1. pad_window Function
pad_window <- function(window, target_rows = 59) {
  current_rows <- nrow(window)

  if (current_rows < target_rows) {
    padding_rows <- target_rows - current_rows
    padding <- matrix(-1, nrow = padding_rows, ncol = ncol(window))

    if (!is.null(colnames(window))) {
      colnames(padding) <- colnames(window)
    }

    window <- rbind(window, padding)
  }

  return(window)
}

# 2. generate_all_windows Function
generate_all_windows <- function(matrix, source, min_window_size, max_window_size, target_rows = 59) {
  windows <- list()

  rows <- dim(matrix)[3]
  cols <- dim(matrix)[2]

  for (window_size in min_window_size:max_window_size) {
    num_windows_for_size <- rows - window_size + 1

    if (num_windows_for_size <= 0) {
      next
    }

    third <- floor(num_windows_for_size / 3)

    # First third
    for (i in 1:third) {
      start_col <- sample(0:(third - 1), 1)
      window_cols <- (start_col + 1):(start_col + window_size)
      if (max(window_cols) > rows) next

      window <- t(matrix[, , window_cols])
      window <- pad_window(window, target_rows)
      windows <- append(windows, list(list(window = window, source = source)))
    }

    # Second third
    for (i in 1:third) {
      start_col <- sample((rows - window_size - third + 1):(rows - window_size), 1)
      window_cols <- (start_col + 1):(start_col + window_size)
      if (max(window_cols) > rows || start_col < 0) next

      window <- t(matrix[, , window_cols])
      window <- pad_window(window, target_rows)
      windows <- append(windows, list(list(window = window, source = source)))
    }

    # Remaining windows
    remaining_windows <- num_windows_for_size - 2 * third
    if (remaining_windows > 0) {
      for (i in 1:remaining_windows) {
        start_col <- sample(third:(rows - window_size - third), 1)
        window_cols <- (start_col + 1):(start_col + window_size)
        if (max(window_cols) > rows || start_col < 0) next

        window <- t(matrix[, , window_cols])
        window <- pad_window(window, target_rows)
        windows <- append(windows, list(list(window = window, source = source)))
      }
    }
  }

  return(windows)
}

# 3. process_sliding_windows_mclapply Function (Parallel)
process_sliding_windows_mclapply <- function(matrices,
                                             sources,
                                             min_window_size,
                                             max_window_size,
                                             target_rows = 59,
                                             ncores = 1) {
  all_windows_list <- mclapply(seq_along(matrices), function(matrix_idx) {
    matrix <- matrices[[matrix_idx]]
    source <- sources[matrix_idx, ]

    # Generate windows for this matrix
    generate_all_windows(matrix, source, min_window_size, max_window_size, target_rows)
  }, mc.cores = ncores)

  # Flatten the resulting list
  all_windows <- do.call(c, all_windows_list)
  return(all_windows)
}

# 4. Actually run the parallel sliding windows and save
split_pad <- process_sliding_windows_mclapply(
  matrices         = matrices,
  sources          = sources,
  min_window_size  = 15,
  max_window_size  = 59,
  target_rows      = 59,
  ncores           = 20  # choose cores
)

# Option 1: Modify 'split_pad' *after* it's created
for (i in seq_along(split_pad)) {
  # Keep only the first column (as a column matrix)
  split_pad[[i]]$window <- split_pad[[i]]$window[, 1, drop = FALSE]
}

saveRDS(split_pad, file = "RNN-MODELS/split_pad.rds", compress = TRUE)
length(split_pad)
input_data=split_pad


input_data2 <- array(unlist(input_data), dim = c(2082017, 59, 1))

# Extract the target data
target_data <- do.call(rbind, lapply(input_data, function(x) x$source))

# Check the shapes
print(dim(input_data2))  # Should be [2082017, 59, 1]
print(dim(target_data))  # Should be [2082017, 4]

model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(59, 1)) %>%
  layer_dense(units = 4)

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mse'
)

summary(model)

set.seed(123)


N <- dim(input_data2)[1]
shuffled_indices <- sample(N)

# 3. Shuffle data
shuffled_input  <- input_data2[shuffled_indices, , , drop = FALSE]
shuffled_target <- target_data[shuffled_indices, , drop = FALSE]

# 4. Define train/test split
train_ratio <- 0.8
train_size <- floor(train_ratio * N)

# 5. Split
train_input  <- shuffled_input[1:train_size, , , drop = FALSE]
train_target <- shuffled_target[1:train_size, , drop = FALSE]

test_input  <- shuffled_input[(train_size + 1):N, , , drop = FALSE]
test_target <- shuffled_target[(train_size + 1):N, , drop = FALSE]

train_target <- as.matrix(train_target)
test_target  <- as.matrix(test_target)
# 6. Confirm shapes
dim(train_input)   # e.g. c(train_size, 59, 1)
dim(train_target)  # e.g. c(train_size, num_targets)
dim(test_input)    # e.g. c(N - train_size, 59, 1)
dim(test_target)

model <- keras3::keras_model_sequential() %>%
  keras3::layer_lstm(units = 50, input_shape = c(59, 1)) %>%  # LSTM layer with 50 units
  keras3::layer_dense(units = 4)  # Output layer with 4 units

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mse',
  metric    = 'accuracy'
)

# Train the model
history <- model %>% fit(
  x = train_input,
  y = train_target,
  epochs = 3,  # Number of epochs
  batch_size = 32,  # Batch size
  validation_data = list(test_input, test_target)
)
evaluation <- model %>% evaluate(test_input, test_target)
print(evaluation)
predictions <- model %>% predict(test_input)

# Print the first few predictions
print(head(predictions))
summary(model)
model$save('lstm_model_2e3.keras')
MAEs <- abs(predictions - as.matrix(test_target)) |>
  colMeans() |>
  print()

saveRDS(MAEs, file = "MAEs.rds")
saveRDS(predictions, file = "predictions.rds")
saveRDS(evaluation, file = "evaluation.rds")

model$export('lstm_model_2e3.keras')

#
# pad_window <- function(window, target_rows = 59) {
#   current_rows <- nrow(window)
#
#   if (current_rows < target_rows) {
#     padding_rows <- target_rows - current_rows
#     padding <- matrix(-1, nrow = padding_rows, ncol = ncol(window))
#
#     if (!is.null(colnames(window))) {
#       colnames(padding) <- colnames(window)
#     }
#
#     window <- rbind(window, padding)
#   }
#
#   return(window)
# }
#
# # 2. generate_all_windows Function
# generate_all_windows <- function(matrix, source, min_window_size, max_window_size, target_rows = 59) {
#   windows <- list()
#
#   rows <- dim(matrix)[3]
#   cols <- dim(matrix)[2]
#
#   for (window_size in min_window_size:max_window_size) {
#     num_windows_for_size <- rows - window_size + 1
#
#     if (num_windows_for_size <= 0) {
#       next
#     }
#
#     third <- floor(num_windows_for_size / 3)
#
#     # First third windows
#     for (i in 1:third) {
#       start_col <- sample(0:(third - 1), 1)
#       window_cols <- (start_col + 1):(start_col + window_size)
#
#       if (max(window_cols) > rows) next
#       window <- t(matrix[, , window_cols])
#
#       window <- pad_window(window, target_rows)
#
#       windows <- append(windows, list(list(window = window, source = source)))
#     }
#
#     # Second third windows
#     for (i in 1:third) {
#       start_col <- sample((rows - window_size - third + 1):(rows - window_size), 1)
#       window_cols <- (start_col + 1):(start_col + window_size)
#
#       if (max(window_cols) > rows || start_col < 0) next
#       window <- t(matrix[, , window_cols])
#
#       window <- pad_window(window, target_rows)
#
#       windows <- append(windows, list(list(window = window, source = source)))
#     }
#
#     # Remaining windows
#     remaining_windows <- num_windows_for_size - 2 * third
#     if (remaining_windows > 0) {
#       for (i in 1:remaining_windows) {
#         start_col <- sample(third:(rows - window_size - third), 1)
#         window_cols <- (start_col + 1):(start_col + window_size)
#
#         if (max(window_cols) > rows || start_col < 0) next
#         window <- t(matrix[, , window_cols])
#
#         window <- pad_window(window, target_rows)
#
#         windows <- append(windows, list(list(window = window, source = source)))
#       }
#     }
#   }
#
#   return(windows)
# }
#
# # 3. process_sliding_windows Function
# process_sliding_windows <- function(matrices, sources, min_window_size, max_window_size, target_rows = 59) {
#   all_windows <- list()
#
#   for (matrix_idx in seq_along(matrices)) {
#     matrix <- matrices[[matrix_idx]]
#     source <- sources[matrix_idx, ]
#
#     windows <- generate_all_windows(matrix, source, min_window_size, max_window_size, target_rows)
#
#     all_windows <- append(all_windows, windows)
#   }
#
#   return(all_windows)
# }
# split_pad=process_sliding_windows(matrices=matrices,sources=sources,
#                                   min_window_size=15,max_window_size = 59,target_rows = 59)
# saveRDS(split_pad,
#         file = "RNN-MODELS/split_pad.rds",
#         compress = TRUE
# )
#
# #spiliting the dataset
# #
# #
# # process_sliding_windows <- function(matrices, sources, min_window_size, max_window_size) {
# #   all_windows <- list()
# # matrix_idx=1
# #   for (matrix_idx in seq_along(matrices)) {
# #     matrix <- matrices[[matrix_idx]]
# #
# #     rows <- dim(matrix)[3]
# #     cols <- dim(matrix)[2]
# #     source <- sources[matrix_idx,]
# #
# #     for (window_size in min_window_size:max_window_size) {
# #       num_windows_for_size <- rows - window_size + 1
# #
# #       third <- floor(num_windows_for_size / 3) # Use floor to handle integer division
# #
# #       for (i in 1:third) {
# #         start_col <- sample(0:(third - 1), 1) # R's sample handles random integers differently
# #         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
# #         # R indexing starts at 1
# #         all_windows <- append(all_windows, list(list(window = window, source = source))) # Nested lists for (window, source) tuple
# #       }
# #
# #       for (i in 1:third) {
# #         start_col <- sample((rows - window_size - third):(rows - window_size), 1)
# #         window <- t(matrix[, ,(start_col + 1):(start_col + window_size)])
# #         all_windows <- append(all_windows, list(list(window = window, source = source)))
# #       }
# #
# #       for (i in 1:(num_windows_for_size - 2 * third)) {
# #         start_col <- sample(third:(rows - window_size - third), 1)
# #         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
# #         all_windows <- append(all_windows, list(list(window = window, source = source)))
# #       }
# #     }
# #   }
# #
# #   return(all_windows)
# # }
# # process_sliding_windows(matrices=arrays_1d,sources,min_window_size=15,max_window_size = 59)
#
#
#
# process_sliding_windows_parallel <- function(matrices, sources, min_window_size, max_window_size, ncores) {
#   # Run the same code used in process_sliding_windows for each matrix, but in parallel.
#   all_windows_list <- parallel::mclapply(seq_along(matrices), function(matrix_idx) {
#     matrix <- matrices[[matrix_idx]]
#
#     rows <- dim(matrix)[3]
#     cols <- dim(matrix)[2]
#     source <- sources[matrix_idx,]
#
#     local_windows <- list()
#     for (window_size in min_window_size:max_window_size) {
#       num_windows_for_size <- rows - window_size + 1
#       third <- floor(num_windows_for_size / 3)  # Same logic: integer division
#
#       # 1) First set of windows
#       for (i in 1:third) {
#         start_col <- sample(0:(third - 1), 1)
#         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
#         local_windows <- append(local_windows, list(list(window = window, source = source)))
#       }
#
#       # 2) Second set of windows
#       for (i in 1:third) {
#         start_col <- sample((rows - window_size - third):(rows - window_size), 1)
#         window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
#         local_windows <- append(local_windows, list(list(window = window, source = source)))
#       }
#
#       # 3) Remaining windows
#       for (i in 1:(num_windows_for_size - 2 * third)) {
#         start_col <- sample(third:(rows - window_size - third), 1)
#         window <- (matrix[,, (start_col + 1):(start_col + window_size)])
#         local_windows <- append(local_windows, list(list(window = window, source = source)))
#       }
#     }
#
#     return(local_windows)
#   }, mc.cores = ncores)
#
#   # Combine (flatten) all lists of windows into a single list
#   all_windows <- do.call(c, all_windows_list)
#   return(all_windows)
# }
#
# all_windows=process_sliding_windows_parallel(matrices, sources, min_window_size, max_window_size, ncores)
#
# # 1. Define the padarray() function in your script
# padarray <- function(x, pad_dims, pad_value = 0, pad_direction = "post") {
#   old_dims <- dim(x)
#   new_dims <- old_dims + pad_dims
#   new_matrix <- matrix(pad_value, nrow = new_dims[1], ncol = new_dims[2])
#   if (pad_direction == "post") {
#     new_matrix[1:old_dims[1], 1:old_dims[2]] <- x
#   } else {
#     stop("Only 'post' padding is implemented.")
#   }
#   return(new_matrix)
# }
#
# # 2. Define or load your pad_windows() function exactly as before:
# pad_windows <- function(all_windows, target_cols = 59) {
#   padded_windows <- list()
#   sources <- list()
#
#   for (i in seq_along(all_windows)) {
#     window <- all_windows[[i]][[1]]
#     source <- all_windows[[i]][[2]]
#
#     window <- as.matrix(window)
#     rows <- nrow(window)
#     cols <- ncol(window)
#
#     padding_size <- target_cols - cols
#
#     if (padding_size > 0) {
#       padded_window <- padarray(window, c(0, padding_size), -1, "post")
#     } else {
#       padded_window <- window
#     }
#
#     padded_windows[[i]] <- padded_window
#     sources[[i]] <- source
#   }
#
#   padded_windows <- array(
#     unlist(padded_windows),
#     dim = c(
#       length(padded_windows),
#       nrow(padded_windows[[1]]),
#       ncol(padded_windows[[1]])
#     )
#   )
#
#   sources <- unlist(sources)
#
#   return(list(padded_windows = padded_windows, sources = sources))
# }
#
# # 3. Now call pad_windows() without error:
# # my_result <- pad_windows(my_all_windows)
#
# padded_winodws=pad_windows(all_windows, target_cols = 59)
#
#
#
#
#
