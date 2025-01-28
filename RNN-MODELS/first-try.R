# Load necessary libraries
library(data.table)
library(epiworldR)
library(parallel)
library(keras3)
library(tensorflow)
library(abind)

# Set TensorFlow backend
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
N <- 2e4           # Number of simulations
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

# Number of cuts you want to make
n_cuts <- 2

# Minimum number of days in each interval
min_diff <- 15

# Total number of days
max_day <- 60

# Optional: Set seed for reproducibility
seed_value <- 123  # You can change this value or remove it for different results

# **Step 2: Initialize Storage for Cuts**

# Create a data frame to store the start and end days of each cut
cuts <- data.frame(start = integer(n_cuts), end = integer(n_cuts))

# **Step 3: Define a Function to Generate Non-Overlapping Cuts**

generate_cuts <- function(n_cuts, min_diff, max_day, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)  # Set seed for reproducibility
  }

  cuts <- data.frame(start = integer(n_cuts), end = integer(n_cuts))

  for (i in 1:n_cuts) {
    attempt <- 1
    max_attempts <- 1000  # Prevent infinite loops

    repeat {
      # Randomly select a start day between 1 and (max_day - min_diff)
      start <- sample(1:(max_day - min_diff), 1)

      # Randomly select an end day ensuring the interval is at least min_diff days long
      end <- sample((start + min_diff):max_day, 1)

      # Check for overlaps with previously selected cuts
      if (i > 1) {
        overlap <- any(!(end < cuts$start[1:(i-1)] | start > cuts$end[1:(i-1)]))
      } else {
        overlap <- FALSE
      }

      if (!overlap) {
        # No overlap detected; accept the cut
        cuts$start[i] <- start
        cuts$end[i] <- end
        break
      } else {
        # Overlap detected; retry
        attempt <- attempt + 1
        if (attempt > max_attempts) {
          stop("Unable to generate non-overlapping cuts after ", max_attempts, " attempts.")
        }
      }
    }
  }

  return(cuts)
}

# **Step 4: Generate Random Cuts**

# Generate the cuts using the defined function
cuts <- generate_cuts(n_cuts = n_cuts, min_diff = min_diff, max_day = max_day, seed = seed_value)

# Display the generated cuts
print("Generated Cuts:")
print(cuts)


extracted_data <- list()
data_list=matrices
# Loop through each cut and extract the corresponding rows from all matrices
for (i in 1:n_cuts) {
  # Extract the start and end days for the current cut
  start_day <- cuts$start[i]
  end_day <- cuts$end[i]

  # Define a name for the current cut
  cut_name <- paste0("Cut_", i, "_Days_", start_day, "-", end_day)

  # Inform the user about the current processing cut
  cat("Processing ", cut_name, "...\n")

  # Extract the subset of days from each matrix in `data_list`
  # This creates a list of matrices containing only the rows from start_day to end_day
  subset_list <- vector("list", length = length(data_list))

  # Optionally, name the list elements based on their index or any other naming convention
  # names(subset_list) <- paste0("Matrix_", seq_along(data_list)))

  # Iterate over each matrix in data_list
  for (i in seq_along(data_list)) {
    mat <- data_list[[i]]  # Access the i-th matrix

    # Check if the matrix has enough rows
    if (length(mat[1,1,]) >= end_day) {
      # Extract rows from start_day to end_day
      subset <- mat[,,start_day:end_day]
    } else {
      # Calculate the required number of rows
      required_rows <- end_day - start_day + 1

      # Create a matrix of NAs with the required dimensions
      subset <- (matrix(NA, nrow = required_rows, ncol = ncol(mat)))

      # Optionally, set column names if your matrices have them
      # colnames(subset) <- colnames(mat)
    }

    # Store the subset in the subset_list
    subset_list[[i]] <- t(subset)
  }
}


split_data <- function(n_cuts, title) {
  set.seed(123)
  max_day <- 60
  min_diff <- 15
  data_length <- length(data_list)
  extracted_data <- list()

  generate_cuts <- function(n_cuts, min_diff, max_day) {
    cuts <- data.frame(start = integer(n_cuts), end = integer(n_cuts))
    for (i in 1:n_cuts) {
      repeat {
        start <- sample(1:(max_day - min_diff), 1)
        end <- sample((start + min_diff):max_day, 1)
        if (i == 1 || all(end < cuts$start[1:(i-1)] | start > cuts$end[1:(i-1)])) {
          cuts$start[i] <- start
          cuts$end[i] <- end
          break
        }
      }
    }
    return(cuts)
  }

  if (n_cuts == 2) {
    cuts <- generate_cuts(2, min_diff, max_day)
    for (i in 1:2) {
      start_day <- cuts$start[i]
      end_day <- cuts$end[i]
      cut_name <- paste0("Cut_", i, "_Days_", start_day, "-", end_day)
      subset_list <- vector("list", data_length)
      for (j in seq_along(data_list)) {
        mat <- data_list[[j]]
        if (length(mat[1,1,]) >= end_day) {
          subset <- mat[,,start_day:end_day]
        } else {
          subset <- matrix(NA, nrow = (end_day - start_day + 1), ncol = ncol(mat))
        }
        subset_list[[j]] <- t(subset)
      }
      extracted_data[[cut_name]] <- subset_list
    }
  } else if (n_cuts == 1) {
    cut <- generate_cuts(1, min_diff, max_day)
    start_day <- cut$start[1]
    end_day <- cut$end[1]
    if (title == "before") {
      cut_name <- paste0("Cut_before_Days_1-", end_day)
      subset_list <- vector("list", data_length)
      for (j in seq_along(data_list)) {
        mat <- data_list[[j]]
        if (length(mat[1,1,]) >= end_day) {
          subset <- mat[,,1:end_day]
        } else {
          subset <- matrix(NA, nrow = end_day, ncol = ncol(mat))
        }
        subset_list[[j]] <- t(subset)
      }
      extracted_data[[cut_name]] <- subset_list
    } else if (title == "after") {
      cut_name <- paste0("Cut_after_Days_", start_day, "-", max_day)
      subset_list <- vector("list", data_length)
      for (j in seq_along(data_list)) {
        mat <- data_list[[j]]
        if (length(mat[1,1,]) >= start_day) {
          subset <- mat[,,start_day:max_day-1]
        } else {
          subset <- matrix(NA, nrow = (max_day - start_day + 1), ncol = ncol(mat))
        }
        subset_list[[j]] <- t(subset)
      }
      extracted_data[[cut_name]] <- subset_list
    }
  }
  return(subset_list)
}

split_data(2)

theta    <- theta[is_not_null, ]
N <- length(is_not_null)
matrices=(split_data(2))
# Create a 3D array [N, days, features]
arrays_1d <- array(dim = c(N, dim(matrices[[1]][2], dim(matrices[[1]])[3])))
for (i in seq_along(matrices)) {
  arrays_1d[i, , ] <- matrices[[i]][1, , ]
}

# Extract only the 'infected' feature (assuming it's the first feature)
arrays_1d <- arrays_1d[, , 1, drop = FALSE]  # Shape: [N, 60, 1]

# Adjust 'crate' parameter using logistic transformation
theta2 <- copy(theta)
theta2[, crate := plogis(crate / 10)]

# Save the prepared data
saveRDS(
  list(
    theta = theta2,
    simulations = arrays_1d
  ),
  file = "RNN-MODELS/sir.rds",
  compress = TRUE
)

# Load the data (optional, if running in separate sessions)
sim_results <- readRDS("RNN-MODELS/sir.rds")
theta <- sim_results$theta
arrays_1d <- sim_results$simulations
N <- dim(arrays_1d)[1]

# Split the data into training and testing sets
set.seed(331)  # For reproducibility
N_train <- floor(N * 0.7)
id_train <- 1:N_train
train <- list(
  x = arrays_1d[id_train, , ],
  y = as.matrix(theta)[id_train, ]
)

N_test <- N - N_train
id_test <- (N_train + 1):N
test <- list(
  x = arrays_1d[id_test, , ],
  y = as.matrix(theta2)[id_test, ]
)

# Define the window to keep (e.g., day 20-40)
keep_start <- 20
keep_end <- 40

# Function to Pad Sequences: Keep only specified window and pad the rest with -1
pad_sequences <- function(x, keep_start, keep_end, max_days) {
  x_padded <- array(-1, dim = c(dim(x)[1], max_days, dim(x)[3]))
  x_padded[, keep_start:keep_end, ] <- x[, keep_start:keep_end, ]
  return(x_padded)
}

# Apply padding to training and testing data
train_x_padded <- pad_sequences(train$x, keep_start, keep_end, ndays)
test_x_padded  <- pad_sequences(test$x, keep_start, keep_end, ndays)

# Verify the padding for the first sample (optional)
# print(train_x_padded[1,,])

# Build the LSTM model
model <- keras_model_sequential() %>%
  layer_masking(mask_value = -1, input_shape = c(ndays, 1)) %>%  # Mask padded values
  layer_lstm(units = 64, return_sequences = FALSE) %>%          # LSTM layer
  layer_dense(units = 64, activation = 'relu') %>%              # Fully connected layer
  layer_dropout(rate = 0.5) %>%                                 # Dropout for regularization
  layer_dense(units = ncol(theta2), activation = 'linear')      # Output layer

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mse',
  metrics = list('mae')
)

# Train the model
history <- model %>% fit(
  x = train_x_padded,
  y = train$y,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 2
)

# Save the trained model
model %>% save_model_hdf5('RNN-MODELS/sir_lstm_model.h5')

# Load the model (if needed)
# model <- load_model_hdf5("RNN-MODELS/sir_lstm_model.h5")

# Predict on test data
pred <- model %>% predict(test_x_padded)
pred_dt <- as.data.table(pred)
setnames(pred_dt, colnames(theta2))

# Calculate Mean Absolute Errors (MAEs)
maes <- colMeans(abs(pred_dt - as.matrix(test$y)))
print(maes)
