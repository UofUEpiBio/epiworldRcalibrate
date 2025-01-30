# Load necessary libraries
library(data.table)
library(epiworldR)
library(parallel)
library(keras3)
library(tensorflow)
library(abind)
#function to prepare datasets
source("RNN-MODELS/dataprep.R")
# function to generate parameters
source("RNN-MODELS/generate_theta.R")
# function to simulate dataset
source("RNN-MODELS/run_simulations.R")
# Set TensorFlow backend
#use_session_with_seed(331, disable_gpu = FALSE, disable_parallel_cpu = FALSE)


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
#spiliting the dataset

process_sliding_windows <- function(matrices, sources, min_window_size, max_window_size) {
  all_windows <- list()

  for (matrix_idx in seq_along(matrices)) {
    matrix <- matrices[[matrix_idx]]

    rows <- dim(matrix)[3]
    cols <- dim(matrix)[2]
    source <- sources[matrix_idx,]

    for (window_size in min_window_size:max_window_size) {
      num_windows_for_size <- rows - window_size + 1

      third <- floor(num_windows_for_size / 3) # Use floor to handle integer division

      for (i in 1:third) {
        start_col <- sample(0:(third - 1), 1) # R's sample handles random integers differently
        window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
        # R indexing starts at 1
        all_windows <- append(all_windows, list(list(window = window, source = source))) # Nested lists for (window, source) tuple
      }

      for (i in 1:third) {
        start_col <- sample((rows - window_size - third):(rows - window_size), 1)
        window <- t(matrix[, ,(start_col + 1):(start_col + window_size)])
        all_windows <- append(all_windows, list(list(window = window, source = source)))
      }

      for (i in 1:(num_windows_for_size - 2 * third)) {
        start_col <- sample(third:(rows - window_size - third), 1)
        window <- t(matrix[,, (start_col + 1):(start_col + window_size)])
        all_windows <- append(all_windows, list(list(window = window, source = source)))
      }
    }
  }

  return(all_windows)
}




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

    # First third windows
    for (i in 1:third) {
      start_col <- sample(0:(third - 1), 1)
      window_cols <- (start_col + 1):(start_col + window_size)

      if (max(window_cols) > rows) next
      window <- t(matrix[, , window_cols])

      window <- pad_window(window, target_rows)

      windows <- append(windows, list(list(window = window, source = source)))
    }

    # Second third windows
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

library(parallel)

process_sliding_windows_parallel <- function(matrices, sources, min_window_size, max_window_size, target_rows = 59) {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)

  clusterExport(cl, varlist = c("generate_all_windows", "pad_window"))
  clusterEvalQ(cl, library(matrixStats))  # if needed

  all_windows <- parLapply(cl, seq_along(matrices), function(i) {
    matrix <- matrices[[i]]
    source <- sources[i, ]
    generate_all_windows(matrix, source, min_window_size, max_window_size, target_rows)
  })

  stopCluster(cl)

  # Flatten the list of lists
  all_windows <- do.call(c, all_windows)

  return(all_windows)
}

# Use the parallel version
split_pad <- process_sliding_windows_parallel(
  matrices = matrices,
  sources = sources,
  min_window_size = 15,
  max_window_size = 59,
  target_rows = 59
)
saveRDS(split_pad,
        file = "RNN-MODELS/split_pad.rds",
        compress = TRUE
)

# 3. process_sliding_windows Function
process_sliding_windows <- function(matrices, sources, min_window_size, max_window_size, target_rows = 59) {
  all_windows <- list()

  for (matrix_idx in seq_along(matrices)) {
    matrix <- matrices[[matrix_idx]]
    source <- sources[matrix_idx, ]

    windows <- generate_all_windows(matrix, source, min_window_size, max_window_size, target_rows)

    all_windows <- append(all_windows, windows)
  }

  return(all_windows)
}
split_pad=process_sliding_windows(matrices=matrices,sources=sources,
                                  min_window_size=15,max_window_size = 59,target_rows = 59)
saveRDS(split_pad,
  file = "RNN-MODELS/split_pad.rds",
  compress = TRUE
)
