
#| label: installing packages
library(epiworldR)
library(data.table)
library(tensorflow)

library(parallel)
library(keras3)
library(dplyr)
library(ggplot2)

N=2e4
n=5000

source("R/dataprep.R")


generate_theta <- function(N, n) {
  set.seed(1231)
  theta <- data.table(
    preval = sample((100:2000) / n, N, TRUE),
    crate  = rgamma(N, 5, 1),    # Mean 10
    ptran  = rbeta(N, 3, 7),     # Mean 3/(3 + 7) = 0.3
    prec   = rbeta(N, 10, 10*2 - 10) # Mean 10 / (10 * 2 - 10) = 0.5
  )
  return(theta)
}

# Load the required library
library(reticulate)

# Generate the theta data


run_simulations <- function(N, n, ndays, ncores, theta, seeds) {
  matrices <- parallel::mclapply(1:N, FUN = function(i) {
    fn <- sprintf("R/simulated_data/sir-%06i.rds", i)

    if (file.exists(fn))
      return(readRDS(fn))

    set.seed(seeds[i])
    m <- theta[i, ModelSIRCONN(
      "mycon",
      prevalence        = preval,
      contact_rate      = crate,
      transmission_rate = ptran,
      recovery_rate     = prec,
      n                 = n
    )]

    verbose_off(m)
    run(m, ndays = ndays)
    ans <- prepare_data(m)
    saveRDS(ans, fn)

    return(ans)
  }, mc.cores = ncores)

  return(matrices)
}

filter_non_null <- function(matrices, theta) {
  is_not_null <- intersect(
    which(!sapply(matrices, inherits, what = "error")),
    which(!sapply(matrices, \(x) any(is.na(x))))
  )

  matrices <- matrices[is_not_null]
  theta    <- theta[is_not_null, ]

  return(list(matrices = matrices, theta = theta, N = length(is_not_null)))
}


prepare_data_for_tensorflow <- function(matrices, N) {
  arrays_1d <- array(dim = c(N, dim(matrices[[1]][1,,])))

  for (i in seq_along(matrices)) {
    arrays_1d[i,,] <- matrices[[i]][1,,]
  }

  return(arrays_1d)
}
theta=generate_theta(2e4,5000)
matrices=run_simulations(2e4,5000,50,20,theta,123)
arrays_1d=prepare_data_for_tensorflow(matrices,2e4)
dim(matrices[[1]])































# Load required libraries
library(data.table)
library(parallel)
library(reticulate)  # Python interface



split_data <- function(arrays_1d, theta2, N) {
  N_train <- floor(N * 0.7)
  id_train <- 1:N_train
  id_test <- (N_train + 1):N

  train <- list(
    x = array_reshape(arrays_1d[id_train,,], dim = c(N_train, dim(arrays_1d)[-1])),
    y = array_reshape(as.matrix(theta2)[id_train,], dim = c(N_train, ncol(theta2)))
  )

  test <- list(
    x = array_reshape(arrays_1d[id_test,,], dim = c(N - N_train, dim(arrays_1d)[-1])),
    y = array_reshape(as.matrix(theta2)[id_test,], dim = c(N - N_train, ncol(theta2)))
  )

  return(list(train = train, test = test))
}

# Generate theta and run simulations
theta <- generate_theta(2e4, 5000)
matrices <- run_simulations(2e4, 5000, 50, 20, theta, 123)
arrays_1d <- prepare_data_for_tensorflow(matrices, 2e4)

# Assuming theta2 is the same as theta for demonstration purposes
theta2 <- theta

# Split the data into train and test sets
split <- split_data(arrays_1d, theta2, 2e4)
train <- split$train
test <- split$test

# Save the train and test data in a single .pkl file
pickle <- import("pickle")

# Create a list to store train and test data
data_to_save <- list(
  train = train,
  test = test
)

# Save the data to a .pkl file
py_run_string("f = open('train_test_data.pkl', 'wb')")
pickle$dump(data_to_save, py$f)
py_run_string("f.close()")

cat("Data saved to 'train_test_data.pkl'")

# Load reticulate library
library(reticulate)

# Your function to generate theta
generate_theta <- function(N, n) {
  set.seed(1231)
  theta <- data.table(
    preval = sample((100:2000) / n, N, TRUE),
    crate  = rgamma(N, 5, 1),    # Mean 10
    ptran  = rbeta(N, 3, 7),     # Mean 3/(3 + 7) = 0.3
    prec   = rbeta(N, 10, 10*2 - 10) # Mean 10 / (10 * 2 - 10) = 0.5
  )
  return(theta)
}

# Generate the theta data
theta <- generate_theta(N, n)

# Convert the theta object to a standard data.frame
theta_df <- as.data.frame(theta)

# Import pickle module
pickle <- import("pickle")

# Use py_eval to open the file in write-binary mode and pass it to R as py$f
py$f <- py_eval("open('calibration/theta_data.pkl', 'wb')")

# Use pickle to dump the data from R (converted to Python object) into the file
pickle$dump(r_to_py(theta_df), py$f)

# Close the file using py_eval
py_eval("f.close()")

file.exists("calibration/theta_data.pkl")

