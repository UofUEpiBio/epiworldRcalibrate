
#' simulate calibrate sir Function
#'
#' @description
#' Orchestrates the entire process: data generation, simulation, data preparation, model training, evaluation, and plotting.
#'
#' @param N Integer. The number of simulations to run.
#' @param n Integer. The population size for each simulation.
#' @param ndays Integer. The number of days to simulate.
#' @param ncores Integer. The number of cores to use for parallel processing.
#' @return Executes the pipeline and generates plots.
#' @export
# N=2e4
# n=5000
# ncores=20
# ndays=50
simulate_calibrate_sir<- function(N,n,ndays,ncores) {
  library(keras3)
  library(data.table)
  library(tensorflow)

# Generate theta and seeds
  theta <- generate_theta(N, n)
  seeds <- sample.int(.Machine$integer.max, N, TRUE)

  # Creating a temporary path to store the data
  # path_dir <- tempfile("simulated_data")
  # if (dir.exists(path_dir)) {
  #   file.remove(list.files(path_dir))
  #   file.remove(path_dir)
  # }
  # dir.create(path_dir, recursive = TRUE)
  #
  # path <- file.path(path_dir, "sir-%06i.rds")

  path="~/epiworldRcalibrate/misc/simulated_data/sir-%06i.rds"
  # Run simulations
  matrices <- run_simulations(N, n, ndays, ncores, theta,seeds,path)

  # Filter non-null elements
  filtered_data <- filter_non_null(matrices, theta)
  matrices <- filtered_data$matrices
  theta <- filtered_data$theta
  N <- filtered_data$N

  # Prepare data for TensorFlow
  arrays_1d <- prepare_data_for_tensorflow(matrices, N)

  # Save theta and simulations data
  theta2 <-as.data.table(copy(theta))
  theta2$crate <- plogis(theta2$crate / 10)

  # theta2[, crate := plogis(crate / 10)]

  # saveRDS(list(theta = theta2, simulations = arrays_1d), file = "R/sir.rds", compress = TRUE)

  # Split data into training and testing sets
  data_split <- split_data(arrays_1d, theta2, N)
  train <- data_split$train
  test <- data_split$test

  # Build and train the CNN model
  model <- build_cnn_model(dim(arrays_1d)[-1], ncol(theta2))
  train_model(model, train)

  # Evaluate the model
  eval_results <- evaluate_model(model, test, theta)
  pred <- eval_results$pred
  MAEs <- eval_results$MAEs

  # Plot the results
  plot_results(pred, test, theta, MAEs, N, floor(N * 0.7))
  return(list(pred=pred,MAEs=MAEs))
}

