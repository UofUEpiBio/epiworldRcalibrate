#' simulate calibrate sir Function
#'
#' @description
#' Orchestrates the entire process: data generation, simulation, data preparation, model training, evaluation, and plotting.
#'
#' @param N Integer. The number of simulations to run.
#' @param n Integer. The population size for each simulation.
#' @param ndays Integer. The number of days to simulate.
#' @param ncores Integer. The number of cores to use for parallel processing.
#' @param epochs Integer. The number of training epochs.
#' @param verbose Integer. Verbosity mode for training. 0 shows training output, 2 doesn't.
#' @importFrom data.table copy
#' @return Executes the pipeline and generates plots.
#' @export
simulate_calibrate_sir <- function(N, n, ndays, ncores, epochs, verbose) {
  # Generate theta and seeds
  theta <- generate_theta(N, n)
  seeds <- sample.int(.Machine$integer.max, N, TRUE)
  path = "~/epiworldRcalibrate/misc/simulated_data/sir-%06i.rds"

  # Run simulations
  matrices <- run_simulations(N, n, ndays, ncores, theta, seeds)

  # Filter non-null elements
  filtered_data <- filter_non_null(matrices, theta)
  matrices <- filtered_data$matrices
  theta <- filtered_data$theta
  N <- filtered_data$N

  # Prepare data for TensorFlow
  arrays_1d <- prepare_data_for_tensorflow(matrices, N)

  # Save theta and simulations data
  theta2 <- data.table::as.data.table(data.table::copy(theta))
  theta2$crate <- stats::plogis(theta2$crate / 10)

  # Split data into training and testing sets
  data_split <- split_data(arrays_1d, theta2, N)
  train <- data_split$train
  test <- data_split$test

  # Build and train the CNN model
  model <- build_cnn_model(dim(arrays_1d)[-1], ncol(theta2))
  train_model(model, train, epochs = epochs, verbose = verbose)

  # Evaluate the model
  eval_results <- evaluate_model(model, test, theta)
  pred <- eval_results$pred
  MAEs <- eval_results$MAEs

  # Plot the results
  plot_results(pred, test, theta, MAEs, N, floor(N * 0.7))
  return(list(pred = pred, MAEs = MAEs))
}
