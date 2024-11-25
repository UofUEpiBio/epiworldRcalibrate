library(data.table)
#' Main Pipeline
#'
#' Executes the full pipeline: simulation, data preparation, and modeling.
#'
#' @param N Integer. Number of datasets.
#' @param n Integer. Size of the population.
#' @param ndays Integer. Number of simulation days.
#' @param ncores Integer. Number of cores for parallelization.
#' @export

main_pipeline <- function(N, n, ndays, ncores) {
  theta <- generate_theta(N, n)
  seeds <- sample.int(.Machine$integer.max, N, TRUE)

  matrices <- run_simulations(N, n, ndays, ncores, theta, seeds)

  filtered_data <- filter_non_null(matrices, theta)
  matrices <- filtered_data$matrices
  theta <- filtered_data$theta
  N <- filtered_data$N

  arrays_1d <- prepare_data_for_tensorflow(matrices, N)

  theta2 <- copy(theta)
  theta2[, crate := plogis(crate / 10)]
  saveRDS(list(theta = theta2, simulations = arrays_1d), file = "R/sir.rds", compress = TRUE)

  data_split <- split_data(arrays_1d, theta2, N)
  train <- data_split$train
  test <- data_split$test

  model <- build_cnn_model(dim(arrays_1d)[-1], ncol(theta2))
  train_model(model, train)

  eval_results <- evaluate_model(model, test, theta)
  pred <- eval_results$pred
  MAEs <- eval_results$MAEs

  plot_results(pred, test, theta, MAEs, N, floor(N * 0.7))
}
