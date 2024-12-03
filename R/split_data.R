#' Split Data into Training and Test Sets
#'
#' @description
#' Splits the data into training and test sets for model training and evaluation.
#'
#' @param arrays Array. The 3D array prepared for TensorFlow.
#' @param theta `data.table`. The parameters corresponding to the simulations.
#' @param N Integer. The number of simulations.
#' @return A list containing:
#' \describe{
#'   \item{train}{A list with training data (`x` and `y`).}
#'   \item{test}{A list with test data (`x` and `y`).}
#' }
#' @export
split_data <- function(arrays, theta, N) {
  N_train <- floor(N * 0.7)
  id_train <- 1:N_train
  id_test <- (N_train + 1):N

  train <- list(
    x = tensorflow::array_reshape(arrays[id_train,,], dim = c(N_train, dim(arrays)[-1])),
    y = tensorflow::array_reshape(as.matrix(theta)[id_train,], dim = c(N_train, ncol(theta)))
  )

  test <- list(
    x = tensorflow::array_reshape(arrays[id_test,,], dim = c(N - N_train, dim(arrays)[-1])),
    y = tensorflow::array_reshape(as.matrix(theta)[id_test,], dim = c(N - N_train, ncol(theta)))
  )

  return(list(train = train, test = test, N_train = N_train))
}
