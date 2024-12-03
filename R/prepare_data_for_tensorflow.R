#' Prepare Data for TensorFlow
#'
#' @description
#' Converts a list of matrices into a 3D array suitable for TensorFlow input.
#'
#' @param matrices List. The list of simulation result matrices.
#' @param N Integer. The number of simulations.
#' @return A 3D array suitable for input into TensorFlow models.
#' @export
prepare_data_for_tensorflow <- function(matrices, N) {
  arrays_1d <- array(dim = c(N, dim(matrices[[1]][1,,])))

  for (i in seq_along(matrices)) {
    arrays_1d[i,,] <- matrices[[i]][1,,]
  }

  return(arrays_1d)
}
