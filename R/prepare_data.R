#' Prepare Data for TensorFlow
#'
#' Converts simulation results into TensorFlow-compatible arrays.
#'
#' @param matrices List of simulation results.
#' @param N Integer. Number of datasets.
#' @return A 3D array for TensorFlow input.
#' @export
prepare_data_for_tensorflow <- function(matrices, N) {
  arrays_1d <- array(dim = c(N, dim(matrices[[1]][1,,])))

  for (i in seq_along(matrices)) {
    arrays_1d[i,,] <- matrices[[i]][1,,]
  }

  return(arrays_1d)
}
