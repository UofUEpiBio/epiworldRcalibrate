
#' Preprocess Data for Machine Learning Models
#'
#' This function preprocesses input data for use in machine learning models, focusing on filtering,
#' reshaping, and preparing it for TensorFlow or similar frameworks.
#'
#' @param data A numeric vector or data frame containing time-series data to be processed.
#'
#' @return A matrix where the data is reshaped into a single column, suitable for machine learning models.
#'
#' @details
#' The function performs the following steps:
#' 1. Prepares the input data using `prepare_data_infected()`.
#' 2. Filters the data using `filter_non_null_infected()` to exclude missing or invalid values.
#' 3. Extracts and reshapes the data into a one-column matrix format.
#'
#' @export
preprocessing_data <- function(data) {
  ans <- list(prepare_data_infected(data))

  if (is.null(ans[[1]]) || inherits(ans[[1]], "error")) {
    stop("Error in prepare_data_infected: The data could not be prepared.")
  }

  filtered_data <- filter_non_null_infected(ans)

  if (is.null(filtered_data$matrices) || length(filtered_data$matrices) == 0) {
    stop("Error in filter_non_null_infected: No valid matrices found.")
  }

  matr <- unlist(filtered_data$matrices)

  if (is.null(matr) || length(matr) == 0) {
    stop("Error: Unlisted matrices are NULL or empty.")
  }

  data2 <- as.vector(matr)
  matrix_a <- as.matrix(data2, ncol = 1)

  return(matrix_a)
}
