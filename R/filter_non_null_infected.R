#' Filter Non-Null and Valid Matrices
#'
#' This function filters a list of matrices, excluding those that contain errors or missing values.
#'
#' @param matrices A list of matrices to be filtered.
#'
#' @return A list with two elements:
#' \item{matrices}{The filtered list of valid matrices.}
#' \item{N}{The number of valid matrices remaining after filtering.}
#'
#' @details
#' The function performs the following checks on each matrix in the input list:
#' 1. Excludes matrices that inherit an "error" class.
#' 2. Excludes matrices that contain any `NA` values.
#'
#' The function then returns the filtered matrices and their count.
#'
#' @export
filter_non_null_infected <- function(matrices) {
  is_not_null <- intersect(
    which(!sapply(matrices, inherits, what = "error")),
    which(!sapply(matrices, function(x) any(is.na(x))))
  )

  matrices <- matrices[is_not_null]

  return(list(matrices = matrices, N = length(is_not_null)))
}
