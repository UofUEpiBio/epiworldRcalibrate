#' Filter Non-Null Simulation Results
#'
#' @description
#' Filters out simulations that resulted in errors or contain `NA` values.
#'
#' @param matrices List. The list of simulation result matrices.
#' @param theta `data.table`. The parameters corresponding to the simulations.
#' @return A list containing:
#' \describe{
#'   \item{matrices}{The filtered list of matrices.}
#'   \item{theta}{The filtered parameters.}
#'   \item{N}{The updated number of simulations.}
#' }
#' @export
filter_non_null <- function(matrices, theta) {
  is_not_null <- intersect(
    which(!sapply(matrices, inherits, what = "error")),
    which(!sapply(matrices, function(x) any(is.na(x))))
  )

  matrices <- matrices[is_not_null]
  theta    <- theta[is_not_null, ]

  return(list(matrices = matrices, theta = theta, N = length(is_not_null)))
}
