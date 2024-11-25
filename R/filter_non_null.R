#' Filter Out Null or NA Matrices
#'
#' This function filters out matrices that are either errors or contain any \code{NA} values from a list of matrices.
#' It also filters the corresponding \code{theta} parameters to match the filtered matrices.
#'
#' @param matrices A list of matrices to be filtered.
#' @param theta A data frame or matrix containing parameters corresponding to each matrix in \code{matrices}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{matrices}}{A filtered list of matrices without errors or \code{NA} values.}
#'   \item{\code{theta}}{The corresponding \code{theta} parameters for the filtered matrices.}
#'   \item{\code{N}}{An integer indicating the number of matrices after filtering.}
#' }
#'
#' @examples
#' # Create a list of matrices, some with errors and NAs
#' matrices <- list(
#'   matrix(1:4, nrow = 2),
#'   simpleError("Error in simulation"),
#'   matrix(c(1, NA, 3, 4), nrow = 2)
#' )
#' theta <- data.frame(param1 = c(0.1, 0.2, 0.3))
#'
#' # Filter out invalid matrices
#' result <- filter_non_null(matrices, theta)
#' print(result$matrices)
#' print(result$theta)
#' print(result$N)
#'
#' @export


filter_non_null <- function(matrices, theta) {
  is_not_null <- intersect(
    which(!sapply(matrices, inherits, what = "error")),
    which(!sapply(matrices, \(x) any(is.na(x))))
  )

  matrices <- matrices[is_not_null]
  theta    <- theta[is_not_null, ]

  return(list(matrices = matrices, theta = theta, N = length(is_not_null)))
}
