#' Generate Theta Parameters
#'
#' Generates the theta parameters for the simulation pipeline.
#'
#' @param N Integer. Number of datasets to generate.
#' @param n Integer. Size of the population.
#' @return A `data.table` containing theta parameters.
#' @export
generate_theta <- function(N, n) {
  set.seed(1231)
  theta <- data.table(
    preval = sample((100:2000) / n, N, TRUE),
    crate  = rgamma(N, 5, 1),
    ptran  = rbeta(N, 3, 7),
    prec   = rbeta(N, 10, 10*2 - 10)
  )
  return(theta)
}
