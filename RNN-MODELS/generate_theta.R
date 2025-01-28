#' Generate Theta Parameters for SIR Model Simulation
#'
#' @description
#' Generates a data table of parameters needed to simulate SIR models.
#'
#' @param N Integer. The number of parameter sets to generate.
#' @param n Integer. The population size for each simulation.
#' @importFrom stats plogis qlogis rbeta rgamma
#' @return A data.table containing the generated parameters.
#' @export
generate_theta <- function(N, n) {
  set.seed(1231)
  theta <- data.table::data.table(
    preval = sample((100:2000) / n, N, TRUE),
    crate  = stats::rgamma(N, 5, 1),
    ptran  = stats::rbeta(N, 3, 7),
    prec   = stats::rbeta(N, 10, 10)
  )
  return(theta)
}
