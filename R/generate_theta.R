#' Generate Theta Parameters for SIR Model Simulation
#'
#' @description
#' Generates a data table of parameters needed to simulate SIR models.
#'
#' @param N Integer. The number of parameter sets to generate.
#' @param n Integer. The population size for each simulation.
#' @importFrom stats plogis predict qlogis rbeta rgamma
#' @return A data.table containing the generated parameters.
#' @export
generate_theta <- function(N, n) {
  library(data.table)
  set.seed(1231)
  theta <- data.table::data.table(
    preval = sample((100:2000) / n, N, TRUE),
    crate  = rgamma(N, 5, 1),    # Mean 5
    ptran  = rbeta(N, 3, 7),     # Mean 0.3
    prec   = rbeta(N, 10, 10)    # Mean 0.5
  )
  return(theta)
}
