#' Run SIR Model Simulations
#'
#' @description
#' Runs SIR model simulations using the `epiworldR` package in parallel.
#'
#' @param N Integer. The number of simulations to run.
#' @param n Integer. The population size for each simulation.
#' @param ndays Integer. The number of days to simulate.
#' @param ncores Integer. The number of cores to use for parallel processing.
#' @param theta `data.table`. The parameters for the simulations.
#' @param seeds Integer vector. Random seeds for each simulation.
#' @importFrom parallel makeCluster stopCluster clusterExport clusterEvalQ parLapply mclapply
#' @importFrom epiworldR run verbose_off ModelSIRCONN
#' @importFrom data.table as.data.table dcast melt
#' @return A list containing the simulation results as matrices.
#' @export
run_simulations <- function(N, n, ndays, ncores, theta, seeds) {
  matrices <- parallel::mclapply(1:N, FUN = function(i) {
    set.seed(seeds[i])
    m <- epiworldR:: ModelSIRCONN(
      "mycon",
      prevalence        = theta$preval[i],
      contact_rate      = theta$crate[i],
      transmission_rate = theta$ptran[i],
      recovery_rate     = theta$prec[i],
      n                 = n
    )

    verbose_off(m)
    run(m, ndays = ndays)
    ans <- prepare_data(m,max_days=ndays)

    return(ans)
  }, mc.cores = ncores)

  return(matrices)
}

