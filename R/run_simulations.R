
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
#' @return A list containing the simulation results as matrices.
#' @export

run_simulations <- function(N, n, ndays, ncores, theta, seeds) {
  library(epiworldR)
  library(parallel)

  # Detect the operating system
  os_type <- .Platform$OS.type

  if (os_type == "windows") {
    # Use parLapply for Windows
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl)) # Ensure the cluster is stopped after use

    clusterExport(cl, varlist = c("theta", "n", "ndays", "seeds", "prepare_data"), envir = environment())
    clusterEvalQ(cl, library(epiworldR)) # Load necessary libraries on workers

    matrices <- parLapply(cl, 1:N, function(i) {
      set.seed(seeds[i])
      m <- epiworldR::ModelSIRCONN(
        "mycon",
        prevalence        = theta$preval[i],
        contact_rate      = theta$crate[i],
        transmission_rate = theta$ptran[i],
        recovery_rate     = theta$prec[i],
        n                 = n
      )

      verbose_off(m)
      run(m, ndays = ndays)
      ans <- prepare_data(m, max_days = ndays)

      return(ans)
    })

  } else {
    # Use mclapply for macOS/Linux
    matrices <- mclapply(1:N, function(i) {
      set.seed(seeds[i])
      m <- epiworldR::ModelSIRCONN(
        "mycon",
        prevalence        = theta$preval[i],
        contact_rate      = theta$crate[i],
        transmission_rate = theta$ptran[i],
        recovery_rate     = theta$prec[i],
        n                 = n
      )

      verbose_off(m)
      run(m, ndays = ndays)
      ans <- prepare_data(m, max_days = ndays)

      return(ans)
    }, mc.cores = ncores)
  }

  return(matrices)
}

