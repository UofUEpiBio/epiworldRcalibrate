#' @name run_simulations
#' @title Run Simulations
#' @description Runs the SIR-CONN simulations based on theta parameters.
#'
#' @param N Integer. Number of simulations to run.
#' @param n Integer. Size of the population.
#' @param ndays Integer. Number of days to simulate.
#' @param ncores Integer. Number of cores for parallel execution.
#' @param theta A `data.table` of theta parameters.
#' @param seeds Integer vector of seeds for reproducibility.
#' @return A list of simulation results.
#' @export
run_simulations <- function(N, n, ndays, ncores, theta, seeds) {
  matrices <- parallel::mclapply(1:N, FUN = function(i) {
    fn <- sprintf("R/simulated_data/sir-%06i.rds", i)

    if (file.exists(fn)) {
      return(readRDS(fn))
    }

    set.seed(seeds[i])
    m <- theta[i, ModelSIRCONN(
      "mycon",
      prevalence        = preval,
      contact_rate      = crate,
      transmission_rate = ptran,
      recovery_rate     = prec,
      n                 = n
    )]

    verbose_off(m)
    run(m, ndays = ndays)
    ans <- myfirst.package::prepare_data(m)  # Explicit namespace
    saveRDS(ans, fn)

    return(ans)
  }, mc.cores = ncores)

  return(matrices)
}
