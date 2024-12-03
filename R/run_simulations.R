
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

run_simulations <- function(N, n, ndays, ncores, theta, seeds,path) {
  library(epiworldR)
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
    ans <- prepare_data(m)

    return(ans)
  }, mc.cores = ncores)

  return(matrices)
}



# run_simulations <- function(N, n, ndays, ncores, theta) {
#   matrices <- parallel::mclapply(1:N, FUN = function(i) {
#     fn <- sprintf("~/myfisrt.package/misc/simulated_data/sir-%06i.rds", i)
#
#     if (file.exists(fn))
#       return(readRDS(fn))
#     seeds <- sample.int(.Machine$integer.max, N, TRUE)
#     set.seed(seeds[i])
#
#     m <- ModelSIRCONN(
#       "mycon",
#       prevalence        = theta$preval[i],
#       contact_rate      = theta$crate[i],
#       transmission_rate = theta$ptran[i],
#       recovery_rate     = theta$prec[i],
#       n                 = n
#     )
#
#     verbose_off_and_run(m, ndays)
#     ans <- prepare_data(m)
#     saveRDS(ans, fn)
#
#     return(ans)
#   }, mc.cores = ncores)
#
#   return(matrices)
# }
# run_simulations(2e4,5000,50,20,theta)
#path="~/myfisrt.package/misc/simulated_data/sir-%06i.rds"
# source("~/myfisrt.package/R/dataprep.R")
# Input dataset as a vector


