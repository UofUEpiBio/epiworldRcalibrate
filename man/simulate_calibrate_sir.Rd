% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_calibrate_sir.R
\name{simulate_calibrate_sir}
\alias{simulate_calibrate_sir}
\title{simulate calibrate sir Function}
\usage{
simulate_calibrate_sir(N, n, ndays, ncores, epochs, verbose)
}
\arguments{
\item{N}{Integer. The number of simulations to run.}

\item{n}{Integer. The population size for each simulation.}

\item{ndays}{Integer. The number of days to simulate.}

\item{ncores}{Integer. The number of cores to use for parallel processing.}

\item{epochs}{Integer. The number of training epochs.}

\item{verbose}{Integer. Verbosity mode for training. 0 shows training output, 2 doesn't.}
}
\value{
Executes the pipeline and generates plots.
}
\description{
Orchestrates the entire process: data generation, simulation, data preparation, model training, evaluation, and plotting.
}
