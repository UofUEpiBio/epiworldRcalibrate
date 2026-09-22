# Small test of the generic simulator interface. No epiworldR model is needed.

source(file.path("R", "01_simulation.R"))

sampler <- function(n_sims) {
  data.frame(
    level = seq_len(n_sims),
    slope = rep(2, n_sims)
  )
}

runner <- function(parameters, seed) {
  # A deterministic toy curve is enough to test the interface.
  out <- parameters$level + parameters$slope * 0:4
  names(out) <- paste0("time_", 0:4)
  out
}

toy <- new_simulator(
  sample_parameters = sampler,
  run = runner,
  type = "toy",
  name = "toy simulator"
)

bank <- simulate_training_data(toy, n_sims = 3, seed = 122)

stopifnot(nrow(bank$theta) == 3)
stopifnot(nrow(bank$observations) == 3)
stopifnot(ncol(bank$observations) == 5)
stopifnot(identical(colnames(bank$observations), paste0("time_", 0:4)))

one <- simulate_from_parameters(
  toy,
  parameters = list(level = 10, slope = 2),
  seed = 1
)
stopifnot(all(one == c(10, 12, 14, 16, 18)))

cat("PASS: generic simulator interface\n")
