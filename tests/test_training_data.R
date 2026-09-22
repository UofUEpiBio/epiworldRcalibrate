source("R/01_simulation.R")
source("R/02_training.R")

parameters <- data.frame(
  n = c(1000, 1200, 1500),
  gamma = c(0.10, 0.12, 0.15),
  beta = c(0.20, 0.30, 0.40)
)

observations <- rbind(
  c(1, 2, 4, 6, 5),
  c(1, 3, 7, 9, 4),
  c(2, 5, 8, 7, 3)
)

x <- new_training_data(
  parameters = parameters,
  observations = observations,
  type = "CUSTOM",
  source = "unit test"
)

stopifnot(inherits(x, "training_data"))
stopifnot(nrow(x$theta) == 3)
stopifnot(ncol(x$observations) == 5)
stopifnot(identical(colnames(x$observations), paste0("time_", 1:5)))

cat("PASS: user-supplied training data interface\n")
