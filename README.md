
## 📊 simulate_calibrate_sir() and 🔧 calibrate_sir()

### ✍️ Authors: George Vega Yon, Sima NJF

### 🚀 simulate_calibrate_sir()

This function simulates and calibrates an SIR (Susceptible-Infected-Recovered) model using TensorFlow and Keras in R. It generates simulated epidemic data, trains a Convolutional Neural Network (CNN) model, and evaluates the model's performance. Key steps include generating model parameters, running simulations, preparing data for training, splitting data, building, and evaluating the CNN model.

📝 **Function Arguments**:
- `N`: Number of simulation runs
- `n`: Population size
- `ndays`: Number of days to simulate
- `ncores`: Number of cores for parallel processing

```{r}
# 📌 Example usage of simulate_calibrate_sir function
library(epiworldR)
library(keras3)
library(tensorflow)
library(reticulate)
reticulate::import("numpy")
library(epiworldRcalibrate)
```
```{r}
N <- 2e4
n <- 5000
ndays <- 50
ncores <- 20
epochs <- 2
verbose <- 2
```
```{r}
simulate_calibrate_sir <- function(N, n, ndays, ncores, epochs, verbose) {
  # ⚙️ Generate Theta and Seeds
  theta <- generate_theta(N, n)
  seeds <- sample.int(.Machine$integer.max, N, TRUE)

  # 🧪 Run Simulations
  matrices <- run_simulations(N, n, ndays, ncores, theta, seeds)

  # 🔍 Filter Non-Null Data
  filtered_data <- filter_non_null(matrices, theta)
  matrices <- filtered_data$matrices
  theta <- filtered_data$theta
  N <- filtered_data$N

  # 📊 Prepare Data for TensorFlow
  arrays_1d <- prepare_data_for_tensorflow(matrices, N)
  theta2 <- as.data.table(copy(theta))
  theta2$crate <- plogis(theta2$crate / 10)

  # 🔀 Split Data into Training and Testing Sets
  data_split <- split_data(arrays_1d, theta2, N)
  train <- data_split$train
  test <- data_split$test

  # 🏗️ Build and Train the CNN Model
  model <- build_cnn_model(dim(arrays_1d)[-1], ncol(theta))
  train_model(model, train, epochs = epochs, verbose = verbose)

  # 📈 Evaluate the Model
  eval_results <- evaluate_model(model, test, theta)
  pred <- eval_results$pred
  MAEs <- eval_results$MAEs
  plot_results(pred, test, theta, MAEs, N, floor(N * 0.7))
  return(list(pred = pred, MAEs = MAEs))
}

# ▶️ Call the function
simulate_calibrate_sir(N, n, ndays, ncores, epochs, verbose)
```

### 🔧 calibrate_sir()

This function is used to predict SIR model parameters based on input data. It takes as input a numeric matrix or array containing counts of infected individuals over a period of 30 or 60 days and returns predicted values for parameters like prevalence, contact rate, transmission probability, and precision.
# 📌 Example usage of calibrate_sir function
```{r}
N <- 1
n <- 5000
set.seed(123)
theta <- generate_theta(N, n)
ncores <- 20
ndays <- 60
seeds <- 123

# 🧪 Run simulations
m <- epiworldR::ModelSIRCONN(
  "mycon",
  prevalence = theta$preval[1],
  contact_rate = theta$crate[1],
  transmission_rate = theta$ptran[1],
  recovery_rate = theta$prec[1],
  n = n
)

verbose_off(m)
run(m, ndays = ndays)
incidence <- epiworldR::plot_incidence(m, plot = FALSE)
data <- incidence$Infected

# 💾 Save theta and simulations data
theta2 <- as.data.table(copy(theta))
theta2$crate <- plogis(theta2$crate / 10)
```

# 🔧 Calibrate SIR model
```{r}
result <- calibrate_sir(data)
print(result)
```


