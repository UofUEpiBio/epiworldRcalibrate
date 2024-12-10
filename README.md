
## 📊 simulate_calibrate_sir() and 🔧 calibrate_sir()

### ✍️ Authors: George Vega Yon, Sima NJF
# 🌍 Introduction

Predicting the trajectory of infectious diseases lies at the heart of public health preparedness. With the epiworldRcalibrate package, you can effortlessly simulate and calibrate SIR (Susceptible-Infected-Recovered) epidemic models, blending the power of R, TensorFlow, and Keras. You can swiftly bridge the gap between theory and practice by generating realistic epidemic scenarios and training advanced Convolutional Neural Networks (CNNs) to identify key parameters—like prevalence, contact rate, transmission probability, and recovery precision.

The 🚀 simulate_calibrate_sir() function empowers you to produce synthetic incidence data and refine your model’s parameters through deep learning. Complementing this, 🔧 calibrate_sir() takes observed data, returning carefully tuned parameters ready for further analysis. Together, these tools streamline the modeling-to-inference pipeline, guiding you toward more informed, data-driven decisions in epidemiological research and public health policy.

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


