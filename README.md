# epiworldRcalibrate

A generic framework for calibrating epidemic and agent-based models.

## Core idea

Every calibration method is represented by the same simple S3 object:

``` r
calibrator_example <- list(
  pre_process = function(x, ...) { ... },
  run = function(x, ...) { ... },
  post_process = function(x, ...) { ... },
  type = "SIR"
) |>
  structure(class = "calibrator")
```

All methods are called through:

``` r
calibrate(daily_cases, calibrator, ...)
```

This keeps the **epidemic model** independent of the **calibration
method**.

## Three-step organization

``` text
R/01_simulation.R     data generation/import
R/02_training.R       training + calibrator definitions
R/03_test_compare.R   calibration + testing + benchmark comparisons
```

## Built-in training-data support

The package currently generates training banks for:

- SIR
- SEIR

For other models such as SIRH/SEIRD, users may provide their own
training data or plug in their own simulator.

## Calibration methods

The package currently demonstrates:

- BiLSTM
- ABC-MCMC
- ABC-SMC
- Nelder-Mead
- Differential Evolution

A user can add any new method by creating another four-field
`calibrator` object; `calibrate()` itself does not change.

See [USER_GUIDE.md](USER_GUIDE.md) for complete examples.

## Calibrating a window of daily infections (SEIR or SIR)

The package ships trained BiLSTM calibrators for windows of daily new
infections (15 to 365 days, from any part of an epidemic) of
`epiworldR::ModelSEIRCONN` and `ModelSIRCONN`:

``` r
library(epiworldRcalibrate)

fit  <- calibrate(cases, window_calibrator("SEIR"), n = 10000, recov = 1 / 7, incub = 5)
fit                                    # R0 (95% range), ptran, crate, starting state
band <- predict_cases(fit, ahead = 14) # or method = "renewal" (renewal equation)
score_band(band)                       # coverage and interval score

baselines <- window_baselines("SEIR")  # ABC-MCMC, ABC-SMC, DE, Nelder-Mead, set up the same way
fit_smc <- calibrate(cases, baselines$`ABC-SMC`, n = 10000, recov = 1 / 7, incub = 5)
window_baselines("SEIR", unknowns = "ptran_crate")   # search ptran and crate instead of R0

windows <- real_data_windows("New York Omicron 2021-22")   # real data shipped with the package
results <- compare_methods(windows, "SEIR", cores = 8)     # all methods on a list of windows
plot_comparison(results)               # coverage, interval score, errors and time by method
plot_bands(lapply(list(BiLSTM = fit, `ABC-SMC` = fit_smc), predict_cases, ahead = 14))
```

Data: `covid_us_states`, `covid_utah`, `admissions_utah`, `flu_baltimore_1918` and
`real_data_waves` (the waves of the report).

To make your own: `simulate_window_data()`, `tune_window_calibrator()`
and `train_window_calibrator()`. See `vignette("Window_calibration")`.

**Tutorial:** `TUTORIAL.Rmd` teaches the package step by step (installing, Python, calibrating,
predicting, scoring, calibrating with the standard methods, ptran and crate, comparing methods
with plots, the real data, training your own model, and rendering the documents); render it with `rmarkdown::render("TUTORIAL.Rmd")`.

## Step-by-step documentation

For the complete copy/paste workflow, see
[STEP_BY_STEP_GUIDE.md](STEP_BY_STEP_GUIDE.md).

## Run SIR and SEIR end-to-end

To actually generate SIR/SEIR training banks, train both BiLSTM models,
and produce validation/test results, open:

``` text
examples/RUN_SIR_SEIR_BILSTM.Rmd
```

and Knit it.

At the top of the file:

``` r
FULL_RUN <- FALSE
```

runs a small pipeline test. Change to:

``` r
FULL_RUN <- TRUE
```

for the current full settings (5,000 SIR simulations, 20,000 SEIR
simulations, 100 epochs for each model).

The official Getting Started vignette remains lightweight documentation
and does not run the expensive full training workflow during package
checks.
