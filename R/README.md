# R code: three-step workflow

The package keeps the user-facing R code in three main files, plus three for windows of daily infections.

| File | Step | What is here |
|---|---|---|
| `01_simulation.R` | 1. Data | Built-in SIR/SEIR simulation, custom simulators, user-supplied training data |
| `02_training.R` | 2. Method | Generic BiLSTM training/loading and the `calibrator` object pattern |
| `03_test_compare.R` | 3. Test | `calibrate()`, `make_calibrator()` (ABC-MCMC, ABC-SMC, Nelder-Mead, DE), comparison utilities |
| `04_windows.R` | All steps for SEIR/SIR windows | `window_calibrator()` (shipped any-length BiLSTM), `window_baselines()` (ABC-MCMC, ABC-SMC, DE, Nelder-Mead set up the same way), `predict_cases()` (model runs or renewal equation), `coverage()`, `interval_score()`, `score_band()`, `compare_methods()`, and `simulate_window_data()`, `tune_window_calibrator()`, `train_window_calibrator()` to make your own |
| `05_real_data.R` | Real data | Documentation of the shipped datasets (`covid_us_states`, `covid_utah`, `admissions_utah`, `flu_baltimore_1918`, `real_data_waves`), `wave_data()`, `real_data_windows()` |
| `06_plots.R` | Plots | `plot_bands()`, `plot_comparison()`, `plot_estimates()`, `plot_ptran_crate()` |

See the top-level `README.md` for the calibrator object design and `STEP_BY_STEP_GUIDE.md` for
a full walkthrough.
