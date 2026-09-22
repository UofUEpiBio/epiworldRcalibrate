# R code: three-step workflow

The package keeps the user-facing R code in three main files.

| File | Step | What is here |
|---|---|---|
| `01_simulation.R` | 1. Data | Built-in SIR/SEIR simulation, custom simulators, user-supplied training data |
| `02_training.R` | 2. Method | Generic BiLSTM training/loading and the `calibrator` object pattern |
| `03_test_compare.R` | 3. Test | `calibrate()`, `make_calibrator()` (ABC-MCMC, ABC-SMC, Nelder-Mead, DE), comparison utilities |

See the top-level `README.md` for the calibrator object design and `STEP_BY_STEP_GUIDE.md` for
a full walkthrough.
