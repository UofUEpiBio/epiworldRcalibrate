# Examples: the SEIR and SIR BiLSTM report

**Read this:** `results/report/Report_SEIR_SIR.html` (everything in one report).

How it is made (run from this folder; each step saves its results, the report only reads them):

| Step | File | Results |
|---|---|---|
| 1. Train the noise-free BiLSTMs (SEIR and SIR; any window length) | `Clean_any_length.Rmd` | `results/{seir,sir}_clean_big/` (also `_clean`, `_clean_tuned`) |
| 2. BiLSTM vs ABC-MCMC, ABC-SMC, DE, Nelder-Mead on simulated windows | `Compare_methods_simulated.Rmd` | `results/simulated_baselines/` |
| 3. The same on real data (10 US states, Utah, Baltimore) | `Real_data.Rmd` | `results/real_data/` |
| 4. Earlier real-data run (current vs big BiLSTM table) | `Clean_real_data.Rmd` (+ `simple_de.R`) | `results/clean_real_data(_big)/` |
| 5. The report | `Report_SEIR_SIR.Rmd` | `results/report/` |

Shared code of the comparison methods and scores: `methods_common.R`. Real data: `data/`.
Each results folder has a `logs/` folder with the SLURM scripts used on CHPC.
`Compare_methods_demo.Rmd`, `RUN_SIR_SEIR_BILSTM.Rmd` and `seir_reference_comparison/` are the
package's original examples. Older experiments are in `archive/`.
