# Chapter 16: Run All Simulations
# ============================================================
#
# This runner regenerates every dataset, table, and figure used in Chapter 16.
# Each method also has its own standalone script, so readers can either run one
# section at a time or reproduce the entire chapter with this file.
#
# Tip for RStudio:
#   Source this file if you want all outputs. Open one of the section scripts
#   if you want to study a single method more slowly.

master_seed <- 123L

source("code/16-01-randomized-controlled-trial.R")
rct_results <- run_rct_simulation(seed = master_seed, show_plot = FALSE)

source("code/16-02-selection-on-observables.R")
selection_results <- run_selection_on_observables_simulation(
  seed = master_seed,
  show_plot = FALSE
)

source("code/16-03-difference-in-differences.R")
did_results <- run_did_simulation(seed = master_seed, show_plot = FALSE)

source("code/16-04-event-studies.R")
event_study_results <- run_event_study_simulation(
  seed = master_seed,
  show_plot = FALSE
)

source("code/16-05-instrumental-variables.R")
iv_results <- run_iv_simulation(seed = master_seed, show_plot = FALSE)

source("code/16-06-regression-discontinuity.R")
rdd_results <- run_rdd_simulation(seed = master_seed, show_plot = FALSE)

source("code/16-07-synthetic-control.R")
scm_results <- run_scm_simulation(seed = master_seed, show_plot = FALSE)

source("code/16-08-causal-arima.R")
causal_arima_results <- run_causal_arima_simulation(
  seed = master_seed,
  show_plot = FALSE
)

source("code/16-09-mlcm.R")
mlcm_results <- run_mlcm_simulation(seed = master_seed, show_plot = FALSE)

cat("\nChapter 16 simulations completed with master seed", master_seed, "\n")
