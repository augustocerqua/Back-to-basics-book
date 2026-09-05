# Section 16.6: Regression Discontinuity Design
# ============================================================
#
# This script simulates a stylized version of the Thistlethwaite and Campbell
# merit-scholarship design.
#
# Example:
#   Candidates receive a scholarship if their recorded test score is above a
#   cutoff. The outcome is a later academic score.
#
# Goal:
#   Show why a sharp RDD can identify a local causal effect at the cutoff when
#   units cannot precisely manipulate the running variable, and how the design
#   becomes less credible when some units sort around the threshold.
#
# The script creates:
#   1. an ideal RDD setting with no manipulation;
#   2. a failure setting with sorting just around the cutoff;
#   3. RD estimates and manipulation diagnostics saved as CSV files;
#   4. a high-resolution PNG figure.
#
# Reproducibility:
#   All random draws use deterministic streams based on master seed 123.


# 0. Packages ---------------------------------------------------------------

required_packages <- c(
  "dplyr", "ggplot2", "patchwork", "rdrobust", "rddensity"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(ggplot2)
library(patchwork)


# 1. Simulate potential outcomes and the running variable ------------------

# The true test score is the running variable. In the ideal case, this is also
# the recorded score used to assign scholarships.
rdd_simulate_population <- function(seed = 123L, sample_size = 10000L) {
  stopifnot(sample_size >= 500L)

  set.seed(seed + 810L)
  true_test_score <- runif(sample_size, min = -2.5, max = 2.5)
  latent_motivation <- rnorm(sample_size)
  outcome_noise <- rnorm(sample_size, sd = 3.0)

  untreated_outcome <-
    75 +
    5.5 * true_test_score +
    0.8 * true_test_score^2 +
    2.5 * latent_motivation +
    outcome_noise

  scholarship_effect <- 4.0

  data.frame(
    candidate_id = seq_len(sample_size),
    simulated_true_test_score = true_test_score,
    simulated_latent_motivation = latent_motivation,
    simulated_outcome_without_scholarship = untreated_outcome,
    simulated_outcome_with_scholarship =
      untreated_outcome + scholarship_effect,
    simulated_scholarship_effect = scholarship_effect
  )
}


# 2. Create ideal and manipulated assignment scenarios ---------------------

rdd_create_ideal_data <- function(population) {
  population %>%
    mutate(
      scenario = "Ideal: no manipulation",
      recorded_test_score = simulated_true_test_score,
      manipulated_score = 0L,
      received_scholarship = as.integer(recorded_test_score >= 0),
      later_academic_score = if_else(
        received_scholarship == 1L,
        simulated_outcome_with_scholarship,
        simulated_outcome_without_scholarship
      )
    )
}

rdd_create_failure_data <- function(population, seed = 123L) {
  # Hypothetical manipulation: highly motivated candidates with a true score
  # slightly below the threshold can obtain a recorded score just above it.
  can_manipulate <-
    population$simulated_true_test_score >= -0.30 &
    population$simulated_true_test_score < 0 &
    population$simulated_latent_motivation > 0.50

  recorded_test_score <- population$simulated_true_test_score
  set.seed(seed + 820L)
  recorded_test_score[can_manipulate] <- runif(
    sum(can_manipulate),
    min = 0.005,
    max = 0.080
  )

  population %>%
    mutate(
      scenario = "Failure: manipulation near cutoff",
      recorded_test_score = recorded_test_score,
      manipulated_score = as.integer(can_manipulate),
      received_scholarship = as.integer(recorded_test_score >= 0),
      later_academic_score = if_else(
        received_scholarship == 1L,
        simulated_outcome_with_scholarship,
        simulated_outcome_without_scholarship
      )
    )
}


# 3. Estimate RD effects and diagnostics -----------------------------------

rdd_extract_estimate <- function(model, scenario, true_effect = 4.0) {
  data.frame(
    scenario = scenario,
    estimate = unname(model$coef["Robust", "Coeff"]),
    standard_error = unname(model$se["Robust", "Std. Err."]),
    conf_low = unname(model$ci["Robust", "CI Lower"]),
    conf_high = unname(model$ci["Robust", "CI Upper"]),
    bandwidth_left = unname(model$bws["h", "left"]),
    bandwidth_right = unname(model$bws["h", "right"]),
    observations_left = unname(model$N_h[1]),
    observations_right = unname(model$N_h[2]),
    true_effect = true_effect
  )
}

rdd_estimate_scenario <- function(data) {
  outcome_model <- rdrobust::rdrobust(
    y = data$later_academic_score,
    x = data$recorded_test_score,
    c = 0,
    p = 1,
    kernel = "triangular",
    bwselect = "mserd"
  )

  motivation_model <- rdrobust::rdrobust(
    y = data$simulated_latent_motivation,
    x = data$recorded_test_score,
    c = 0,
    p = 1,
    kernel = "triangular",
    bwselect = "mserd"
  )

  density_model <- rddensity::rddensity(
    X = data$recorded_test_score,
    c = 0
  )

  outcome_result <- rdd_extract_estimate(
    outcome_model,
    scenario = unique(data$scenario)
  )

  motivation_result <- data.frame(
    scenario = unique(data$scenario),
    motivation_jump = unname(motivation_model$coef["Robust", "Coeff"]),
    motivation_conf_low =
      unname(motivation_model$ci["Robust", "CI Lower"]),
    motivation_conf_high =
      unname(motivation_model$ci["Robust", "CI Upper"]),
    density_test_statistic = unname(density_model$test$t_jk),
    density_test_p_value = unname(density_model$test$p_jk),
    manipulated_candidates = sum(data$manipulated_score),
    manipulated_share = mean(data$manipulated_score)
  )

  list(
    outcome_model = outcome_model,
    motivation_model = motivation_model,
    density_model = density_model,
    outcome_result = outcome_result,
    diagnostic_result = motivation_result
  )
}


# 4. Prepare figure inputs --------------------------------------------------

rdd_build_binned_outcomes <- function(data, plot_limit = 1.2, bin_width = 0.08) {
  data %>%
    filter(abs(recorded_test_score) <= plot_limit) %>%
    mutate(
      bin = floor(recorded_test_score / bin_width),
      bin_midpoint = (bin + 0.5) * bin_width
    ) %>%
    group_by(scenario, bin_midpoint) %>%
    summarise(
      mean_outcome = mean(later_academic_score),
      observations = n(),
      .groups = "drop"
    )
}

rdd_build_local_fits <- function(data, bandwidth, prediction_points = 100L) {
  bind_rows(lapply(c("Below cutoff", "Above cutoff"), function(side) {
    if (side == "Below cutoff") {
      estimation_data <- data %>%
        filter(recorded_test_score < 0, recorded_test_score >= -bandwidth)
      score_grid <- seq(-bandwidth, 0, length.out = prediction_points)
    } else {
      estimation_data <- data %>%
        filter(recorded_test_score >= 0, recorded_test_score <= bandwidth)
      score_grid <- seq(0, bandwidth, length.out = prediction_points)
    }

    triangular_weights <-
      1 - abs(estimation_data$recorded_test_score) / bandwidth
    local_model <- lm(
      later_academic_score ~ recorded_test_score,
      data = estimation_data,
      weights = triangular_weights
    )

    data.frame(
      scenario = unique(data$scenario),
      side = side,
      recorded_test_score = score_grid,
      fitted_outcome = predict(
        local_model,
        newdata = data.frame(recorded_test_score = score_grid)
      )
    )
  }))
}

rdd_build_density_bins <- function(
    ideal_data,
    failure_data,
    plot_limit = 0.60,
    bin_width = 0.025) {
  bind_rows(ideal_data, failure_data) %>%
    filter(abs(recorded_test_score) <= plot_limit) %>%
    mutate(
      bin = floor(recorded_test_score / bin_width),
      bin_midpoint = (bin + 0.5) * bin_width
    ) %>%
    count(scenario, bin_midpoint, name = "observations") %>%
    group_by(scenario) %>%
    mutate(density = observations / (sum(observations) * bin_width)) %>%
    ungroup()
}

rdd_make_outcome_panel <- function(
    data,
    binned_outcomes,
    local_fits,
    estimate_result,
    panel_label) {
  estimate_text <- sprintf(
    "RD estimate = %.2f; 95%% CI [%.2f, %.2f]",
    estimate_result$estimate,
    estimate_result$conf_low,
    estimate_result$conf_high
  )

  ggplot(binned_outcomes, aes(x = bin_midpoint, y = mean_outcome)) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.7,
      color = "#555555"
    ) +
    geom_point(size = 2.0, color = "#5A7994", alpha = 0.90) +
    geom_line(
      data = local_fits,
      aes(x = recorded_test_score, y = fitted_outcome, group = side),
      inherit.aes = FALSE,
      linewidth = 1.0,
      color = "#C94C32"
    ) +
    coord_cartesian(xlim = c(-1.2, 1.2), ylim = c(66, 88)) +
    labs(
      title = paste0(panel_label, ". ", unique(data$scenario)),
      subtitle = estimate_text,
      x = "Test score relative to scholarship cutoff",
      y = "Later academic score"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank())
}

rdd_make_figure <- function(
    ideal_data,
    failure_data,
    estimates,
    density_bins) {
  ideal_result <- estimates %>% filter(scenario == "Ideal: no manipulation")
  failure_result <- estimates %>%
    filter(scenario == "Failure: manipulation near cutoff")

  ideal_bins <- rdd_build_binned_outcomes(ideal_data)
  failure_bins <- rdd_build_binned_outcomes(failure_data)
  ideal_fits <- rdd_build_local_fits(
    ideal_data,
    bandwidth = ideal_result$bandwidth_left
  )
  failure_fits <- rdd_build_local_fits(
    failure_data,
    bandwidth = failure_result$bandwidth_left
  )

  ideal_panel <- rdd_make_outcome_panel(
    ideal_data,
    ideal_bins,
    ideal_fits,
    ideal_result,
    panel_label = "A"
  )
  failure_panel <- rdd_make_outcome_panel(
    failure_data,
    failure_bins,
    failure_fits,
    failure_result,
    panel_label = "B"
  )

  scenario_colors <- c(
    "Ideal: no manipulation" = "#4C8BCB",
    "Failure: manipulation near cutoff" = "#D9794A"
  )

  density_panel <- ggplot(
    density_bins,
    aes(x = bin_midpoint, y = density, color = scenario)
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.7,
      color = "#555555"
    ) +
    geom_step(linewidth = 0.85, direction = "mid") +
    scale_color_manual(values = scenario_colors) +
    coord_cartesian(xlim = c(-0.60, 0.60)) +
    labs(
      title = "C. Distribution of the recorded test score",
      x = "Test score relative to scholarship cutoff",
      y = "Density",
      color = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  estimate_plot_data <- estimates %>%
    mutate(
      scenario = factor(
        scenario,
        levels = c(
          "Failure: manipulation near cutoff",
          "Ideal: no manipulation"
        )
      )
    )

  estimate_panel <- ggplot(
    estimate_plot_data,
    aes(x = estimate, y = scenario, color = scenario)
  ) +
    geom_vline(
      xintercept = 4,
      linetype = "dashed",
      linewidth = 0.75,
      color = "#238B45"
    ) +
    geom_errorbarh(
      aes(xmin = conf_low, xmax = conf_high),
      height = 0.18,
      linewidth = 0.75
    ) +
    geom_point(size = 3) +
    scale_color_manual(values = scenario_colors) +
    labs(
      title = "D. Estimated discontinuity",
      subtitle = "Dashed line: true scholarship effect = 4 points",
      x = "Effect on later academic score",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  (ideal_panel + failure_panel) /
    (density_panel + estimate_panel) +
    plot_layout(heights = c(1, 0.9))
}


# 5. Main simulation function ----------------------------------------------

# This is the function called from the chapter and from the master runner.
# It returns all key objects and, by default, writes the datasets, tables,
# and figure used in the book.
run_rdd_simulation <- function(
    seed = 123L,
    sample_size = 10000L,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images",
    show_plot = interactive()) {
  population <- rdd_simulate_population(
    seed = seed,
    sample_size = sample_size
  )
  ideal_data <- rdd_create_ideal_data(population)
  failure_data <- rdd_create_failure_data(population, seed = seed)

  ideal_results <- rdd_estimate_scenario(ideal_data)
  failure_results <- rdd_estimate_scenario(failure_data)
  estimates <- bind_rows(
    ideal_results$outcome_result,
    failure_results$outcome_result
  )
  diagnostics <- bind_rows(
    ideal_results$diagnostic_result,
    failure_results$diagnostic_result
  )
  density_bins <- rdd_build_density_bins(ideal_data, failure_data)
  rdd_figure <- rdd_make_figure(
    ideal_data,
    failure_data,
    estimates,
    density_bins
  )

  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      ideal_data,
      file.path(data_directory, "rdd_merit_scholarship_ideal.csv"),
      row.names = FALSE
    )
    write.csv(
      failure_data,
      file.path(data_directory, "rdd_merit_scholarship_failure.csv"),
      row.names = FALSE
    )
    write.csv(
      estimates,
      file.path(data_directory, "rdd_merit_scholarship_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      diagnostics,
      file.path(data_directory, "rdd_merit_scholarship_diagnostics.csv"),
      row.names = FALSE
    )
    ggsave(
      file.path(image_directory, "rdd-merit-scholarship-simulation.png"),
      rdd_figure,
      width = 12,
      height = 9.2,
      dpi = 320,
      bg = "white"
    )
  }

  # Display the figure in RStudio's Plots pane when the function is run
  # interactively. The figure is still saved above when write_outputs = TRUE.
  if (show_plot) {
    print(rdd_figure)
  }

  list(
    ideal_data = ideal_data,
    failure_data = failure_data,
    estimates = estimates,
    diagnostics = diagnostics,
    ideal_outcome_model = ideal_results$outcome_model,
    failure_outcome_model = failure_results$outcome_model,
    ideal_density_model = ideal_results$density_model,
    failure_density_model = failure_results$density_model,
    figure = rdd_figure
  )
}


# 6. Run the script directly ------------------------------------------------

# When this file is run directly, it reproduces the RDD outputs and prints the
# main tables. When it is sourced by another file, this block is skipped.
if (sys.nframe() == 0L) {
  rdd_results <- run_rdd_simulation(
    seed = 123L,
    sample_size = 10000L,
    show_plot = TRUE
  )
  cat("\nRegression-discontinuity estimates\n")
  print(rdd_results$estimates, row.names = FALSE, digits = 3)
  cat("\nManipulation and balance diagnostics\n")
  print(rdd_results$diagnostics, row.names = FALSE, digits = 3)
}
