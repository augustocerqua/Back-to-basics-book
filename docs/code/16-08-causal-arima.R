# Section 16.8: Causal ARIMA
# ============================================================
#
# This script simulates a forecast-based counterfactual method.
#
# Example:
#   A national workplace-safety regulation is introduced for all firms at the
#   same time. Since the reform is national, there is no untreated comparison
#   group within the country.
#
# Goal:
#   Show how Causal ARIMA uses the pre-treatment history of one national time
#   series to forecast the post-treatment counterfactual path.
#
# The script creates:
#   1. an ideal setting, where the safety regulation is the only relevant
#      post-treatment intervention;
#   2. a failure setting, where an additional national enforcement policy
#      begins in June 2022 and also reduces accidents;
#   3. datasets and estimate tables saved as CSV files;
#   4. a high-resolution PNG figure.
#
# Reproducibility:
#   All random draws use deterministic streams based on master seed 123.


# 0. Packages ---------------------------------------------------------------

required_packages <- c(
  "CausalArima", "dplyr", "tidyr", "ggplot2", "patchwork"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    ". The CausalArima package can be installed with ",
    "remotes::install_github('FMenchetti/CausalArima')."
  )
}

suppressPackageStartupMessages(library(CausalArima))
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


# 1. Simulate one national time series -------------------------------------

# The simulated dataset has:
#   - 50 pre-treatment monthly observations;
#   - 10 post-treatment monthly observations;
#   - one treated unit: the country.
#
# The untreated accident rate is designed to have three realistic ingredients:
#   - a mild downward trend;
#   - annual seasonality;
#   - serially correlated shocks.
causal_arima_simulate_data <- function(
    seed = 123L,
    pre_periods = 50L,
    post_periods = 10L) {
  total_periods <- pre_periods + post_periods

  dates <- seq.Date(
    from = as.Date("2018-01-01"),
    by = "month",
    length.out = total_periods
  )

  time_index <- seq_len(total_periods)

  # Persistent AR(1) fluctuations around the systematic trend and seasonality.
  set.seed(seed + 687L)
  innovations <- rnorm(total_periods, mean = 0, sd = 0.16)
  ar_component <- numeric(total_periods)

  for (period in seq_len(total_periods)) {
    lagged_error <- if (period == 1L) 0 else ar_component[period - 1L]
    ar_component[period] <- 0.55 * lagged_error + innovations[period]
  }

  untreated_accident_rate <-
    10.6 -
    0.008 * time_index +
    0.48 * sin(2 * pi * time_index / 12) +
    0.24 * cos(2 * pi * time_index / 12) +
    ar_component

  # True effect of the workplace-safety regulation.
  # Negative values mean that the policy reduces accident rates.
  policy_effect_post <- -c(
    0.60, 0.90, 1.20, 1.50, 1.60,
    1.70, 1.80, 1.80, 1.90, 2.00
  )
  policy_effect <- c(rep(0, pre_periods), policy_effect_post)

  # Failure scenario:
  # A separate enforcement policy starts in June 2022, after the main reform.
  # It also lowers accidents. If the model attributes this additional drop to
  # the original regulation, the policy effect is overstated.
  concurrent_policy_effect <- ifelse(
    dates >= as.Date("2022-06-01"),
    -1.00,
    0
  )

  ideal_data <- data.frame(
    scenario = "Ideal: stable untreated process",
    date = dates,
    period = time_index,
    post_treatment = time_index > pre_periods,
    observed_accident_rate = untreated_accident_rate + policy_effect,
    untreated_accident_rate = untreated_accident_rate,
    true_policy_effect = policy_effect,
    concurrent_policy_effect = 0
  )

  failure_data <- data.frame(
    scenario = "Failure: additional policy from June 2022",
    date = dates,
    period = time_index,
    post_treatment = time_index > pre_periods,
    observed_accident_rate =
      untreated_accident_rate + policy_effect + concurrent_policy_effect,
    untreated_accident_rate = untreated_accident_rate,
    true_policy_effect = policy_effect,
    concurrent_policy_effect = concurrent_policy_effect
  )

  list(
    ideal_data = ideal_data,
    failure_data = failure_data,
    intervention_date = dates[pre_periods + 1L],
    pre_periods = pre_periods,
    post_periods = post_periods
  )
}


# 2. Time-series covariates -------------------------------------------------

# The ARIMA model includes deterministic covariates for:
#   - a linear trend;
#   - annual seasonality through sine and cosine terms.
causal_arima_covariates <- function(period) {
  cbind(
    trend = period,
    annual_sine = sin(2 * pi * period / 12),
    annual_cosine = cos(2 * pi * period / 12)
  )
}


# 3. Estimate one scenario --------------------------------------------------

# CausalArima estimates the pre-treatment outcome process and then forecasts
# the post-treatment counterfactual path. The causal effect is the difference
# between the observed post-treatment outcome and this forecasted path.
causal_arima_estimate_scenario <- function(
    data,
    intervention_date,
    seed,
    nboot = 1000L) {
  seasonal_covariates <- causal_arima_covariates(data$period)

  set.seed(seed)
  fit <- CausalArima::CausalArima(
    y = ts(data$observed_accident_rate, frequency = 12),
    dates = data$date,
    int.date = intervention_date,
    auto = FALSE,
    order = c(1, 0, 0),
    seasonal = c(0, 0, 0),
    xreg = seasonal_covariates,
    nboot = nboot,
    alpha = 0.05
  )

  post_data <- data %>% filter(post_treatment)
  post_count <- nrow(post_data)

  # fit$norm$inf contains normal-approximation inference for cumulative and
  # average effects. The final row summarizes the full post-treatment window.
  normal_inference <- fit$norm$inf
  final_horizon <- normal_inference[post_count, ]

  effect_path <- post_data %>%
    mutate(
      forecasted_counterfactual = as.numeric(fit$forecast),
      prediction_interval_lower = as.numeric(fit$forecast_lower),
      prediction_interval_upper = as.numeric(fit$forecast_upper),
      estimated_effect = as.numeric(fit$causal.effect),
      effect_standard_error = normal_inference[, "sd.tau"],
      effect_interval_lower =
        estimated_effect - qnorm(0.975) * effect_standard_error,
      effect_interval_upper =
        estimated_effect + qnorm(0.975) * effect_standard_error
    )

  average_effect <- unname(final_horizon["avg"])
  average_effect_se <- unname(final_horizon["sd.avg"])
  average_interval <-
    average_effect + c(-1, 1) * qnorm(0.975) * average_effect_se

  pre_residuals <- as.numeric(residuals(fit$model))

  results <- data.frame(
    scenario = unique(data$scenario),
    pre_treatment_periods = sum(!data$post_treatment),
    post_treatment_periods = sum(data$post_treatment),
    pre_treatment_rmse = sqrt(mean(pre_residuals^2, na.rm = TRUE)),
    estimated_temporal_average_effect = average_effect,
    standard_error = average_effect_se,
    confidence_interval_lower = average_interval[1],
    confidence_interval_upper = average_interval[2],
    true_average_policy_effect = mean(post_data$true_policy_effect),
    average_concurrent_policy_effect =
      mean(post_data$concurrent_policy_effect),
    estimation_error_for_policy_effect =
      average_effect - mean(post_data$true_policy_effect)
  )

  full_path <- data %>%
    left_join(
      effect_path %>%
        select(
          date,
          forecasted_counterfactual,
          prediction_interval_lower,
          prediction_interval_upper,
          estimated_effect,
          effect_interval_lower,
          effect_interval_upper
        ),
      by = "date"
    )

  list(
    fit = fit,
    results = results,
    paths = full_path,
    effects = effect_path
  )
}


# 4. Build the figure -------------------------------------------------------

causal_arima_path_panel <- function(
    paths,
    panel_title,
    intervention_date,
    y_limits,
    additional_policy_date = NULL) {
  forecast_display <- paths %>%
    filter(post_treatment) %>%
    select(
      date,
      forecasted_counterfactual,
      prediction_interval_lower,
      prediction_interval_upper
    )

  additional_policy_data <- if (is.null(additional_policy_date)) {
    data.frame(date = as.Date(character()), label_y = numeric())
  } else {
    data.frame(
      date = as.Date(additional_policy_date),
      label_y = y_limits[2] - 0.22
    )
  }

  ggplot(paths, aes(x = date)) +
    geom_ribbon(
      data = forecast_display,
      aes(
        ymin = prediction_interval_lower,
        ymax = prediction_interval_upper
      ),
      inherit.aes = TRUE,
      fill = "#B8CCE0",
      alpha = 0.55
    ) +
    geom_line(
      aes(y = observed_accident_rate, color = "Observed accident rate"),
      linewidth = 0.95
    ) +
    geom_line(
      data = forecast_display,
      aes(
        y = forecasted_counterfactual,
        color = "Forecasted no-reform path"
      ),
      linewidth = 0.95,
      linetype = "longdash"
    ) +
    geom_vline(
      xintercept = as.numeric(intervention_date),
      linetype = "dotted",
      linewidth = 0.75,
      color = "#444444"
    ) +
    geom_vline(
      data = additional_policy_data,
      aes(xintercept = as.numeric(date)),
      linetype = "dotdash",
      linewidth = 0.75,
      color = "#D9A420"
    ) +
    annotate(
      "text",
      x = intervention_date,
      y = max(paths$observed_accident_rate) + 0.28,
      label = "Reform",
      hjust = -0.08,
      size = 3.5,
      fontface = "bold",
      color = "#444444"
    ) +
    geom_text(
      data = additional_policy_data,
      aes(x = date, y = label_y, label = "Additional policy"),
      inherit.aes = FALSE,
      hjust = 1.0,
      vjust = -0.25,
      angle = 90,
      size = 3.0,
      fontface = "bold",
      color = "#B07B00"
    ) +
    scale_color_manual(
      values = c(
        "Observed accident rate" = "#D4513F",
        "Forecasted no-reform path" = "#275D8C"
      )
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(ylim = y_limits) +
    labs(
      title = panel_title,
      x = NULL,
      y = "Accidents per 1,000 workers",
      color = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}


causal_arima_make_figure <- function(paths, results, intervention_date) {
  ideal_paths <- paths %>%
    filter(scenario == "Ideal: stable untreated process")

  failure_paths <- paths %>%
    filter(scenario == "Failure: additional policy from June 2022")

  # Use the same y-axis support in Panels A and B so the visual comparison is
  # not driven by different axis scales.
  common_y_limits <- range(
    paths$observed_accident_rate,
    paths$prediction_interval_lower,
    paths$prediction_interval_upper,
    na.rm = TRUE
  ) + c(-0.25, 0.45)

  ideal_panel <- causal_arima_path_panel(
    ideal_paths,
    "A. Ideal setting",
    intervention_date,
    common_y_limits,
    additional_policy_date = NULL
  )

  failure_panel <- causal_arima_path_panel(
    failure_paths,
    "B. Failure: additional policy from June 2022",
    intervention_date,
    common_y_limits,
    additional_policy_date = as.Date("2022-06-01")
  )

  effect_data <- paths %>%
    filter(post_treatment) %>%
    transmute(
      scenario = recode(
        scenario,
        "Ideal: stable untreated process" = "Ideal C-ARIMA estimate",
        "Failure: additional policy from June 2022" =
          "Failure C-ARIMA estimate"
      ),
      date,
      estimated_effect,
      effect_interval_lower,
      effect_interval_upper,
      true_policy_effect
    )

  true_effect_data <- effect_data %>%
    filter(scenario == "Ideal C-ARIMA estimate") %>%
    select(date, true_policy_effect)

  effect_panel <- ggplot(
    effect_data,
    aes(x = date, y = estimated_effect, color = scenario)
  ) +
    geom_hline(yintercept = 0, linewidth = 0.55, color = "#777777") +
    geom_ribbon(
      aes(
        ymin = effect_interval_lower,
        ymax = effect_interval_upper,
        fill = scenario
      ),
      alpha = 0.10,
      color = NA
    ) +
    geom_line(linewidth = 1.0) +
    geom_line(
      data = true_effect_data,
      aes(x = date, y = true_policy_effect, color = "True policy effect"),
      inherit.aes = FALSE,
      linewidth = 1.0,
      linetype = "longdash"
    ) +
    scale_color_manual(
      values = c(
        "Ideal C-ARIMA estimate" = "#275D8C",
        "Failure C-ARIMA estimate" = "#D4513F",
        "True policy effect" = "#25834D"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Ideal C-ARIMA estimate" = "#275D8C",
        "Failure C-ARIMA estimate" = "#D4513F"
      )
    ) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
    labs(
      title = "C. Estimated and true period-specific effects",
      x = NULL,
      y = "Effect on accident rate",
      color = NULL,
      fill = NULL
    ) +
    guides(fill = "none") +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  average_data <- results %>%
    mutate(
      setting = recode(
        scenario,
        "Ideal: stable untreated process" = "Ideal",
        "Failure: additional policy from June 2022" = "Failure"
      ),
      setting = factor(setting, levels = c("Failure", "Ideal"))
    )

  average_panel <- ggplot(
    average_data,
    aes(x = estimated_temporal_average_effect, y = setting)
  ) +
    geom_vline(
      xintercept = unique(results$true_average_policy_effect),
      color = "#25834D",
      linewidth = 0.9,
      linetype = "longdash"
    ) +
    geom_errorbarh(
      aes(
        xmin = confidence_interval_lower,
        xmax = confidence_interval_upper
      ),
      height = 0.14,
      linewidth = 0.8,
      color = "#555555"
    ) +
    geom_point(aes(color = setting), size = 3.4) +
    scale_color_manual(
      values = c("Ideal" = "#275D8C", "Failure" = "#D4513F")
    ) +
    labs(
      title = "D. Temporal average effects",
      subtitle = "Dashed line: true average policy effect",
      x = "Average effect on accident rate",
      y = NULL
    ) +
    guides(color = "none") +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank())

  (ideal_panel + failure_panel) /
    (effect_panel + average_panel) +
    plot_layout(heights = c(1, 0.95))
}


# 5. Main simulation function ----------------------------------------------

# This is the function called from the chapter and from the master runner.
# It returns all key objects and, by default, writes the datasets, tables,
# and figure used in the book.
run_causal_arima_simulation <- function(
    seed = 123L,
    nboot = 1000L,
    write_outputs = TRUE,
    show_plot = interactive(),
    data_directory = "data/simulations",
    image_directory = "images") {

  # Step 1: simulate the ideal and failure time series.
  simulated <- causal_arima_simulate_data(seed = seed)

  # Step 2: estimate Causal ARIMA in the ideal scenario.
  ideal <- causal_arima_estimate_scenario(
    data = simulated$ideal_data,
    intervention_date = simulated$intervention_date,
    seed = seed + 820L,
    nboot = nboot
  )

  # Step 3: estimate Causal ARIMA in the failure scenario.
  failure <- causal_arima_estimate_scenario(
    data = simulated$failure_data,
    intervention_date = simulated$intervention_date,
    seed = seed + 830L,
    nboot = nboot
  )

  results <- bind_rows(ideal$results, failure$results)
  paths <- bind_rows(ideal$paths, failure$paths)

  causal_arima_figure <- causal_arima_make_figure(
    paths,
    results,
    simulated$intervention_date
  )

  # Step 4: save book outputs.
  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)

    write.csv(
      simulated$ideal_data,
      file.path(data_directory, "causal_arima_workplace_safety_ideal.csv"),
      row.names = FALSE
    )

    write.csv(
      simulated$failure_data,
      file.path(data_directory, "causal_arima_workplace_safety_failure.csv"),
      row.names = FALSE
    )

    write.csv(
      results,
      file.path(data_directory, "causal_arima_workplace_safety_estimates.csv"),
      row.names = FALSE
    )

    write.csv(
      paths,
      file.path(data_directory, "causal_arima_workplace_safety_paths.csv"),
      row.names = FALSE
    )

    ggsave(
      file.path(image_directory, "causal-arima-workplace-safety-simulation.png"),
      causal_arima_figure,
      width = 12,
      height = 9.2,
      dpi = 320,
      bg = "white"
    )
  }

  # Display the figure in RStudio's Plots pane when the function is run
  # interactively. The figure is still saved above when write_outputs = TRUE.
  if (show_plot) {
    print(causal_arima_figure)
  }

  list(
    ideal_data = simulated$ideal_data,
    failure_data = simulated$failure_data,
    estimates = results,
    paths = paths,
    ideal_fit = ideal$fit,
    failure_fit = failure$fit,
    figure = causal_arima_figure
  )
}


# 6. Run the script directly ------------------------------------------------

# When this file is run directly, it reproduces the Causal ARIMA outputs and
# prints the main estimate table. When it is sourced by another file, this
# block is skipped.
if (sys.nframe() == 0L) {
  causal_arima_results <- run_causal_arima_simulation(
    seed = 123L,
    show_plot = TRUE
  )

  cat("\nCausal ARIMA estimates\n")
  print(causal_arima_results$estimates, row.names = FALSE, digits = 3)
}
