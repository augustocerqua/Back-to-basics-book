# Section 16.3: Difference-in-Differences
# ============================================================
#
# This script simulates a stylized application inspired by unilateral divorce
# laws and divorce rates.
#
# Example:
#   Twenty states adopt a reform in 1979. Thirty states never adopt it during
#   the sample period. We observe all states from 1975 to 1988.
#
# Goal:
#   Show how DiD works when treated and control states would have followed
#   parallel trends, and how it fails when the control group follows a
#   different untreated trend.
#
# The script creates:
#   1. an ideal DiD setting with credible parallel trends;
#   2. a failure setting with non-parallel trends;
#   3. TWFE estimates and pre-trend diagnostics saved as CSV files;
#   4. event-study diagnostics saved as CSV files;
#   5. a high-resolution PNG figure.
#
# Reproducibility:
#   All random draws use deterministic streams based on master seed 123.


# 0. Packages ---------------------------------------------------------------

required_packages <- c("dplyr", "ggplot2", "patchwork")
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


# 1. Small helper functions -------------------------------------------------

# Clustered standard errors are computed at the state level. This is important
# because each state contributes repeated observations over time.
did_clustered_inference <- function(model, cluster, term) {
  design_matrix <- model.matrix(model)
  residual_vector <- residuals(model)
  cluster <- as.factor(cluster)
  cluster_levels <- levels(cluster)
  n_observations <- nrow(design_matrix)
  n_coefficients <- ncol(design_matrix)
  n_clusters <- length(cluster_levels)

  bread <- solve(crossprod(design_matrix))
  meat <- matrix(0, nrow = n_coefficients, ncol = n_coefficients)

  for (cluster_level in cluster_levels) {
    rows <- cluster == cluster_level
    score <- crossprod(
      design_matrix[rows, , drop = FALSE],
      residual_vector[rows]
    )
    meat <- meat + tcrossprod(score)
  }

  finite_sample_correction <-
    (n_clusters / (n_clusters - 1)) *
    ((n_observations - 1) / (n_observations - n_coefficients))
  variance_matrix <- finite_sample_correction * bread %*% meat %*% bread
  standard_error <- sqrt(diag(variance_matrix))[term]
  estimate <- unname(coef(model)[term])

  data.frame(
    estimate = estimate,
    std_error = unname(standard_error),
    conf_low = estimate - qnorm(0.975) * standard_error,
    conf_high = estimate + qnorm(0.975) * standard_error,
    row.names = NULL
  )
}


# 2. Simulate state-by-year panel data -------------------------------------

# In the ideal case, control_differential_trend is zero. In the failure case,
# the untreated trend of control states differs from that of treated states.
did_simulate_panel <- function(
    seed = 123L,
    n_treated_states = 20L,
    n_control_states = 30L,
    n_pre_periods = 4L,
    n_post_periods = 10L,
    start_year = 1975L,
    true_att = 0.30,
    control_differential_trend = 0) {
  n_states <- n_treated_states + n_control_states
  event_times <- seq.int(-n_pre_periods, n_post_periods - 1L)

  state_data <- data.frame(
    state_id = seq_len(n_states),
    treated_state = c(
      rep(1L, n_treated_states),
      rep(0L, n_control_states)
    )
  )

  set.seed(seed + 510L)
  state_data$state_component <-
    rnorm(n_states, mean = 0, sd = 0.28) +
    0.45 * state_data$treated_state

  set.seed(seed + 520L)
  common_time_shock <- rnorm(
    length(event_times),
    mean = 0,
    sd = 0.055
  )

  panel <- merge(
    state_data,
    data.frame(event_time = event_times),
    by = NULL
  ) %>%
    arrange(state_id, event_time) %>%
    mutate(
      calendar_year = start_year + event_time + n_pre_periods,
      post_reform = as.integer(event_time >= 0),
      treated_post = treated_state * post_reform,
      common_time_shock = common_time_shock[
        match(event_time, event_times)
      ]
    )

  set.seed(seed + 530L)
  state_period_noise <- rnorm(nrow(panel), mean = 0, sd = 0.18)

  panel %>%
    mutate(
      simulated_control_untreated_trend_change =
        (1 - treated_state) * control_differential_trend *
        (event_time + n_pre_periods),
      simulated_y0 =
        4.60 +
        state_component -
        0.015 * event_time +
        common_time_shock +
        simulated_control_untreated_trend_change +
        state_period_noise,
      simulated_y1 = simulated_y0 + true_att,
      simulated_individual_effect = true_att,
      divorce_rate = simulated_y0 + treated_post * true_att
    ) %>%
    select(
      state_id,
      calendar_year,
      event_time,
      treated_state,
      post_reform,
      treated_post,
      divorce_rate,
      simulated_y0,
      simulated_y1,
      simulated_individual_effect,
      simulated_control_untreated_trend_change
    )
}


# 3. Estimate DiD and event-study models -----------------------------------

did_event_term_name <- function(event_time) {
  if (event_time < 0) {
    paste0("event_m", abs(event_time))
  } else {
    paste0("event_p", event_time)
  }
}

did_estimate_event_study <- function(
    data,
    setting,
    reference_period = -1L) {
  event_times <- sort(unique(data$event_time))
  estimated_periods <- setdiff(event_times, reference_period)
  event_terms <- vapply(
    estimated_periods,
    did_event_term_name,
    character(1)
  )

  event_study_data <- data
  for (index in seq_along(estimated_periods)) {
    event_study_data[[event_terms[index]]] <-
      event_study_data$treated_state *
      as.integer(event_study_data$event_time == estimated_periods[index])
  }

  event_study_formula <- reformulate(
    c("factor(state_id)", "factor(event_time)", event_terms),
    response = "divorce_rate"
  )
  event_study_model <- lm(event_study_formula, data = event_study_data)

  estimated_results <- bind_rows(lapply(
    seq_along(estimated_periods),
    function(index) {
      did_clustered_inference(
        event_study_model,
        cluster = event_study_data$state_id,
        term = event_terms[index]
      ) %>%
        mutate(
          event_time = estimated_periods[index],
          reference_period = FALSE
        )
    }
  ))

  reference_result <- data.frame(
    estimate = 0,
    std_error = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    event_time = reference_period,
    reference_period = TRUE
  )

  event_study_results <- bind_rows(
    estimated_results,
    reference_result
  ) %>%
    arrange(event_time) %>%
    mutate(setting = setting, .before = 1)

  list(
    model = event_study_model,
    results = event_study_results
  )
}

did_estimate_twfe <- function(data, setting, true_att) {
  twfe_model <- lm(
    divorce_rate ~ treated_post + factor(state_id) + factor(calendar_year),
    data = data
  )
  twfe_inference <- did_clustered_inference(
    twfe_model,
    cluster = data$state_id,
    term = "treated_post"
  )

  pre_data <- data %>% filter(event_time < 0)
  pretrend_model <- lm(
    divorce_rate ~ treated_state + event_time +
      treated_state:event_time,
    data = pre_data
  )
  pretrend_inference <- did_clustered_inference(
    pretrend_model,
    cluster = pre_data$state_id,
    term = "treated_state:event_time"
  )

  twfe_results <- twfe_inference %>%
    mutate(
      setting = setting,
      true_att = true_att,
      .before = 1
    )
  pretrend_results <- pretrend_inference %>%
    mutate(
      setting = setting,
      .before = 1
    )

  list(
    twfe_model = twfe_model,
    pretrend_model = pretrend_model,
    twfe_results = twfe_results,
    pretrend_results = pretrend_results
  )
}


# 4. Prepare the figure -----------------------------------------------------

did_prepare_plot_data <- function(data, setting) {
  observed_means <- data %>%
    mutate(
      series = if_else(
        treated_state == 1L,
        "Treated states",
        "Control states"
      )
    ) %>%
    group_by(calendar_year, event_time, series) %>%
    summarise(divorce_rate = mean(divorce_rate), .groups = "drop")

  counterfactual_means <- data %>%
    filter(treated_state == 1L, event_time >= -1L) %>%
    group_by(calendar_year, event_time) %>%
    summarise(divorce_rate = mean(simulated_y0), .groups = "drop") %>%
    mutate(series = "Treated counterfactual")

  bind_rows(observed_means, counterfactual_means) %>%
    mutate(setting = setting)
}

did_make_panel <- function(plot_data, panel_title) {
  reform_year <- unique(
    plot_data$calendar_year[plot_data$event_time == 0]
  )[1]
  first_year <- min(plot_data$calendar_year)
  final_year <- max(plot_data$calendar_year)
  year_breaks <- unique(c(
    first_year,
    reform_year - 1,
    reform_year + 2,
    reform_year + 5,
    final_year
  ))
  year_breaks <- year_breaks[
    year_breaks >= first_year & year_breaks <= final_year
  ]

  ggplot(
    plot_data,
    aes(
      x = calendar_year,
      y = divorce_rate,
      color = series,
      linetype = series,
      group = series
    )
  ) +
    geom_vline(
      xintercept = reform_year - 0.5,
      linetype = "dotted",
      linewidth = 0.7,
      color = "#555555"
    ) +
    geom_line(linewidth = 1.05) +
    geom_point(
      data = plot_data %>% filter(series != "Treated counterfactual"),
      size = 2.2
    ) +
    annotate(
      "text",
      x = reform_year - 0.30,
      y = Inf,
      label = "Reform",
      hjust = 0,
      vjust = 1.5,
      size = 3.5,
      color = "#444444"
    ) +
    scale_color_manual(
      values = c(
        "Control states" = "#2C7FB8",
        "Treated states" = "#D95F3D",
        "Treated counterfactual" = "#D95F3D"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Control states" = "solid",
        "Treated states" = "solid",
        "Treated counterfactual" = "dashed"
      )
    ) +
    scale_x_continuous(breaks = year_breaks) +
    coord_cartesian(ylim = c(3.50, 5.55)) +
    labs(
      title = panel_title,
      x = "Year",
      y = "Annual divorces per 1,000 people",
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

did_make_event_study_panel <- function(results, panel_title) {
  ggplot(results, aes(x = event_time, y = estimate)) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.6,
      color = "#555555"
    ) +
    geom_vline(
      xintercept = -0.5,
      linetype = "dotted",
      linewidth = 0.7,
      color = "#555555"
    ) +
    geom_line(linewidth = 0.75, color = "#315A7D") +
    geom_errorbar(
      data = results %>% filter(!reference_period),
      aes(ymin = conf_low, ymax = conf_high),
      width = 0.16,
      linewidth = 0.65,
      color = "#315A7D"
    ) +
    geom_point(
      data = results %>% filter(!reference_period),
      size = 2.3,
      color = "#315A7D"
    ) +
    geom_point(
      data = results %>% filter(reference_period),
      shape = 21,
      size = 2.7,
      stroke = 0.9,
      fill = "white",
      color = "#315A7D"
    ) +
    annotate(
      "text",
      x = -1,
      y = -0.25,
      label = "Reference",
      size = 3.0,
      color = "#555555"
    ) +
    scale_x_continuous(breaks = seq(-4, 9, by = 2)) +
    scale_y_continuous(
      breaks = c(0, 0.3, 0.6, 0.9),
      labels = c("0", "0.3", "0.6", "0.9")
    ) +
    coord_cartesian(ylim = c(-0.30, 1.00)) +
    labs(
      title = panel_title,
      x = "Periods relative to reform",
      y = "Effect relative to period -1"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank())
}

did_make_figure <- function(
    ideal_data,
    failure_data,
    ideal_event_study,
    failure_event_study) {
  ideal_plot_data <- did_prepare_plot_data(
    ideal_data,
    "Parallel trends"
  )
  failure_plot_data <- did_prepare_plot_data(
    failure_data,
    "Different untreated trends"
  )

  ideal_plot <- did_make_panel(
    ideal_plot_data,
    "A. Ideal case: parallel untreated trends"
  )
  failure_plot <- did_make_panel(
    failure_plot_data,
    "B. Failure: control states follow a different trend"
  )

  ideal_event_study_plot <- did_make_event_study_panel(
    ideal_event_study,
    "C. Event study: parallel trends"
  )
  failure_event_study_plot <- did_make_event_study_panel(
    failure_event_study,
    "D. Event study: non-parallel trends"
  )

  (ideal_plot + failure_plot) /
    (ideal_event_study_plot + failure_event_study_plot) +
    plot_layout(guides = "collect", heights = c(1, 1)) &
    theme(legend.position = "bottom")
}


# 5. Main simulation function ----------------------------------------------

# This is the function called from the chapter and from the master runner.
# It returns all key objects and, by default, writes the datasets, tables,
# and figure used in the book.
run_did_simulation <- function(
    seed = 123L,
    n_treated_states = 20L,
    n_control_states = 30L,
    n_pre_periods = 4L,
    n_post_periods = 10L,
    start_year = 1975L,
    true_att = 0.30,
    failure_control_differential_trend = -0.06,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images",
    show_plot = interactive()) {
  ideal_data <- did_simulate_panel(
    seed = seed,
    n_treated_states = n_treated_states,
    n_control_states = n_control_states,
    n_pre_periods = n_pre_periods,
    n_post_periods = n_post_periods,
    start_year = start_year,
    true_att = true_att,
    control_differential_trend = 0
  )
  failure_data <- did_simulate_panel(
    seed = seed,
    n_treated_states = n_treated_states,
    n_control_states = n_control_states,
    n_pre_periods = n_pre_periods,
    n_post_periods = n_post_periods,
    start_year = start_year,
    true_att = true_att,
    control_differential_trend = failure_control_differential_trend
  )

  ideal_results <- did_estimate_twfe(
    ideal_data,
    setting = "Ideal: parallel trends",
    true_att = true_att
  )
  failure_results <- did_estimate_twfe(
    failure_data,
    setting = "Failure: different untreated trends",
    true_att = true_att
  )
  ideal_event_study <- did_estimate_event_study(
    ideal_data,
    setting = "Ideal: parallel trends",
    reference_period = -1L
  )
  failure_event_study <- did_estimate_event_study(
    failure_data,
    setting = "Failure: different untreated trends",
    reference_period = -1L
  )

  twfe_results <- bind_rows(
    ideal_results$twfe_results,
    failure_results$twfe_results
  )
  pretrend_results <- bind_rows(
    ideal_results$pretrend_results,
    failure_results$pretrend_results
  )
  event_study_results <- bind_rows(
    ideal_event_study$results,
    failure_event_study$results
  )
  did_figure <- did_make_figure(
    ideal_data,
    failure_data,
    ideal_event_study$results,
    failure_event_study$results
  )

  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      ideal_data,
      file.path(data_directory, "did_unilateral_divorce_ideal.csv"),
      row.names = FALSE
    )
    write.csv(
      failure_data,
      file.path(data_directory, "did_unilateral_divorce_failure.csv"),
      row.names = FALSE
    )
    write.csv(
      twfe_results,
      file.path(data_directory, "did_unilateral_divorce_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      pretrend_results,
      file.path(data_directory, "did_unilateral_divorce_pretrends.csv"),
      row.names = FALSE
    )
    write.csv(
      event_study_results,
      file.path(data_directory, "did_unilateral_divorce_event_study.csv"),
      row.names = FALSE
    )
    ggsave(
      file.path(image_directory, "did-unilateral-divorce-simulation.png"),
      did_figure,
      width = 12,
      height = 10.2,
      dpi = 320,
      bg = "white"
    )
  }

  # Display the figure in RStudio's Plots pane when the function is run
  # interactively. The figure is still saved above when write_outputs = TRUE.
  if (show_plot) {
    print(did_figure)
  }

  list(
    ideal_data = ideal_data,
    failure_data = failure_data,
    twfe_estimates = twfe_results,
    pretrend_estimates = pretrend_results,
    event_study_estimates = event_study_results,
    ideal_twfe_model = ideal_results$twfe_model,
    failure_twfe_model = failure_results$twfe_model,
    ideal_event_study_model = ideal_event_study$model,
    failure_event_study_model = failure_event_study$model,
    did_figure = did_figure
  )
}


# 6. Run the script directly ------------------------------------------------

# When this file is run directly, it reproduces the DiD outputs and prints the
# main tables. When it is sourced by another file, this block is skipped.
if (sys.nframe() == 0L) {
  did_results <- run_did_simulation(seed = 123L, show_plot = TRUE)
  cat("\nDifference-in-differences estimates\n")
  print(did_results$twfe_estimates, row.names = FALSE, digits = 3)
  cat("\nPre-treatment trend diagnostics\n")
  print(did_results$pretrend_estimates, row.names = FALSE, digits = 3)
}
