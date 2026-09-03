# Section 16.4: Event studies with staggered adoption
#
# This self-contained script extends the unilateral-divorce simulation to two
# treatment cohorts. It estimates group-time effects and their dynamic
# aggregation using the Callaway and Sant'Anna estimator implemented in the
# did package. All random draws use stable streams based on master seed 123.

required_packages <- c("did", "dplyr", "ggplot2", "patchwork")
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

es_simulate_panel <- function(
    seed = 123L,
    start_year = 1975L,
    final_year = 1988L,
    n_never_treated = 30L,
    n_early_cohort = 10L,
    n_late_cohort = 10L,
    early_treatment_year = 1979L,
    late_treatment_year = 1982L,
    early_effect = 0.18,
    late_effect = 0.30) {
  years <- seq.int(start_year, final_year)
  n_states <- n_never_treated + n_early_cohort + n_late_cohort

  state_data <- data.frame(
    state_id = seq_len(n_states),
    first_treat = c(
      rep(early_treatment_year, n_early_cohort),
      rep(late_treatment_year, n_late_cohort),
      rep(0L, n_never_treated)
    )
  ) %>%
    mutate(
      cohort = case_when(
        first_treat == early_treatment_year ~ "1979 cohort",
        first_treat == late_treatment_year ~ "1982 cohort",
        TRUE ~ "Never treated"
      ),
      simulated_cohort_effect = case_when(
        first_treat == early_treatment_year ~ early_effect,
        first_treat == late_treatment_year ~ late_effect,
        TRUE ~ 0
      )
    )

  set.seed(seed + 610L)
  state_data$state_component <-
    rnorm(n_states, mean = 0, sd = 0.28) +
    ifelse(
      state_data$first_treat == early_treatment_year,
      0.42,
      ifelse(
        state_data$first_treat == late_treatment_year,
        0.20,
        0
      )
    )

  set.seed(seed + 620L)
  common_time_shock <- rnorm(length(years), mean = 0, sd = 0.045)

  panel <- merge(
    state_data,
    data.frame(calendar_year = years),
    by = NULL
  ) %>%
    arrange(state_id, calendar_year) %>%
    mutate(
      common_time_shock = common_time_shock[
        match(calendar_year, years)
      ],
      treated = as.integer(
        first_treat > 0 & calendar_year >= first_treat
      ),
      event_time = if_else(
        first_treat > 0,
        calendar_year - first_treat,
        NA_integer_
      )
    )

  set.seed(seed + 630L)
  state_year_noise <- rnorm(nrow(panel), mean = 0, sd = 0.075)

  panel %>%
    mutate(
      simulated_y0 =
        4.60 +
        state_component -
        0.015 * (calendar_year - start_year) +
        common_time_shock +
        state_year_noise,
      simulated_y1 = simulated_y0 + simulated_cohort_effect,
      divorce_rate = simulated_y0 +
        treated * simulated_cohort_effect
    ) %>%
    select(
      state_id,
      calendar_year,
      first_treat,
      cohort,
      treated,
      event_time,
      divorce_rate,
      simulated_y0,
      simulated_y1,
      simulated_cohort_effect
    )
}

es_add_confidence_interval <- function(data) {
  data %>%
    mutate(
      conf_low = estimate - qnorm(0.975) * std_error,
      conf_high = estimate + qnorm(0.975) * std_error
    )
}

es_estimate_callaway_santanna <- function(
    data,
    seed = 123L,
    bootstrap_iterations = 999L) {
  set.seed(seed + 640L)
  group_time_model <- did::att_gt(
    yname = "divorce_rate",
    tname = "calendar_year",
    idname = "state_id",
    gname = "first_treat",
    xformla = ~1,
    data = data,
    panel = TRUE,
    control_group = "nevertreated",
    base_period = "universal",
    anticipation = 0,
    est_method = "reg",
    bstrap = TRUE,
    biters = bootstrap_iterations,
    cband = FALSE,
    clustervars = "state_id",
    print_details = FALSE
  )

  set.seed(seed + 650L)
  dynamic_model <- did::aggte(
    group_time_model,
    type = "dynamic",
    na.rm = TRUE,
    bstrap = TRUE,
    biters = bootstrap_iterations,
    cband = FALSE,
    clustervars = "state_id"
  )

  set.seed(seed + 660L)
  group_model <- did::aggte(
    group_time_model,
    type = "group",
    na.rm = TRUE,
    bstrap = TRUE,
    biters = bootstrap_iterations,
    cband = FALSE,
    clustervars = "state_id"
  )

  group_time_results <- data.frame(
    cohort_year = group_time_model$group,
    calendar_year = group_time_model$t,
    estimate = group_time_model$att,
    std_error = group_time_model$se
  ) %>%
    mutate(
      event_time = calendar_year - cohort_year,
      cohort = paste(cohort_year, "cohort"),
      reference_period = event_time == -1
    ) %>%
    es_add_confidence_interval()

  dynamic_results <- data.frame(
    event_time = dynamic_model$egt,
    estimate = dynamic_model$att.egt,
    std_error = dynamic_model$se.egt
  ) %>%
    mutate(reference_period = event_time == -1) %>%
    es_add_confidence_interval()

  group_results <- data.frame(
    cohort_year = group_model$egt,
    estimate = group_model$att.egt,
    std_error = group_model$se.egt
  ) %>%
    mutate(cohort = paste(cohort_year, "cohort")) %>%
    es_add_confidence_interval()

  list(
    group_time_model = group_time_model,
    dynamic_model = dynamic_model,
    group_model = group_model,
    group_time_results = group_time_results,
    dynamic_results = dynamic_results,
    group_results = group_results
  )
}

es_build_composition <- function(
    data,
    first_post_event = 0L,
    final_post_event = 9L) {
  cohort_data <- data %>%
    filter(first_treat > 0) %>%
    distinct(state_id, first_treat, cohort, simulated_cohort_effect)
  final_year <- max(data$calendar_year)

  composition <- bind_rows(lapply(
    seq.int(first_post_event, final_post_event),
    function(event_time_value) {
      cohort_data %>%
        filter(first_treat + event_time_value <= final_year) %>%
        count(
          event_time = event_time_value,
          cohort,
          simulated_cohort_effect,
          name = "contributing_states"
        )
    }
  ))

  true_aggregate <- composition %>%
    group_by(event_time) %>%
    summarise(
      true_aggregate_effect = weighted.mean(
        simulated_cohort_effect,
        contributing_states
      ),
      contributing_states = sum(contributing_states),
      contributing_cohorts = n(),
      .groups = "drop"
    )

  list(
    composition = composition,
    true_aggregate = true_aggregate
  )
}

es_make_cohort_panel <- function(group_time_results) {
  plot_data <- group_time_results %>%
    filter(event_time >= -7, event_time <= 9) %>%
    mutate(
      plot_event_time = event_time + if_else(
        cohort_year == 1979,
        -0.055,
        0.055
      )
    )

  true_lines <- data.frame(
    cohort = c("1979 cohort", "1982 cohort"),
    start = c(0, 0),
    end = c(9, 6),
    true_effect = c(0.18, 0.30)
  )

  ggplot(
    plot_data,
    aes(
      x = plot_event_time,
      y = estimate,
      color = cohort,
      group = cohort
    )
  ) +
    geom_hline(yintercept = 0, linewidth = 0.55, color = "#555555") +
    geom_vline(
      xintercept = -0.5,
      linetype = "dotted",
      linewidth = 0.65,
      color = "#555555"
    ) +
    geom_segment(
      data = true_lines,
      aes(
        x = start,
        xend = end,
        y = true_effect,
        yend = true_effect,
        color = cohort
      ),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.75,
      alpha = 0.8
    ) +
    geom_line(linewidth = 0.75) +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      width = 0.12,
      linewidth = 0.55
    ) +
    geom_point(size = 2.1) +
    scale_color_manual(
      values = c(
        "1979 cohort" = "#D95F3D",
        "1982 cohort" = "#2C7FB8"
      )
    ) +
    scale_x_continuous(
      breaks = c(-7, -4, -1, 2, 5, 8),
      labels = c("-7", "-4", "-1", "2", "5", "8")
    ) +
    coord_cartesian(ylim = c(-0.18, 0.52)) +
    labs(
      title = "A. Cohort-specific group-time effects",
      x = "Event time",
      y = "Estimated effect",
      color = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

es_make_aggregate_panel <- function(dynamic_results, true_aggregate) {
  plot_data <- dynamic_results %>%
    filter(event_time >= -7, event_time <= 9)

  ggplot(plot_data, aes(x = event_time, y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.55, color = "#555555") +
    geom_vline(
      xintercept = -0.5,
      linetype = "dotted",
      linewidth = 0.65,
      color = "#555555"
    ) +
    geom_vline(
      xintercept = 6.5,
      linetype = "dashed",
      linewidth = 0.65,
      color = "#777777"
    ) +
    geom_step(
      data = true_aggregate,
      aes(x = event_time, y = true_aggregate_effect),
      inherit.aes = FALSE,
      direction = "mid",
      linewidth = 0.9,
      linetype = "dashed",
      color = "#238B45"
    ) +
    geom_line(linewidth = 0.8, color = "#315A7D") +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      width = 0.14,
      linewidth = 0.6,
      color = "#315A7D"
    ) +
    geom_point(size = 2.3, color = "#315A7D") +
    annotate(
      "text",
      x = 6.3,
      y = 0.45,
      label = "Composition\nchanges",
      hjust = 1,
      size = 3.1,
      color = "#555555"
    ) +
    scale_x_continuous(breaks = c(-7, -4, -1, 2, 5, 8)) +
    coord_cartesian(ylim = c(-0.18, 0.52)) +
    labs(
      title = "B. Aggregated Callaway-Sant'Anna event study",
      x = "Event time",
      y = "Aggregated effect"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.minor = element_blank())
}

es_make_composition_panel <- function(composition) {
  ggplot(
    composition,
    aes(x = event_time, y = contributing_states, fill = cohort)
  ) +
    geom_col(width = 0.72) +
    geom_vline(
      xintercept = 6.5,
      linetype = "dashed",
      linewidth = 0.65,
      color = "#777777"
    ) +
    annotate(
      "text",
      x = 6.7,
      y = 18.7,
      label = "1982 cohort\ndrops out",
      hjust = 0,
      vjust = 1,
      size = 3.1,
      color = "#555555"
    ) +
    scale_fill_manual(
      values = c(
        "1979 cohort" = "#D95F3D",
        "1982 cohort" = "#2C7FB8"
      )
    ) +
    scale_x_continuous(breaks = 0:9) +
    scale_y_continuous(breaks = c(0, 10, 20), limits = c(0, 20)) +
    labs(
      title = "C. Cohorts contributing at each post-treatment horizon",
      x = "Event time",
      y = "Number of treated states",
      fill = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

es_make_figure <- function(
    group_time_results,
    dynamic_results,
    composition,
    true_aggregate) {
  cohort_panel <- es_make_cohort_panel(group_time_results)
  aggregate_panel <- es_make_aggregate_panel(
    dynamic_results,
    true_aggregate
  )
  composition_panel <- es_make_composition_panel(composition)

  cohort_panel /
    (aggregate_panel + composition_panel) +
    plot_layout(heights = c(1, 1))
}

run_event_study_simulation <- function(
    seed = 123L,
    bootstrap_iterations = 999L,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images") {
  event_study_data <- es_simulate_panel(seed = seed)
  estimates <- es_estimate_callaway_santanna(
    event_study_data,
    seed = seed,
    bootstrap_iterations = bootstrap_iterations
  )
  composition <- es_build_composition(event_study_data)
  event_study_figure <- es_make_figure(
    estimates$group_time_results,
    estimates$dynamic_results,
    composition$composition,
    composition$true_aggregate
  )

  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      event_study_data,
      file.path(data_directory, "event_study_unilateral_divorce.csv"),
      row.names = FALSE
    )
    write.csv(
      estimates$group_time_results,
      file.path(data_directory, "event_study_group_time_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      estimates$dynamic_results,
      file.path(data_directory, "event_study_dynamic_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      composition$composition,
      file.path(data_directory, "event_study_composition.csv"),
      row.names = FALSE
    )
    ggsave(
      file.path(image_directory, "event-study-cohort-aggregation.png"),
      event_study_figure,
      width = 12,
      height = 9.4,
      dpi = 320,
      bg = "white"
    )
  }

  list(
    data = event_study_data,
    group_time_estimates = estimates$group_time_results,
    dynamic_estimates = estimates$dynamic_results,
    group_estimates = estimates$group_results,
    composition = composition$composition,
    true_aggregate = composition$true_aggregate,
    group_time_model = estimates$group_time_model,
    dynamic_model = estimates$dynamic_model,
    group_model = estimates$group_model,
    figure = event_study_figure
  )
}

if (sys.nframe() == 0L) {
  event_study_results <- run_event_study_simulation(seed = 123L)
  cat("\nCohort-average effects\n")
  print(event_study_results$group_estimates, row.names = FALSE, digits = 3)
  cat("\nAggregated post-treatment event-study effects\n")
  print(
    event_study_results$dynamic_estimates %>% filter(event_time >= 0),
    row.names = FALSE,
    digits = 3
  )
  cat("\nEvent-time composition\n")
  print(event_study_results$true_aggregate, row.names = FALSE, digits = 3)
}
