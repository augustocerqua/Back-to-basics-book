# Section 16.7: Synthetic control and the emergence of a sports star
#
# This self-contained script studies a stylized rise in youth cycling after the
# emergence of a Slovenian cycling star. The ideal treated path lies within the
# convex hull of a restricted donor pool. In the failure setting, Slovenia has
# a peculiar accelerating pre-treatment path that no convex combination of the
# donors can reproduce. All random draws use stable streams based on seed 123.

required_packages <- c("dplyr", "tidyr", "ggplot2", "patchwork", "quadprog")
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

scm_simulate_donor_pool <- function(
    seed = 123L,
    years = 2008:2024) {
  donor_names <- c(
    "Croatia", "Estonia", "Latvia", "Lithuania",
    "Slovakia", "Finland", "Ireland"
  )
  time_index <- seq_along(years) - 1L
  intercept_shift <- c(-0.20, 0.30, -0.10, 0.50, 0.10, 0.80, -0.40)
  slope_shift <- c(0.05, -0.02, 0.00, 0.03, 0.06, 0.02, 0.08)
  phases <- seq(0, pi, length.out = length(donor_names))

  set.seed(seed + 910L)
  donor_matrix <- sapply(seq_along(donor_names), function(j) {
    5 +
      intercept_shift[j] +
      (0.18 + slope_shift[j]) * time_index +
      0.13 * sin(time_index / 2 + phases[j]) +
      rnorm(length(years), mean = 0, sd = 0.035)
  })
  colnames(donor_matrix) <- donor_names
  rownames(donor_matrix) <- years

  donor_panel <- as.data.frame(donor_matrix) %>%
    mutate(year = years) %>%
    tidyr::pivot_longer(
      cols = -year,
      names_to = "country",
      values_to = "youth_cycling_registrations"
    ) %>%
    arrange(country, year)

  list(
    matrix = donor_matrix,
    panel = donor_panel,
    donor_names = donor_names,
    years = years
  )
}

scm_create_treated_paths <- function(
    donor_matrix,
    years,
    seed = 123L,
    treatment_year = 2019L) {
  time_index <- seq_along(years) - 1L
  ideal_population_weights <- c(
    Croatia = 0.35,
    Estonia = 0.20,
    Latvia = 0.00,
    Lithuania = 0.00,
    Slovakia = 0.25,
    Finland = 0.20,
    Ireland = 0.00
  )

  set.seed(seed + 920L)
  ideal_untreated_path <-
    as.vector(donor_matrix %*% ideal_population_weights) +
    rnorm(length(years), mean = 0, sd = 0.025)

  # The failure path has an accelerating component that is not present in any
  # donor. By the end of the pre-treatment period, it is above every donor.
  failure_untreated_path <-
    ideal_untreated_path + 0.025 * time_index^2

  post_effect <- c(0.50, 1.50, 2.50, 3.20, 3.80, 4.20)
  treatment_effect <- rep(0, length(years))
  treatment_effect[years >= treatment_year] <- post_effect

  list(
    ideal_untreated_path = ideal_untreated_path,
    failure_untreated_path = failure_untreated_path,
    treatment_effect = treatment_effect,
    ideal_population_weights = ideal_population_weights
  )
}

scm_solve_weights <- function(treated_pre, donor_pre) {
  donor_count <- ncol(donor_pre)
  ridge <- 1e-8

  quadratic_matrix <-
    2 * crossprod(donor_pre) + diag(ridge, donor_count)
  linear_vector <- 2 * crossprod(donor_pre, treated_pre)

  # solve.QP uses constraints t(Amat) %*% w >= bvec. The first constraint is
  # an equality requiring weights to sum to one; the others impose w_j >= 0.
  constraint_matrix <- cbind(rep(1, donor_count), diag(donor_count))
  constraint_vector <- c(1, rep(0, donor_count))

  solution <- quadprog::solve.QP(
    Dmat = quadratic_matrix,
    dvec = linear_vector,
    Amat = constraint_matrix,
    bvec = constraint_vector,
    meq = 1L
  )

  weights <- pmax(solution$solution, 0)
  weights / sum(weights)
}

scm_estimate_scenario <- function(
    scenario,
    untreated_path,
    treatment_effect,
    donor_matrix,
    donor_names,
    years,
    treatment_year = 2019L) {
  pre_period <- years < treatment_year
  post_period <- years >= treatment_year
  observed_path <- untreated_path + treatment_effect

  weights <- scm_solve_weights(
    treated_pre = observed_path[pre_period],
    donor_pre = donor_matrix[pre_period, , drop = FALSE]
  )
  names(weights) <- donor_names
  synthetic_path <- as.vector(donor_matrix %*% weights)
  estimated_gap <- observed_path - synthetic_path

  pre_rmspe <- sqrt(mean(
    (observed_path[pre_period] - synthetic_path[pre_period])^2
  ))
  average_estimated_effect <- mean(estimated_gap[post_period])
  average_true_effect <- mean(treatment_effect[post_period])

  results <- data.frame(
    scenario = scenario,
    pre_treatment_rmspe = pre_rmspe,
    average_estimated_effect = average_estimated_effect,
    average_true_effect = average_true_effect,
    estimation_error = average_estimated_effect - average_true_effect,
    treated_last_pre = observed_path[max(which(pre_period))],
    donor_min_last_pre = min(donor_matrix[max(which(pre_period)), ]),
    donor_max_last_pre = max(donor_matrix[max(which(pre_period)), ])
  )

  paths <- data.frame(
    scenario = scenario,
    year = years,
    observed_slovenia = observed_path,
    simulated_no_star_slovenia = untreated_path,
    synthetic_slovenia = synthetic_path,
    true_effect = treatment_effect,
    estimated_gap = estimated_gap
  )

  weight_results <- data.frame(
    scenario = scenario,
    donor = donor_names,
    weight = weights
  )

  list(
    results = results,
    paths = paths,
    weights = weight_results
  )
}

scm_build_donor_envelope <- function(donor_matrix, years) {
  data.frame(
    year = years,
    donor_min = apply(donor_matrix, 1, min),
    donor_max = apply(donor_matrix, 1, max)
  )
}

scm_make_path_panel <- function(
    scenario_paths,
    donor_envelope,
    result,
    panel_label,
    treatment_year = 2019L) {
  plot_data <- scenario_paths %>%
    select(year, observed_slovenia, synthetic_slovenia) %>%
    tidyr::pivot_longer(
      cols = c(observed_slovenia, synthetic_slovenia),
      names_to = "series",
      values_to = "registrations"
    ) %>%
    mutate(
      series = recode(
        series,
        observed_slovenia = "Observed Slovenia",
        synthetic_slovenia = "Synthetic Slovenia"
      )
    )

  subtitle <- sprintf(
    "Pre-treatment RMSPE = %.3f",
    result$pre_treatment_rmspe
  )

  ggplot() +
    geom_ribbon(
      data = donor_envelope,
      aes(x = year, ymin = donor_min, ymax = donor_max),
      fill = "#D9D9D9",
      alpha = 0.55
    ) +
    geom_vline(
      xintercept = treatment_year,
      linetype = "dashed",
      linewidth = 0.7,
      color = "#555555"
    ) +
    geom_line(
      data = plot_data,
      aes(x = year, y = registrations, color = series, linetype = series),
      linewidth = 1.05
    ) +
    scale_color_manual(
      values = c(
        "Observed Slovenia" = "#D4513F",
        "Synthetic Slovenia" = "#275D8C"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Observed Slovenia" = "solid",
        "Synthetic Slovenia" = "longdash"
      )
    ) +
    scale_x_continuous(breaks = seq(2008, 2024, by = 4)) +
    coord_cartesian(ylim = c(4, 20)) +
    labs(
      title = paste0(panel_label, ". ", unique(scenario_paths$scenario)),
      subtitle = subtitle,
      x = NULL,
      y = "Cycling registrations per 1,000 children",
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

scm_make_figure <- function(
    paths,
    weights,
    results,
    donor_envelope,
    treatment_year = 2019L) {
  ideal_paths <- paths %>% filter(scenario == "Ideal: convex-hull support")
  failure_paths <- paths %>%
    filter(scenario == "Failure: no convex-hull support")
  ideal_result <- results %>% filter(scenario == "Ideal: convex-hull support")
  failure_result <- results %>%
    filter(scenario == "Failure: no convex-hull support")

  ideal_panel <- scm_make_path_panel(
    ideal_paths,
    donor_envelope,
    ideal_result,
    panel_label = "A",
    treatment_year = treatment_year
  )
  failure_panel <- scm_make_path_panel(
    failure_paths,
    donor_envelope,
    failure_result,
    panel_label = "B",
    treatment_year = treatment_year
  )

  weight_panel <- weights %>%
    mutate(
      scenario = recode(
        scenario,
        "Ideal: convex-hull support" = "Ideal",
        "Failure: no convex-hull support" = "Failure"
      ),
      scenario = factor(scenario, levels = c("Ideal", "Failure"))
    ) %>%
    ggplot(aes(x = donor, y = weight, fill = scenario)) +
    geom_col(width = 0.68, show.legend = FALSE) +
    facet_wrap(~scenario, ncol = 1) +
    scale_fill_manual(values = c("Ideal" = "#4C8BCB", "Failure" = "#D9794A")) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    labs(
      title = "C. Estimated donor weights",
      x = NULL,
      y = "Weight"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 35, hjust = 1)
    )

  estimated_gaps <- paths %>%
    transmute(
      year,
      effect = estimated_gap,
      series = if_else(
        scenario == "Ideal: convex-hull support",
        "Ideal SCM gap",
        "Failure SCM gap"
      )
    )
  true_effect_path <- paths %>%
    filter(scenario == "Ideal: convex-hull support") %>%
    transmute(
      year,
      effect = true_effect,
      series = "True star effect"
    )
  gap_data <- bind_rows(estimated_gaps, true_effect_path)

  gap_panel <- ggplot(
    gap_data,
    aes(x = year, y = effect, color = series, linetype = series)
  ) +
    geom_hline(yintercept = 0, linewidth = 0.55, color = "#777777") +
    geom_vline(
      xintercept = treatment_year,
      linetype = "dotted",
      linewidth = 0.7,
      color = "#555555"
    ) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(
      values = c(
        "Ideal SCM gap" = "#4C8BCB",
        "Failure SCM gap" = "#D9794A",
        "True star effect" = "#238B45"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Ideal SCM gap" = "solid",
        "Failure SCM gap" = "solid",
        "True star effect" = "longdash"
      )
    ) +
    scale_x_continuous(breaks = seq(2008, 2024, by = 4)) +
    labs(
      title = "D. Estimated gaps and true causal effects",
      x = NULL,
      y = "Effect on registrations",
      color = NULL,
      linetype = NULL
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  (ideal_panel + failure_panel) /
    (weight_panel + gap_panel) +
    plot_layout(heights = c(1, 0.95))
}

run_scm_simulation <- function(
    seed = 123L,
    treatment_year = 2019L,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images") {
  donor_data <- scm_simulate_donor_pool(seed = seed)
  treated_paths <- scm_create_treated_paths(
    donor_matrix = donor_data$matrix,
    years = donor_data$years,
    seed = seed,
    treatment_year = treatment_year
  )

  ideal <- scm_estimate_scenario(
    scenario = "Ideal: convex-hull support",
    untreated_path = treated_paths$ideal_untreated_path,
    treatment_effect = treated_paths$treatment_effect,
    donor_matrix = donor_data$matrix,
    donor_names = donor_data$donor_names,
    years = donor_data$years,
    treatment_year = treatment_year
  )
  failure <- scm_estimate_scenario(
    scenario = "Failure: no convex-hull support",
    untreated_path = treated_paths$failure_untreated_path,
    treatment_effect = treated_paths$treatment_effect,
    donor_matrix = donor_data$matrix,
    donor_names = donor_data$donor_names,
    years = donor_data$years,
    treatment_year = treatment_year
  )

  results <- bind_rows(ideal$results, failure$results)
  paths <- bind_rows(ideal$paths, failure$paths)
  weights <- bind_rows(ideal$weights, failure$weights)
  donor_envelope <- scm_build_donor_envelope(
    donor_data$matrix,
    donor_data$years
  )
  scm_figure <- scm_make_figure(
    paths,
    weights,
    results,
    donor_envelope,
    treatment_year = treatment_year
  )

  ideal_dataset <- bind_rows(
    donor_data$panel %>% mutate(unit_type = "Donor"),
    ideal$paths %>%
      transmute(
        year,
        country = "Slovenia",
        youth_cycling_registrations = observed_slovenia,
        unit_type = "Treated"
      )
  )
  failure_dataset <- bind_rows(
    donor_data$panel %>% mutate(unit_type = "Donor"),
    failure$paths %>%
      transmute(
        year,
        country = "Slovenia",
        youth_cycling_registrations = observed_slovenia,
        unit_type = "Treated"
      )
  )

  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      ideal_dataset,
      file.path(data_directory, "scm_youth_cycling_ideal.csv"),
      row.names = FALSE
    )
    write.csv(
      failure_dataset,
      file.path(data_directory, "scm_youth_cycling_failure.csv"),
      row.names = FALSE
    )
    write.csv(
      results,
      file.path(data_directory, "scm_youth_cycling_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      weights,
      file.path(data_directory, "scm_youth_cycling_weights.csv"),
      row.names = FALSE
    )
    write.csv(
      paths,
      file.path(data_directory, "scm_youth_cycling_paths.csv"),
      row.names = FALSE
    )
    ggsave(
      file.path(image_directory, "scm-youth-cycling-simulation.png"),
      scm_figure,
      width = 12,
      height = 9.4,
      dpi = 320,
      bg = "white"
    )
  }

  list(
    ideal_data = ideal_dataset,
    failure_data = failure_dataset,
    estimates = results,
    weights = weights,
    paths = paths,
    donor_envelope = donor_envelope,
    figure = scm_figure
  )
}

if (sys.nframe() == 0L) {
  scm_results <- run_scm_simulation(seed = 123L)
  cat("\nSynthetic-control estimates\n")
  print(scm_results$estimates, row.names = FALSE, digits = 3)
  cat("\nSynthetic-control weights\n")
  print(scm_results$weights, row.names = FALSE, digits = 3)
}
