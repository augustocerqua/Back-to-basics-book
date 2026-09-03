# Section 16.2: Selection on observables
#
# This self-contained script adapts the Project STAR setting to an
# observational study. It compares OLS and propensity-score matching when
# all confounders are observed and when family affluence is unobserved.
# All random draws are generated from stable streams based on master seed 123.

required_packages <- c("dplyr", "ggplot2", "patchwork", "Matching")
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

soo_standardized_mean_difference <- function(x, group) {
  mean_difference <- mean(x[group == 1]) - mean(x[group == 0])
  pooled_sd <- sqrt((var(x[group == 1]) + var(x[group == 0])) / 2)
  mean_difference / pooled_sd
}

soo_extract_lm_estimate <- function(
    model,
    term,
    method,
    true_value) {
  coefficient_table <- summary(model)$coefficients
  confidence_interval <- confint(model, term, level = 0.95)

  data.frame(
    method = method,
    estimand = "ATT",
    estimate = unname(coefficient_table[term, "Estimate"]),
    std_error = unname(coefficient_table[term, "Std. Error"]),
    conf_low = unname(confidence_interval[1]),
    conf_high = unname(confidence_interval[2]),
    true_value = true_value,
    row.names = NULL
  )
}

soo_extract_matching_estimate <- function(
    matching_result,
    method,
    true_value,
    estimand = "ATT") {
  estimate <- unname(matching_result$est)
  standard_error <- unname(matching_result$se)

  data.frame(
    method = method,
    estimand = estimand,
    estimate = estimate,
    std_error = standard_error,
    conf_low = estimate - qnorm(0.975) * standard_error,
    conf_high = estimate + qnorm(0.975) * standard_error,
    true_value = true_value,
    row.names = NULL
  )
}

soo_create_no_common_support_data <- function(population, seed = 123L) {
  high_family_background <-
    population$simulated_parental_affluence > 0.8 &
    population$parental_education_years > 14

  treatment_probability_in_overlap <- plogis(
    -0.35 +
    0.55 * population$simulated_parental_affluence +
    0.16 * (population$parental_education_years - 12)
  )

  set.seed(seed + 400L)
  received_small_class <- ifelse(
    high_family_background,
    1L,
    rbinom(
      nrow(population),
      size = 1,
      prob = treatment_probability_in_overlap
    )
  )

  untreated_score <-
    population$simulated_y0 +
    12 * high_family_background +
    2 * pmax(population$simulated_parental_affluence - 0.8, 0)^2

  population %>%
    mutate(
      simulated_high_family_background = high_family_background,
      simulated_treatment_probability = if_else(
        high_family_background,
        1,
        treatment_probability_in_overlap
      ),
      received_small_class = received_small_class,
      simulated_y0_no_common_support = untreated_score,
      end_school_year_score = untreated_score +
        received_small_class * simulated_individual_effect
    )
}

soo_estimate_no_common_support <- function(
    data,
    true_att,
    propensity_score_caliper = 0.05) {
  ols_model <- lm(
    end_school_year_score ~ received_small_class +
      parental_education_years + simulated_parental_affluence,
    data = data
  )
  propensity_model <- glm(
    received_small_class ~ parental_education_years +
      simulated_parental_affluence + simulated_high_family_background,
    family = binomial(),
    data = data
  )
  estimated_propensity_score <- fitted(propensity_model)
  standardized_caliper <- propensity_score_caliper /
    sd(estimated_propensity_score)
  caliper_match <- Matching::Match(
    Y = data$end_school_year_score,
    Tr = data$received_small_class,
    X = estimated_propensity_score,
    estimand = "ATT",
    M = 1,
    replace = TRUE,
    caliper = standardized_caliper
  )

  estimates <- bind_rows(
    soo_extract_lm_estimate(
      ols_model,
      "received_small_class",
      "Linear OLS on the full sample",
      true_att
    ),
    soo_extract_matching_estimate(
      caliper_match,
      "PSM with a 0.05 propensity-score caliper",
      true_att,
      estimand = "ATT in common support"
    )
  )
  estimates$analysis_sample <- c(
    "All treated pupils",
    "Matched treated pupils"
  )

  list(
    estimates = estimates,
    ols_model = ols_model,
    propensity_model = propensity_model,
    caliper_match = caliper_match,
    total_treated = sum(data$received_small_class == 1L),
    matched_treated = length(unique(caliper_match$index.treated)),
    excluded_treated = sum(data$received_small_class == 1L) -
      length(unique(caliper_match$index.treated))
  )
}

soo_simulate_population <- function(
    n = 5000L,
    n_schools = 20L,
    seed = 123L) {
  if (n %% n_schools != 0L) {
    stop("n must be divisible by n_schools in this simulation.")
  }

  pupils_per_school <- n / n_schools
  school_id <- rep(seq_len(n_schools), each = pupils_per_school)

  set.seed(seed + 10L)
  school_effects <- rnorm(n_schools, mean = 0, sd = 2.5)
  set.seed(seed + 20L)
  female <- rbinom(n, size = 1, prob = 0.50)
  set.seed(seed + 30L)
  disadvantaged <- rbinom(n, size = 1, prob = 0.35)
  set.seed(seed + 40L)
  latent_motivation <- rnorm(n)
  set.seed(seed + 50L)
  parental_affluence <- rnorm(n)
  set.seed(seed + 60L)
  parental_education <- pmin(
    20,
    pmax(
      6,
      12 + 1.2 * parental_affluence + rnorm(n, mean = 0, sd = 2.2)
    )
  )

  set.seed(seed + 70L)
  baseline_score <-
    50 +
    3.0 * latent_motivation -
    4.0 * disadvantaged +
    1.4 * parental_affluence +
    0.55 * (parental_education - 12) +
    school_effects[school_id] +
    rnorm(n, mean = 0, sd = 7)

  set.seed(seed + 80L)
  untreated_score <-
    20 +
    0.55 * baseline_score -
    2.5 * disadvantaged +
    1.0 * female +
    2.0 * latent_motivation +
    2.8 * parental_affluence +
    0.75 * (parental_education - 12) +
    school_effects[school_id] +
    rnorm(n, mean = 0, sd = 4)

  individual_effect <- rep(4, n)

  data.frame(
    pupil_id = seq_len(n),
    school_id = school_id,
    female = female,
    disadvantaged = disadvantaged,
    baseline_score = baseline_score,
    parental_education_years = parental_education,
    simulated_parental_affluence = parental_affluence,
    simulated_latent_motivation = latent_motivation,
    simulated_y0 = untreated_score,
    simulated_y1 = untreated_score + individual_effect,
    simulated_individual_effect = individual_effect
  )
}

soo_create_observational_data <- function(population, seed = 123L) {
  treatment_probability <- plogis(
    -0.25 +
    0.80 * population$simulated_parental_affluence +
    0.22 * (population$parental_education_years - 12)
  )

  set.seed(seed + 300L)
  received_small_class <- rbinom(
    nrow(population),
    size = 1,
    prob = treatment_probability
  )

  population %>%
    mutate(
      simulated_treatment_probability = treatment_probability,
      received_small_class = received_small_class,
      end_school_year_score = simulated_y0 +
        received_small_class * simulated_individual_effect
    )
}

soo_estimate_effects <- function(data, true_att) {
  naive_model <- lm(
    end_school_year_score ~ received_small_class,
    data = data
  )
  complete_ols_model <- lm(
    end_school_year_score ~ received_small_class +
      parental_education_years + simulated_parental_affluence,
    data = data
  )
  incomplete_ols_model <- lm(
    end_school_year_score ~ received_small_class +
      parental_education_years,
    data = data
  )
  complete_propensity_model <- glm(
    received_small_class ~ parental_education_years +
      simulated_parental_affluence,
    family = binomial(),
    data = data
  )
  incomplete_propensity_model <- glm(
    received_small_class ~ parental_education_years,
    family = binomial(),
    data = data
  )

  complete_match <- Matching::Match(
    Y = data$end_school_year_score,
    Tr = data$received_small_class,
    X = fitted(complete_propensity_model),
    estimand = "ATT",
    M = 1,
    replace = TRUE
  )
  incomplete_match <- Matching::Match(
    Y = data$end_school_year_score,
    Tr = data$received_small_class,
    X = fitted(incomplete_propensity_model),
    estimand = "ATT",
    M = 1,
    replace = TRUE
  )

  estimates <- bind_rows(
    soo_extract_lm_estimate(
      naive_model,
      "received_small_class",
      "Unadjusted comparison",
      true_att
    ),
    soo_extract_lm_estimate(
      complete_ols_model,
      "received_small_class",
      "OLS: both confounders observed",
      true_att
    ),
    soo_extract_matching_estimate(
      complete_match,
      "Matching: both confounders observed",
      true_att
    ),
    soo_extract_lm_estimate(
      incomplete_ols_model,
      "received_small_class",
      "OLS: family affluence unobserved",
      true_att
    ),
    soo_extract_matching_estimate(
      incomplete_match,
      "Matching: family affluence unobserved",
      true_att
    )
  )

  list(
    estimates = estimates,
    complete_match = complete_match,
    incomplete_match = incomplete_match
  )
}

soo_make_figure <- function(data, results) {
  matched_smd <- function(variable, match_index) {
    treated_values <- data[[variable]][match_index$index.treated]
    control_values <- data[[variable]][match_index$index.control]
    weights <- match_index$weights
    treated_mean <- weighted.mean(treated_values, weights)
    control_mean <- weighted.mean(control_values, weights)
    treated_variance <- weighted.mean(
      (treated_values - treated_mean)^2,
      weights
    )
    control_variance <- weighted.mean(
      (control_values - control_mean)^2,
      weights
    )
    mean_difference <- treated_mean - control_mean
    pooled_sd <- sqrt((treated_variance + control_variance) / 2)
    abs(mean_difference / pooled_sd)
  }

  balance_data <- data.frame(
    adjustment = rep(
      c(
        "Before adjustment",
        "Matching: both observed",
        "Matching: affluence unobserved"
      ),
      each = 2
    ),
    variable = rep(c("Parental education", "Family affluence"), times = 3),
    absolute_smd = c(
      abs(soo_standardized_mean_difference(
        data$parental_education_years,
        data$received_small_class
      )),
      abs(soo_standardized_mean_difference(
        data$simulated_parental_affluence,
        data$received_small_class
      )),
      matched_smd("parental_education_years", results$complete_match),
      matched_smd("simulated_parental_affluence", results$complete_match),
      matched_smd("parental_education_years", results$incomplete_match),
      matched_smd("simulated_parental_affluence", results$incomplete_match)
    )
  )

  balance_plot <- ggplot(
    balance_data,
    aes(x = variable, y = absolute_smd, fill = adjustment)
  ) +
    geom_hline(
      yintercept = 0.10,
      linetype = "dashed",
      linewidth = 0.6,
      color = "#666666"
    ) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    scale_fill_manual(values = c("#D98256", "#4C9F70", "#7CA6D8")) +
    labs(
      title = "A. Balance depends on what is observed",
      x = NULL,
      y = "Absolute standardized difference",
      fill = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(legend.position = "bottom")

  estimate_levels <- results$estimates$method
  estimates_plot <- results$estimates %>%
    mutate(method = factor(method, levels = rev(estimate_levels))) %>%
    ggplot(aes(x = estimate, y = method)) +
    geom_vline(
      xintercept = unique(results$estimates$true_value),
      linetype = "dashed",
      linewidth = 0.8,
      color = "#1B7837"
    ) +
    geom_segment(
      aes(x = true_value, xend = estimate, yend = method),
      linewidth = 0.8,
      color = "#9A9A9A"
    ) +
    geom_point(size = 3.2, color = "#1F4E79") +
    labs(
      title = "B. Estimated effects",
      x = "Test-score points",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5)

  balance_plot + estimates_plot + plot_layout(widths = c(1.08, 1))
}

run_selection_on_observables_simulation <- function(
    seed = 123L,
    sample_size = 5000L,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images") {
  population <- soo_simulate_population(
    n = sample_size,
    n_schools = 20L,
    seed = seed
  )
  observational_data <- soo_create_observational_data(
    population,
    seed = seed
  )
  true_att <- mean(
    observational_data$simulated_individual_effect[
      observational_data$received_small_class == 1L
    ]
  )
  results <- soo_estimate_effects(observational_data, true_att)
  observational_figure <- soo_make_figure(observational_data, results)

  no_common_support_data <- soo_create_no_common_support_data(
    population,
    seed = seed
  )
  true_no_common_support_att <- mean(
    no_common_support_data$simulated_individual_effect[
      no_common_support_data$received_small_class == 1L
    ]
  )
  no_common_support_results <- soo_estimate_no_common_support(
    no_common_support_data,
    true_att = true_no_common_support_att,
    propensity_score_caliper = 0.05
  )

  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      observational_data,
      file.path(data_directory, "star_observational.csv"),
      row.names = FALSE
    )
    write.csv(
      results$estimates,
      file.path(data_directory, "star_observational_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      no_common_support_data,
      file.path(data_directory, "star_observational_no_common_support.csv"),
      row.names = FALSE
    )
    write.csv(
      no_common_support_results$estimates,
      file.path(
        data_directory,
        "star_observational_no_common_support_estimates.csv"
      ),
      row.names = FALSE
    )
    ggsave(
      file.path(image_directory, "star-observational-ols-matching.png"),
      observational_figure,
      width = 12,
      height = 5.8,
      dpi = 320,
      bg = "white"
    )
  }

  list(
    observational_data = observational_data,
    observational_estimates = results$estimates,
    true_att = true_att,
    complete_match = results$complete_match,
    incomplete_match = results$incomplete_match,
    observational_figure = observational_figure,
    no_common_support_data = no_common_support_data,
    no_common_support_estimates = no_common_support_results$estimates,
    no_common_support_total_treated = no_common_support_results$total_treated,
    no_common_support_matched_treated = no_common_support_results$matched_treated,
    no_common_support_excluded_treated = no_common_support_results$excluded_treated
  )
}

if (sys.nframe() == 0L) {
  selection_results <- run_selection_on_observables_simulation(seed = 123L)
  cat("\nSelection-on-observables estimates\n")
  print(selection_results$observational_estimates, row.names = FALSE, digits = 3)
  cat("\nLack-of-common-support estimates\n")
  print(
    selection_results$no_common_support_estimates,
    row.names = FALSE,
    digits = 3
  )
}
