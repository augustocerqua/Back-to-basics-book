# Section 16.1: Randomized controlled trial
#
# This self-contained script simulates a stylized Project STAR experiment.
# It covers perfect compliance and selective one-sided non-compliance.
# All random draws are generated from stable streams based on master seed 123.

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

rct_standardized_mean_difference <- function(x, group) {
  mean_difference <- mean(x[group == 1]) - mean(x[group == 0])
  pooled_sd <- sqrt((var(x[group == 1]) + var(x[group == 0])) / 2)
  mean_difference / pooled_sd
}

rct_extract_lm_estimate <- function(
    model,
    term,
    method,
    estimand,
    true_value) {
  coefficient_table <- summary(model)$coefficients
  confidence_interval <- confint(model, term, level = 0.95)

  data.frame(
    method = method,
    estimand = estimand,
    estimate = unname(coefficient_table[term, "Estimate"]),
    std_error = unname(coefficient_table[term, "Std. Error"]),
    conf_low = unname(confidence_interval[1]),
    conf_high = unname(confidence_interval[2]),
    true_value = true_value,
    row.names = NULL
  )
}

rct_randomize_within_school <- function(school_id, seed = 123L) {
  set.seed(seed + 100L)
  assignment <- integer(length(school_id))

  for (school in unique(school_id)) {
    pupils <- which(school_id == school)
    n_treated <- floor(length(pupils) / 2)
    assignment[pupils] <- sample(
      c(rep(1L, n_treated), rep(0L, length(pupils) - n_treated))
    )
  }

  assignment
}

rct_simulate_population <- function(
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

rct_create_ideal_data <- function(population, seed = 123L) {
  assignment <- rct_randomize_within_school(
    population$school_id,
    seed = seed
  )

  population %>%
    mutate(
      assigned_small_class = assignment,
      received_small_class = assignment,
      end_school_year_score = if_else(
        received_small_class == 1L,
        simulated_y1,
        simulated_y0
      )
    )
}

rct_create_noncompliance_data <- function(
    population,
    assignment,
    seed = 123L) {
  compliance_probability <- plogis(
    0.70 +
    0.035 * (population$baseline_score - 50) -
    0.55 * population$disadvantaged +
    1.10 * population$simulated_latent_motivation
  )

  set.seed(seed + 200L)
  would_comply_if_offered <- rbinom(
    nrow(population),
    size = 1,
    prob = compliance_probability
  )
  received_small_class <- assignment * would_comply_if_offered

  population %>%
    mutate(
      assigned_small_class = assignment,
      simulated_compliance_probability = compliance_probability,
      simulated_would_comply_if_offered = would_comply_if_offered,
      received_small_class = received_small_class,
      end_school_year_score = simulated_y0 +
        received_small_class * simulated_individual_effect
    )
}

rct_wald_estimate <- function(outcome, treatment, instrument) {
  y <- as.matrix(outcome)
  x <- cbind(intercept = 1, treatment = treatment)
  z <- cbind(intercept = 1, instrument = instrument)

  z_crossprod_inverse <- solve(crossprod(z))
  x_pz_x <- crossprod(x, z) %*% z_crossprod_inverse %*% crossprod(z, x)
  x_pz_y <- crossprod(x, z) %*% z_crossprod_inverse %*% crossprod(z, y)
  coefficients <- solve(x_pz_x, x_pz_y)

  residuals <- y - x %*% coefficients
  residual_variance <- sum(residuals^2) / (nrow(x) - ncol(x))
  variance_matrix <- residual_variance * solve(x_pz_x)
  standard_error <- sqrt(variance_matrix["treatment", "treatment"])
  estimate <- unname(coefficients["treatment", 1])

  data.frame(
    estimate = estimate,
    std_error = standard_error,
    conf_low = estimate - qnorm(0.975) * standard_error,
    conf_high = estimate + qnorm(0.975) * standard_error
  )
}

rct_make_ideal_figure <- function(data, estimates, true_ate) {
  plot_data <- data %>%
    mutate(
      assignment = factor(
        assigned_small_class,
        levels = c(0, 1),
        labels = c("Regular class", "Small class")
      )
    )

  balance_plot <- ggplot(
    plot_data,
    aes(x = assignment, y = baseline_score, fill = assignment)
  ) +
    geom_violin(alpha = 0.35, width = 0.85, color = NA) +
    geom_boxplot(width = 0.18, outlier.alpha = 0.15) +
    scale_fill_manual(values = c("#5DADE2", "#F07B52")) +
    labs(
      title = "A. Baseline balance after randomization",
      x = NULL,
      y = "Baseline test score"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")

  estimates_plot <- estimates %>%
    mutate(method = factor(method, levels = rev(unique(method)))) %>%
    ggplot(aes(x = estimate, y = method)) +
    geom_vline(
      xintercept = true_ate,
      linetype = "dashed",
      linewidth = 0.8,
      color = "#1B7837"
    ) +
    geom_errorbarh(
      aes(xmin = conf_low, xmax = conf_high),
      height = 0.15,
      linewidth = 0.8,
      color = "#1F4E79"
    ) +
    geom_point(size = 3.2, color = "#1F4E79") +
    labs(
      title = "B. Estimated effect of assignment",
      x = "Test-score points",
      y = NULL
    ) +
    theme_minimal(base_size = 13)

  balance_plot + estimates_plot + plot_layout(widths = c(1, 1.08))
}

rct_make_noncompliance_figure <- function(data, estimates) {
  balance_data <- data.frame(
    comparison = rep(c("Random assignment", "Treatment received"), each = 2),
    variable = rep(c("Baseline score", "Latent motivation"), times = 2),
    absolute_smd = abs(c(
      rct_standardized_mean_difference(
        data$baseline_score,
        data$assigned_small_class
      ),
      rct_standardized_mean_difference(
        data$simulated_latent_motivation,
        data$assigned_small_class
      ),
      rct_standardized_mean_difference(
        data$baseline_score,
        data$received_small_class
      ),
      rct_standardized_mean_difference(
        data$simulated_latent_motivation,
        data$received_small_class
      )
    ))
  )

  balance_plot <- ggplot(
    balance_data,
    aes(x = variable, y = absolute_smd, fill = comparison)
  ) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62) +
    scale_fill_manual(values = c("#5DADE2", "#F07B52")) +
    labs(
      title = "A. Random assignment is balanced; receipt is not",
      x = NULL,
      y = "Absolute standardized difference",
      fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")

  estimates_plot <- estimates %>%
    mutate(method = factor(method, levels = rev(method))) %>%
    ggplot(aes(x = method)) +
    geom_segment(
      aes(xend = method, y = true_value, yend = estimate),
      linewidth = 0.9,
      color = "#8C8C8C"
    ) +
    geom_point(
      aes(y = true_value, shape = "True target"),
      size = 3.6,
      color = "#1B7837"
    ) +
    geom_point(
      aes(y = estimate, shape = "Estimate"),
      size = 3.4,
      color = "#1F4E79"
    ) +
    coord_flip() +
    scale_shape_manual(values = c("Estimate" = 16, "True target" = 4)) +
    labs(
      title = "B. Estimates and their causal targets",
      x = NULL,
      y = "Test-score points",
      shape = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")

  balance_plot + estimates_plot + plot_layout(widths = c(1.08, 1))
}

run_rct_simulation <- function(
    seed = 123L,
    sample_size = 5000L,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images") {
  population <- rct_simulate_population(
    n = sample_size,
    n_schools = 20L,
    seed = seed
  )
  ideal_data <- rct_create_ideal_data(population, seed = seed)
  true_ate <- mean(ideal_data$simulated_individual_effect)

  ideal_unadjusted_model <- lm(
    end_school_year_score ~ assigned_small_class,
    data = ideal_data
  )
  ideal_blocked_model <- lm(
    end_school_year_score ~ assigned_small_class + factor(school_id),
    data = ideal_data
  )
  ideal_estimates <- bind_rows(
    rct_extract_lm_estimate(
      ideal_unadjusted_model,
      "assigned_small_class",
      "Difference in means",
      "ATE",
      true_ate
    ),
    rct_extract_lm_estimate(
      ideal_blocked_model,
      "assigned_small_class",
      "Regression with school blocks",
      "ATE",
      true_ate
    )
  )

  noncompliance_data <- rct_create_noncompliance_data(
    population,
    assignment = ideal_data$assigned_small_class,
    seed = seed
  )
  true_itt <- mean(
    noncompliance_data$simulated_would_comply_if_offered *
      noncompliance_data$simulated_individual_effect
  )
  true_late <- mean(
    noncompliance_data$simulated_individual_effect[
      noncompliance_data$simulated_would_comply_if_offered == 1L
    ]
  )

  itt_model <- lm(
    end_school_year_score ~ assigned_small_class,
    data = noncompliance_data
  )
  as_treated_model <- lm(
    end_school_year_score ~ received_small_class,
    data = noncompliance_data
  )
  as_treated_adjusted_model <- lm(
    end_school_year_score ~ received_small_class + baseline_score +
      disadvantaged + female + factor(school_id),
    data = noncompliance_data
  )
  first_stage_model <- lm(
    received_small_class ~ assigned_small_class,
    data = noncompliance_data
  )
  wald_result <- rct_wald_estimate(
    outcome = noncompliance_data$end_school_year_score,
    treatment = noncompliance_data$received_small_class,
    instrument = noncompliance_data$assigned_small_class
  )

  noncompliance_estimates <- bind_rows(
    rct_extract_lm_estimate(
      itt_model,
      "assigned_small_class",
      "ITT: compare assignment groups",
      "Effect of assignment (ITT)",
      true_itt
    ),
    rct_extract_lm_estimate(
      as_treated_model,
      "received_small_class",
      "Naive as-treated comparison",
      "Effect of treatment receipt",
      true_ate
    ),
    rct_extract_lm_estimate(
      as_treated_adjusted_model,
      "received_small_class",
      "Adjusted as-treated regression",
      "Effect of treatment receipt",
      true_ate
    ),
    data.frame(
      method = "Wald ratio (preview of IV)",
      estimand = "LATE for compliers",
      estimate = wald_result$estimate,
      std_error = wald_result$std_error,
      conf_low = wald_result$conf_low,
      conf_high = wald_result$conf_high,
      true_value = true_late
    )
  )

  first_stage_estimate <- unname(
    coef(first_stage_model)["assigned_small_class"]
  )
  ideal_figure <- rct_make_ideal_figure(
    ideal_data,
    ideal_estimates,
    true_ate
  )
  noncompliance_figure <- rct_make_noncompliance_figure(
    noncompliance_data,
    noncompliance_estimates
  )

  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      ideal_data,
      file.path(data_directory, "rct_star_ideal.csv"),
      row.names = FALSE
    )
    write.csv(
      noncompliance_data,
      file.path(data_directory, "rct_star_noncompliance.csv"),
      row.names = FALSE
    )
    write.csv(
      ideal_estimates,
      file.path(data_directory, "rct_star_ideal_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      noncompliance_estimates,
      file.path(data_directory, "rct_star_noncompliance_estimates.csv"),
      row.names = FALSE
    )
    ggsave(
      file.path(image_directory, "rct-star-simulation-ideal.png"),
      ideal_figure,
      width = 12,
      height = 5.4,
      dpi = 320,
      bg = "white"
    )
    ggsave(
      file.path(image_directory, "rct-star-simulation-noncompliance.png"),
      noncompliance_figure,
      width = 12,
      height = 5.4,
      dpi = 320,
      bg = "white"
    )
  }

  list(
    ideal_data = ideal_data,
    ideal_estimates = ideal_estimates,
    noncompliance_data = noncompliance_data,
    noncompliance_estimates = noncompliance_estimates,
    true_ate = true_ate,
    true_itt = true_itt,
    true_late = true_late,
    first_stage = first_stage_estimate,
    ideal_figure = ideal_figure,
    noncompliance_figure = noncompliance_figure
  )
}

if (sys.nframe() == 0L) {
  rct_results <- run_rct_simulation(seed = 123L)
  cat("\nIdeal RCT estimates\n")
  print(rct_results$ideal_estimates, row.names = FALSE, digits = 3)
  cat("\nPartial-compliance estimates\n")
  print(rct_results$noncompliance_estimates, row.names = FALSE, digits = 3)
  cat(
    "\nFirst-stage effect of assignment on treatment receipt:",
    round(rct_results$first_stage, 3),
    "\n"
  )
}
