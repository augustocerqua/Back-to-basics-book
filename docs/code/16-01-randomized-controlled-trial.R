# Section 16.1: Randomized Controlled Trial
# ============================================================
#
# This script simulates a stylized version of Project STAR.
#
# Goal:
#   Show why random assignment identifies causal effects in an ideal RCT,
#   and why actual treatment receipt may no longer be causal when there is
#   selective non-compliance.
#
# The script creates:
#   1. an ideal RCT with perfect compliance;
#   2. an RCT with one-sided selective non-compliance;
#   3. tables of estimates saved as CSV files;
#   4. figures saved as high-resolution PNG files.
#
# Reproducibility:
#   All random draws use deterministic streams based on master seed 123.
#   This makes the numerical results stable across runs.


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

# The standardized mean difference compares the mean of a variable across
# two groups, measured in pooled-standard-deviation units. Here we use it to
# assess whether random assignment and treatment receipt are balanced with
# respect to pre-treatment characteristics.
rct_standardized_mean_difference <- function(x, group) {
  mean_difference <- mean(x[group == 1]) - mean(x[group == 0])
  pooled_sd <- sqrt((var(x[group == 1]) + var(x[group == 0])) / 2)

  mean_difference / pooled_sd
}


# Extract the coefficient, standard error, and confidence interval from a
# regression model and store them in a tidy one-row data frame.
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


# Randomize pupils within schools. This mimics a blocked experiment:
# each school contributes treated and control pupils.
rct_randomize_within_school <- function(school_id, seed = 123L) {
  set.seed(seed + 100L)
  assignment <- integer(length(school_id))

  for (school in unique(school_id)) {
    pupils_in_school <- which(school_id == school)
    n_pupils <- length(pupils_in_school)
    n_assigned_small <- floor(n_pupils / 2)

    assignment[pupils_in_school] <- sample(
      c(
        rep(1L, n_assigned_small),
        rep(0L, n_pupils - n_assigned_small)
      )
    )
  }

  assignment
}


# Compute the simple Wald ratio for the non-compliance example.
# In this script, assignment is the instrument, treatment received is the
# endogenous treatment, and the outcome is the end-of-year test score.
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

  estimate <- unname(coefficients["treatment", 1])
  standard_error <- sqrt(variance_matrix["treatment", "treatment"])

  data.frame(
    estimate = estimate,
    std_error = standard_error,
    conf_low = estimate - qnorm(0.975) * standard_error,
    conf_high = estimate + qnorm(0.975) * standard_error
  )
}


# 2. Simulate the pupil population -----------------------------------------

# The population contains both observed and latent characteristics.
#
# Observed by the analyst:
#   - school_id
#   - female
#   - disadvantaged
#   - baseline_score
#   - parental_education_years
#
# Known only because this is a simulation:
#   - simulated_parental_affluence
#   - simulated_latent_motivation
#   - simulated_y0
#   - simulated_y1
#   - simulated_individual_effect
#
# In a real dataset, the analyst would never observe both potential outcomes
# or the latent motivation variable.
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

  # Baseline score is measured before assignment. It is affected by both
  # observed and unobserved pupil characteristics.
  set.seed(seed + 70L)
  baseline_score <-
    50 +
    3.0 * latent_motivation -
    4.0 * disadvantaged +
    1.4 * parental_affluence +
    0.55 * (parental_education - 12) +
    school_effects[school_id] +
    rnorm(n, mean = 0, sd = 7)

  # Y(0): potential end-of-year score if the pupil attends a regular class.
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

  # Homogeneous treatment effect: small classes raise the end-of-year score
  # by four points for every pupil.
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


# 3. Scenario A: ideal RCT --------------------------------------------------

# In the ideal experiment, assignment to a small class equals treatment
# receipt. Therefore:
#
#   assigned_small_class = received_small_class
#
# Since assignment is random, the difference in average outcomes between
# assigned groups identifies the ATE up to sampling variation.
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


# 4. Scenario B: one-sided non-compliance ----------------------------------

# In this scenario, pupils are randomly offered a small class, but only some
# of those offered one actually attend it.
#
# One-sided non-compliance:
#   - pupils assigned to a regular class cannot attend a small class;
#   - pupils assigned to a small class may or may not take up the offer.
#
# Selective compliance:
#   Take-up is more likely among pupils with higher baseline scores and
#   stronger latent motivation, and less likely among disadvantaged pupils.
#   This means treatment receipt is no longer randomly assigned.
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


# 5. Figures ---------------------------------------------------------------

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


# 6. Main simulation function ----------------------------------------------

# This is the function called from the chapter and from the master runner.
# It returns all key objects and, by default, writes the datasets, tables,
# and figures used in the book.
run_rct_simulation <- function(
    seed = 123L,
    sample_size = 5000L,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images",
    show_plot = interactive()) {

  # Step 1: create the complete simulated population.
  population <- rct_simulate_population(
    n = sample_size,
    n_schools = 20L,
    seed = seed
  )

  # Step 2: create and analyze the ideal RCT.
  ideal_data <- rct_create_ideal_data(population, seed = seed)
  true_ate <- mean(ideal_data$simulated_individual_effect)

  # In a two-group RCT, this regression coefficient is exactly the difference
  # in mean outcomes between pupils assigned to small and regular classes.
  ideal_unadjusted_model <- lm(
    end_school_year_score ~ assigned_small_class,
    data = ideal_data
  )

  # Adding school indicators respects the blocked randomization design and
  # can improve precision when schools differ in average scores.
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

  # Step 3: create and analyze selective one-sided non-compliance.
  noncompliance_data <- rct_create_noncompliance_data(
    population,
    assignment = ideal_data$assigned_small_class,
    seed = seed
  )

  # True ITT:
  #   effect of being offered a small class.
  #
  # True LATE:
  #   effect of actually attending a small class among compliers.
  #
  # In this simulation, treatment effects are homogeneous, so the ATE and LATE
  # are both equal to four. The ITT is smaller because not everyone offered a
  # small class actually takes it up.
  true_itt <- mean(
    noncompliance_data$simulated_would_comply_if_offered *
      noncompliance_data$simulated_individual_effect
  )

  true_late <- mean(
    noncompliance_data$simulated_individual_effect[
      noncompliance_data$simulated_would_comply_if_offered == 1L
    ]
  )

  # ITT model: compares pupils by random assignment.
  itt_model <- lm(
    end_school_year_score ~ assigned_small_class,
    data = noncompliance_data
  )

  # Naive as-treated model: compares pupils by actual receipt. This is not a
  # randomized comparison because receipt is selectively determined.
  as_treated_model <- lm(
    end_school_year_score ~ received_small_class,
    data = noncompliance_data
  )

  # Adjusted as-treated model: controls for observed pre-treatment variables,
  # but still cannot adjust for latent motivation.
  as_treated_adjusted_model <- lm(
    end_school_year_score ~ received_small_class + baseline_score +
      disadvantaged + female + factor(school_id),
    data = noncompliance_data
  )

  # First stage: effect of assignment on actual treatment receipt.
  first_stage_model <- lm(
    received_small_class ~ assigned_small_class,
    data = noncompliance_data
  )

  # Wald ratio: ITT divided by the first stage. In the IV chapter, this is the
  # basic logic behind LATE estimation with a binary instrument.
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

  # Step 4: create figures.
  ideal_figure <- rct_make_ideal_figure(
    ideal_data,
    ideal_estimates,
    true_ate
  )

  noncompliance_figure <- rct_make_noncompliance_figure(
    noncompliance_data,
    noncompliance_estimates
  )

  # Step 5: save book outputs.
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

  # Display the two figures in RStudio's Plots pane when the function is run
  # interactively. The figures are still saved above when write_outputs = TRUE.
  if (show_plot) {
    print(ideal_figure)
    print(noncompliance_figure)
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


# 7. Run the script directly ------------------------------------------------

# When this file is sourced from another script, the following block is not
# executed. When the file is run directly, it reproduces the RCT outputs and
# prints the main tables in the console.
if (sys.nframe() == 0L) {
  rct_results <- run_rct_simulation(
    seed = 123L,
    sample_size = 5000L,
    show_plot = TRUE
  )

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
