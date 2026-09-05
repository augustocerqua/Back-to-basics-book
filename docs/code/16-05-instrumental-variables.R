# Section 16.5: Instrumental Variables
# ============================================================
#
# This script simulates a stylized version of the Vietnam-era draft lottery.
#
# Example:
#   Draft eligibility is used as a binary instrument for military service.
#   The outcome is later earnings.
#
# Goal:
#   Show why IV can identify the LATE for compliers when the instrument is
#   randomly assigned, and how the estimate fails when eligibility is related
#   to a latent determinant of the outcome.
#
# The script creates:
#   1. an ideal IV setting with random draft eligibility;
#   2. a failure setting with non-random eligibility;
#   3. first-stage, reduced-form, OLS, Wald, and 2SLS estimates;
#   4. balance and principal-strata tables saved as CSV files;
#   5. a high-resolution PNG figure.
#
# Reproducibility:
#   All random draws use deterministic streams based on master seed 123.


# 0. Packages ---------------------------------------------------------------

required_packages <- c("dplyr", "ggplot2", "patchwork", "ivreg", "sandwich")
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


# 1. Simulate potential outcomes and principal strata ----------------------

# Principal strata describe how each person would respond to draft
# eligibility: comply, always serve, or never serve.
iv_simulate_population <- function(seed = 123L, sample_size = 5000L) {
  stopifnot(sample_size >= 100L)

  set.seed(seed + 710L)
  latent_ability <- rnorm(sample_size)
  latent_service_preference <- rnorm(sample_size)
  education_noise <- rnorm(sample_size, sd = 1.1)
  earnings_noise <- rnorm(sample_size, sd = 4.0)

  education_years <- pmin(
    18,
    pmax(8, round(12.5 + 1.25 * latent_ability + education_noise))
  )

  # Exact principal-strata shares: 10% always-takers, 35% compliers,
  # and 55% never-takers. Higher service preference increases take-up but is
  # not observed by the analyst.
  service_rank <- rank(-latent_service_preference, ties.method = "first")
  always_cutoff <- floor(0.10 * sample_size)
  complier_cutoff <- floor(0.45 * sample_size)
  principal_stratum <- case_when(
    service_rank <= always_cutoff ~ "Always-taker",
    service_rank <= complier_cutoff ~ "Complier",
    TRUE ~ "Never-taker"
  )

  treatment_effect <- case_when(
    principal_stratum == "Always-taker" ~ -2.0,
    principal_stratum == "Complier" ~ -5.0,
    TRUE ~ -3.5
  )

  earnings_without_service <-
    45 +
    1.15 * (education_years - 12) +
    2.25 * latent_ability +
    1.75 * latent_service_preference +
    earnings_noise

  data.frame(
    person_id = seq_len(sample_size),
    education_years = education_years,
    simulated_latent_ability = latent_ability,
    simulated_service_preference = latent_service_preference,
    simulated_principal_stratum = principal_stratum,
    simulated_treatment_effect = treatment_effect,
    simulated_earnings_without_service = earnings_without_service,
    simulated_earnings_with_service =
      earnings_without_service + treatment_effect
  )
}


# 2. Apply the instrument ---------------------------------------------------

# In the ideal scenario, draft eligibility is random. In the failure scenario,
# eligibility is correlated with latent ability, so the instrument is no longer
# as good as randomly assigned.
iv_apply_assignment <- function(population, seed = 123L, scenario) {
  sample_size <- nrow(population)
  eligible_count <- floor(sample_size / 2)

  if (scenario == "ideal") {
    set.seed(seed + 720L)
    lottery_order <- sample.int(sample_size)
    draft_eligible <- as.integer(lottery_order <= eligible_count)
    scenario_label <- "Ideal: random lottery"
  } else if (scenario == "failure") {
    # Hypothetical failure: low lottery scores, and hence eligibility, are
    # partly determined by latent ability. This is not a claim about the
    # historical Vietnam draft lottery.
    set.seed(seed + 730L)
    nonrandom_lottery_score <-
      0.55 * population$simulated_latent_ability + rnorm(sample_size)
    lottery_order <- rank(nonrandom_lottery_score, ties.method = "first")
    draft_eligible <- as.integer(lottery_order <= eligible_count)
    scenario_label <- "Failure: non-random eligibility"
  } else {
    stop("scenario must be either 'ideal' or 'failure'.")
  }

  served_military <- as.integer(
    population$simulated_principal_stratum == "Always-taker" |
      (
        population$simulated_principal_stratum == "Complier" &
          draft_eligible == 1L
      )
  )

  observed_earnings <- ifelse(
    served_military == 1L,
    population$simulated_earnings_with_service,
    population$simulated_earnings_without_service
  )

  population %>%
    mutate(
      scenario = scenario_label,
      simulated_lottery_order = lottery_order,
      draft_eligible = draft_eligible,
      served_military = served_military,
      annual_earnings_thousands = observed_earnings
    )
}


# 3. Estimate OLS, first stage, reduced form, Wald, and 2SLS ----------------

iv_robust_result <- function(model, term) {
  robust_vcov <- sandwich::vcovHC(model, type = "HC1")
  estimate <- unname(coef(model)[term])
  standard_error <- sqrt(unname(diag(robust_vcov)[term]))

  data.frame(
    estimate = estimate,
    standard_error = standard_error,
    conf_low = estimate - 1.96 * standard_error,
    conf_high = estimate + 1.96 * standard_error
  )
}

iv_estimate_scenario <- function(data) {
  first_stage_model <- lm(
    served_military ~ draft_eligible,
    data = data
  )
  reduced_form_model <- lm(
    annual_earnings_thousands ~ draft_eligible,
    data = data
  )
  iv_model <- ivreg::ivreg(
    annual_earnings_thousands ~ served_military | draft_eligible,
    data = data
  )
  naive_model <- lm(
    annual_earnings_thousands ~ served_military,
    data = data
  )

  first_stage <- iv_robust_result(first_stage_model, "draft_eligible")
  reduced_form <- iv_robust_result(reduced_form_model, "draft_eligible")
  iv_result <- iv_robust_result(iv_model, "served_military")
  naive_result <- iv_robust_result(naive_model, "served_military")

  wald_ratio <- reduced_form$estimate / first_stage$estimate
  if (!isTRUE(all.equal(wald_ratio, iv_result$estimate, tolerance = 1e-10))) {
    stop("The Wald ratio and just-identified 2SLS estimate do not agree.")
  }

  true_late <- mean(
    data$simulated_treatment_effect[
      data$simulated_principal_stratum == "Complier"
    ]
  )
  true_att <- mean(
    data$simulated_treatment_effect[data$served_military == 1L]
  )

  bind_rows(
    data.frame(
      scenario = unique(data$scenario),
      estimator = "First stage",
      first_stage,
      true_target = NA_real_
    ),
    data.frame(
      scenario = unique(data$scenario),
      estimator = "Reduced form",
      reduced_form,
      true_target = first_stage$estimate * true_late
    ),
    data.frame(
      scenario = unique(data$scenario),
      estimator = "Wald ratio / 2SLS",
      iv_result,
      true_target = true_late
    ),
    data.frame(
      scenario = unique(data$scenario),
      estimator = "Naive as-treated comparison",
      naive_result,
      true_target = true_att
    )
  )
}


# 4. Build diagnostics and figure inputs -----------------------------------

iv_absolute_standardized_difference <- function(data, variable) {
  values <- data[[variable]]
  eligible <- values[data$draft_eligible == 1L]
  ineligible <- values[data$draft_eligible == 0L]
  pooled_sd <- sqrt((var(eligible) + var(ineligible)) / 2)
  abs(mean(eligible) - mean(ineligible)) / pooled_sd
}

iv_build_balance_results <- function(ideal_data, failure_data) {
  variables <- c("education_years", "simulated_latent_ability")
  labels <- c("Education", "Latent ability")

  bind_rows(lapply(list(ideal_data, failure_data), function(data) {
    data.frame(
      scenario = unique(data$scenario),
      characteristic = factor(labels, levels = labels),
      absolute_standardized_difference = vapply(
        variables,
        function(variable) {
          iv_absolute_standardized_difference(data, variable)
        },
        numeric(1)
      )
    )
  }))
}

iv_build_service_rates <- function(ideal_data, failure_data) {
  bind_rows(ideal_data, failure_data) %>%
    group_by(scenario, draft_eligible) %>%
    summarise(service_rate = mean(served_military), .groups = "drop") %>%
    mutate(
      eligibility = if_else(
        draft_eligible == 1L,
        "Draft eligible",
        "Not draft eligible"
      )
    )
}

iv_build_principal_strata <- function(ideal_data) {
  ideal_data %>%
    count(simulated_principal_stratum, name = "individuals") %>%
    mutate(
      share = individuals / sum(individuals),
      treatment_if_ineligible = case_when(
        simulated_principal_stratum == "Always-taker" ~ 1L,
        TRUE ~ 0L
      ),
      treatment_if_eligible = case_when(
        simulated_principal_stratum == "Never-taker" ~ 0L,
        TRUE ~ 1L
      )
    ) %>%
    arrange(desc(treatment_if_ineligible), desc(treatment_if_eligible))
}

iv_make_figure <- function(balance_results, service_rates, estimates) {
  scenario_colors <- c(
    "Ideal: random lottery" = "#4C8BCB",
    "Failure: non-random eligibility" = "#D9794A"
  )

  balance_panel <- ggplot(
    balance_results,
    aes(
      x = characteristic,
      y = absolute_standardized_difference,
      fill = scenario
    )
  ) +
    geom_hline(
      yintercept = 0.10,
      linetype = "dashed",
      linewidth = 0.65,
      color = "#777777"
    ) +
    geom_col(position = position_dodge(width = 0.72), width = 0.62) +
    scale_fill_manual(values = scenario_colors) +
    labs(
      title = "A. Is draft eligibility balanced?",
      x = NULL,
      y = "Absolute standardized difference",
      fill = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  service_panel <- ggplot(
    service_rates,
    aes(x = eligibility, y = service_rate, fill = eligibility)
  ) +
    geom_col(width = 0.62) +
    facet_wrap(~scenario) +
    scale_fill_manual(
      values = c(
        "Not draft eligible" = "#AFC6DD",
        "Draft eligible" = "#315A7D"
      )
    ) +
    scale_y_continuous(
      limits = c(0, 0.55),
      breaks = seq(0, 0.5, by = 0.1),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    labs(
      title = "B. First stage: eligibility raises military service",
      x = NULL,
      y = "Military service rate"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 9.5)
    )

  iv_estimates <- estimates %>%
    filter(estimator == "Wald ratio / 2SLS") %>%
    mutate(
      scenario = factor(
        scenario,
        levels = c(
          "Failure: non-random eligibility",
          "Ideal: random lottery"
        )
      )
    )

  estimate_panel <- ggplot(
    iv_estimates,
    aes(x = estimate, y = scenario, color = scenario)
  ) +
    geom_vline(
      xintercept = unique(iv_estimates$true_target),
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
      title = "C. IV estimate versus the true complier effect",
      subtitle = "Dashed line: true LATE = -5 thousand dollars",
      x = "Effect on annual earnings (thousand dollars)",
      y = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  (balance_panel + service_panel) /
    estimate_panel +
    plot_layout(heights = c(1.05, 0.8))
}


# 5. Main simulation function ----------------------------------------------

# This is the function called from the chapter and from the master runner.
# It returns all key objects and, by default, writes the datasets, tables,
# and figure used in the book.
run_iv_simulation <- function(
    seed = 123L,
    sample_size = 5000L,
    write_outputs = TRUE,
    data_directory = "data/simulations",
    image_directory = "images",
    show_plot = interactive()) {
  population <- iv_simulate_population(
    seed = seed,
    sample_size = sample_size
  )
  ideal_data <- iv_apply_assignment(
    population,
    seed = seed,
    scenario = "ideal"
  )
  failure_data <- iv_apply_assignment(
    population,
    seed = seed,
    scenario = "failure"
  )

  estimates <- bind_rows(
    iv_estimate_scenario(ideal_data),
    iv_estimate_scenario(failure_data)
  )
  balance_results <- iv_build_balance_results(ideal_data, failure_data)
  service_rates <- iv_build_service_rates(ideal_data, failure_data)
  principal_strata <- iv_build_principal_strata(ideal_data)
  iv_figure <- iv_make_figure(
    balance_results,
    service_rates,
    estimates
  )

  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)
    write.csv(
      ideal_data,
      file.path(data_directory, "iv_vietnam_draft_ideal.csv"),
      row.names = FALSE
    )
    write.csv(
      failure_data,
      file.path(data_directory, "iv_vietnam_draft_failure.csv"),
      row.names = FALSE
    )
    write.csv(
      estimates,
      file.path(data_directory, "iv_vietnam_draft_estimates.csv"),
      row.names = FALSE
    )
    write.csv(
      principal_strata,
      file.path(data_directory, "iv_vietnam_draft_principal_strata.csv"),
      row.names = FALSE
    )
    ggsave(
      file.path(image_directory, "iv-vietnam-draft-simulation.png"),
      iv_figure,
      width = 12,
      height = 8.2,
      dpi = 320,
      bg = "white"
    )
  }

  # Display the figure in RStudio's Plots pane when the function is run
  # interactively. The figure is still saved above when write_outputs = TRUE.
  if (show_plot) {
    print(iv_figure)
  }

  list(
    ideal_data = ideal_data,
    failure_data = failure_data,
    estimates = estimates,
    balance = balance_results,
    service_rates = service_rates,
    principal_strata = principal_strata,
    figure = iv_figure
  )
}


# 6. Run the script directly ------------------------------------------------

# When this file is run directly, it reproduces the IV outputs and prints the
# main tables. When it is sourced by another file, this block is skipped.
if (sys.nframe() == 0L) {
  iv_results <- run_iv_simulation(
    seed = 123L,
    sample_size = 5000L,
    show_plot = TRUE
  )
  cat("\nPrincipal strata\n")
  print(iv_results$principal_strata, row.names = FALSE, digits = 3)
  cat("\nInstrumental-variable estimates\n")
  print(iv_results$estimates, row.names = FALSE, digits = 3)
  cat("\nBalance of draft eligibility\n")
  print(iv_results$balance, row.names = FALSE, digits = 3)
}
