# Section 16.9: Machine Learning Control Method
# ============================================================
#
# This script simulates a stylized MLCM application inspired by local excess
# mortality estimation during the COVID-19 pandemic in Italy.
#
# Example:
#   A pandemic affects all municipalities in the country. Since all units are
#   exposed, there is no contemporaneous untreated control group. The analyst
#   therefore uses pre-pandemic information to predict what each municipality's
#   2020 mortality would have been in an ordinary no-pandemic year.
#
# Goal:
#   Show how MLCM builds unit-level counterfactual predictions and how the
#   estimated effects can be summarized by area.
#
# The script creates:
#   1. an ideal setting, where the only 2020 shock is the pandemic;
#   2. a failure setting, where a non-pandemic reporting shock also affects
#      some areas in 2020;
#   3. datasets and estimate tables saved as CSV files;
#
# Reproducibility:
#   All random draws use deterministic streams based on master seed 123.


# 0. Packages ---------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "patchwork", "glmnet", "ranger"
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
library(tidyr)
library(ggplot2)
library(patchwork)
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(ranger))


# 1. Simulate municipalities ------------------------------------------------

# Municipalities differ in area, population size, demographic structure,
# pollution exposure, density, and healthcare access. These variables help
# predict ordinary mortality in the absence of the pandemic.
mlcm_simulate_municipalities <- function(
    seed = 123L,
    n_municipalities = 800L) {
  set.seed(seed + 901L)

  area <- sample(
    c("North West", "North East", "Center", "South and Islands"),
    size = n_municipalities,
    replace = TRUE,
    prob = c(0.25, 0.22, 0.21, 0.32)
  )

  area_factor <- factor(
    area,
    levels = c("North West", "North East", "Center", "South and Islands")
  )

  population_log <- rnorm(
    n_municipalities,
    mean = 8.0 +
      ifelse(area == "North West", 0.15, 0) +
      ifelse(area == "Center", 0.10, 0),
    sd = 0.85
  )

  population <- round(exp(population_log))
  population <- pmax(population, 600)

  elderly_share <- pmin(
    35,
    pmax(
      12,
      rnorm(
        n_municipalities,
        mean = 23 +
          ifelse(area == "North West", 1.4, 0) +
          ifelse(area == "North East", 0.9, 0) -
          ifelse(area == "South and Islands", 1.2, 0),
        sd = 3.2
      )
    )
  )

  population_density <- exp(
    rnorm(
      n_municipalities,
      mean = 5.0 +
        ifelse(area == "North West", 0.35, 0) +
        ifelse(area == "Center", 0.10, 0),
      sd = 0.75
    )
  )

  pm10 <- pmin(
    48,
    pmax(
      12,
      rnorm(
        n_municipalities,
        mean = 23 +
          ifelse(area == "North West", 7.0, 0) +
          ifelse(area == "North East", 4.5, 0) -
          ifelse(area == "South and Islands", 3.0, 0),
        sd = 4.5
      )
    )
  )

  hospital <- rbinom(
    n_municipalities,
    size = 1,
    prob = plogis(-5.1 + 0.55 * population_log)
  )

  data.frame(
    municipality_id = seq_len(n_municipalities),
    area = area_factor,
    population = population,
    population_log = population_log,
    population_log_squared = population_log^2,
    elderly_share = elderly_share,
    population_density = population_density,
    log_population_density = log(population_density),
    pm10 = pm10,
    elderly_pm10 = elderly_share * pm10,
    hospital = hospital
  )
}


# 2. Simulate ordinary mortality and pandemic effects -----------------------

# The outcome is cumulative all-cause deaths per 10,000 inhabitants over a
# target period. The observed 2020 outcome is treated as the pandemic year.
mlcm_simulate_panel <- function(
    municipalities,
    seed = 123L,
    years = 2015:2020) {
  n_municipalities <- nrow(municipalities)

  panel <- tidyr::expand_grid(
    municipality_id = municipalities$municipality_id,
    year = years
  ) %>%
    left_join(municipalities, by = "municipality_id") %>%
    arrange(municipality_id, year)

  set.seed(seed + 902L)
  municipality_unobserved_risk <- rnorm(n_municipalities, mean = 0, sd = 1.0)

  # Ordinary year effects are mild and predictable. The ideal scenario should
  # fail or succeed because of the counterfactual method, not because 2020
  # contains a purely random no-pandemic shock that no model could foresee.
  year_shock <- c(-0.20, -0.05, 0.05, 0.02, 0.10, 0.15)
  names(year_shock) <- years

  panel <- panel %>%
    mutate(
      unobserved_risk =
        municipality_unobserved_risk[municipality_id],
      year_centered = year - 2015,
      ordinary_mortality =
        13.5 +
        0.42 * elderly_share +
        0.020 * pm10 +
        0.70 * hospital +
        0.50 * log(population_density) -
        0.08 * population_log +
        0.018 * elderly_share * (pm10 - 23) / 10 +
        0.18 * year_centered +
        ifelse(area == "North West", 1.3, 0) +
        ifelse(area == "North East", 0.6, 0) -
        ifelse(area == "South and Islands", 0.8, 0) +
        unobserved_risk +
        year_shock[as.character(year)]
    )

  set.seed(seed + 904L)
  panel$ordinary_mortality <- panel$ordinary_mortality +
    rnorm(nrow(panel), mean = 0, sd = 0.85)

  panel <- panel %>%
    group_by(municipality_id) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      previous_year_mortality = lag(ordinary_mortality),
      previous_year_mortality = ifelse(
        is.na(previous_year_mortality),
        ordinary_mortality,
        previous_year_mortality
      )
    ) %>%
    ungroup()

  # The pandemic effect varies strongly across areas and is larger in places
  # with higher elderly shares and higher pollution exposure.
  panel <- panel %>%
    mutate(
      true_pandemic_effect = ifelse(
        year == 2020,
        3.0 +
          ifelse(area == "North West", 13.0, 0) +
          ifelse(area == "North East", 7.0, 0) +
          ifelse(area == "Center", 2.0, 0) -
          ifelse(area == "South and Islands", 1.0, 0) +
          0.11 * (elderly_share - mean(elderly_share)) +
          0.07 * (pm10 - mean(pm10)),
        0
      ),
      # Failure setting: a non-pandemic reporting shock increases recorded
      # deaths in some areas in 2020. This is not part of the pandemic effect,
      # but a forecast-based method would see it in the observed outcome.
      reporting_shock = ifelse(
        year == 2020 & area == "South and Islands",
        3.0,
        ifelse(year == 2020 & area == "Center", 1.0, 0)
      ),
      observed_mortality_ideal =
        ordinary_mortality + true_pandemic_effect,
      observed_mortality_failure =
        ordinary_mortality + true_pandemic_effect + reporting_shock
    )

  panel
}


# 3. Prediction models ------------------------------------------------------

# The intuitive method predicts 2020 ordinary mortality using the historical
# average for the same municipality. This mirrors the simple benchmark used in
# many excess-mortality exercises.
mlcm_historical_average_prediction <- function(panel, target_year = 2020L) {
  panel %>%
    filter(year < target_year) %>%
    group_by(municipality_id) %>%
    summarise(
      historical_average_prediction = mean(ordinary_mortality),
      .groups = "drop"
    )
}


# We use the same feature set for LASSO and random forest. The formula includes
# a few nonlinear terms and interactions because mortality is unlikely to be
# driven by only additive linear relationships. These transformed variables are
# created explicitly in the dataset so that ranger can handle them cleanly.
mlcm_feature_formula <- function() {
  ordinary_mortality ~
    year_centered +
    area +
    previous_year_mortality +
    elderly_share +
    pm10 +
    elderly_pm10 +
    population_log +
    population_log_squared +
    log_population_density +
    hospital
}


# LASSO requires a numeric design matrix. model.matrix() expands factor
# variables such as area into dummy variables.
mlcm_make_design_matrix <- function(data) {
  model.matrix(mlcm_feature_formula(), data = data)[, -1, drop = FALSE]
}


mlcm_make_lambda_grid <- function(x, y, n_lambda = 100L) {
  centered_y <- y - mean(y)
  lambda_max <- max(abs(crossprod(x, centered_y))) / length(y)
  exp(seq(log(lambda_max), log(lambda_max * 0.001), length.out = n_lambda))
}


# Tune LASSO by cross-validation over exactly 100 lambda values.
mlcm_fit_lasso_cv <- function(training_data, seed = 123L, n_lambda = 100L) {
  x <- mlcm_make_design_matrix(training_data)
  y <- training_data$ordinary_mortality
  lambda_grid <- mlcm_make_lambda_grid(x, y, n_lambda = n_lambda)

  set.seed(seed)
  cv_fit <- glmnet::cv.glmnet(
    x = x,
    y = y,
    alpha = 1,
    lambda = lambda_grid,
    nfolds = 5
  )

  final_fit <- glmnet::glmnet(
    x = x,
    y = y,
    alpha = 1,
    lambda = cv_fit$lambda.min
  )

  list(
    method = "LASSO",
    model = final_fit,
    lambda = cv_fit$lambda.min,
    cv_rmse = sqrt(min(cv_fit$cvm)),
    lambda_grid = lambda_grid
  )
}


mlcm_predict_lasso <- function(fit, new_data) {
  x_new <- mlcm_make_design_matrix(new_data)
  as.numeric(predict(fit$model, newx = x_new, s = fit$lambda))
}


# Tune random forest by cross-validation over three values of mtry:
# all covariates, half of the covariates, or one third of the covariates.
mlcm_fit_random_forest_cv <- function(
    training_data,
    seed = 123L,
    n_folds = 3L,
    n_trees = 1000L) {
  formula <- mlcm_feature_formula()
  p <- length(attr(terms(formula), "term.labels"))

  mtry_grid <- unique(pmax(1L, c(p, floor(p / 2), floor(p / 3))))
  mtry_grid <- sort(mtry_grid, decreasing = TRUE)

  set.seed(seed)
  fold_id <- sample(rep(seq_len(n_folds), length.out = nrow(training_data)))

  cv_results <- lapply(mtry_grid, function(mtry_value) {
    fold_errors <- numeric(n_folds)

    for (fold in seq_len(n_folds)) {
      fold_train <- training_data[fold_id != fold, ]
      fold_test <- training_data[fold_id == fold, ]

      set.seed(seed + 100L * mtry_value + fold)
      fold_fit <- ranger::ranger(
        formula,
        data = fold_train,
        num.trees = n_trees,
        mtry = mtry_value
      )

      fold_prediction <- predict(fold_fit, data = fold_test)$predictions
      fold_errors[fold] <- mean(
        (fold_prediction - fold_test$ordinary_mortality)^2
      )
    }

    data.frame(
      mtry = mtry_value,
      cv_rmse = sqrt(mean(fold_errors))
    )
  })

  cv_table <- bind_rows(cv_results)
  best_mtry <- cv_table$mtry[which.min(cv_table$cv_rmse)]

  set.seed(seed + 500L)
  final_fit <- ranger::ranger(
    formula,
    data = training_data,
    num.trees = n_trees,
    mtry = best_mtry
  )

  list(
    method = "Random forest",
    model = final_fit,
    mtry = best_mtry,
    cv_rmse = min(cv_table$cv_rmse),
    cv_table = cv_table
  )
}


mlcm_predict_random_forest <- function(fit, new_data) {
  as.numeric(predict(fit$model, data = new_data)$predictions)
}


mlcm_predict_model <- function(fit, new_data) {
  if (fit$method == "LASSO") {
    return(mlcm_predict_lasso(fit, new_data))
  }

  if (fit$method == "Random forest") {
    return(mlcm_predict_random_forest(fit, new_data))
  }

  stop("Unknown MLCM model type: ", fit$method)
}


mlcm_fit_candidate_models <- function(training_data, seed = 123L) {
  lasso <- mlcm_fit_lasso_cv(
    training_data = training_data,
    seed = seed + 10L,
    n_lambda = 100L
  )

  random_forest <- mlcm_fit_random_forest_cv(
    training_data = training_data,
    seed = seed + 20L
  )

  list(
    lasso = lasso,
    random_forest = random_forest
  )
}


# 4. Estimate excess mortality ---------------------------------------------

mlcm_estimate_scenario <- function(
    panel,
    observed_outcome,
    scenario_label,
    selected_method,
    seed = 123L,
    target_year = 2020L) {
  training_data <- panel %>%
    filter(year < target_year, year > min(year))

  target_data <- panel %>%
    filter(year == target_year)

  historical_predictions <- mlcm_historical_average_prediction(
    panel,
    target_year = target_year
  )

  candidate_models <- mlcm_fit_candidate_models(
    training_data = training_data,
    seed = seed + 950L
  )

  selected_model <- switch(
    selected_method,
    "LASSO" = candidate_models$lasso,
    "Random forest" = candidate_models$random_forest,
    stop("Unknown selected method: ", selected_method)
  )

  target_estimates <- target_data %>%
    left_join(historical_predictions, by = "municipality_id") %>%
    mutate(
      scenario = scenario_label,
      observed_mortality = .data[[observed_outcome]],
      selected_ml_method = selected_method,
      mlcm_prediction = mlcm_predict_model(selected_model, target_data),
      intuitive_excess = observed_mortality -
        historical_average_prediction,
      mlcm_excess = observed_mortality - mlcm_prediction,
      true_excess = true_pandemic_effect,
      non_pandemic_shock =
        if (observed_outcome == "observed_mortality_failure") {
          reporting_shock
        } else {
          0
        },
      intuitive_error = intuitive_excess - true_excess,
      mlcm_error = mlcm_excess - true_excess,
      true_no_pandemic_mortality = ordinary_mortality
    )

  area_estimates <- target_estimates %>%
    group_by(scenario, area) %>%
    summarise(
      municipalities = n(),
      true_excess = mean(true_excess),
      intuitive_excess = mean(intuitive_excess),
      mlcm_excess = mean(mlcm_excess),
      average_non_pandemic_shock = mean(non_pandemic_shock),
      mlcm_standard_error = sd(mlcm_excess) / sqrt(n()),
      mlcm_conf_low = mlcm_excess - qnorm(0.975) * mlcm_standard_error,
      mlcm_conf_high = mlcm_excess + qnorm(0.975) * mlcm_standard_error,
      .groups = "drop"
    )

  overall_estimates <- target_estimates %>%
    summarise(
      scenario = scenario_label,
      municipalities = n(),
      true_excess = mean(true_excess),
      intuitive_excess = mean(intuitive_excess),
      mlcm_excess = mean(mlcm_excess),
      average_non_pandemic_shock = mean(non_pandemic_shock),
      intuitive_rmse = sqrt(mean(intuitive_error^2)),
      mlcm_rmse = sqrt(mean(mlcm_error^2))
    )

  list(
    municipality_estimates = target_estimates,
    area_estimates = area_estimates,
    overall_estimates = overall_estimates,
    candidate_models = candidate_models,
    selected_model = selected_model
  )
}


# 5. Validate prediction in an ordinary year --------------------------------

# Before using a forecasting method causally, it is useful to ask whether it can
# predict an ordinary year that was not used for training. Here we train on
# 2015-2018 and validate on 2019, when there is no pandemic by construction.
mlcm_validate_prediction <- function(panel, seed = 123L) {
  training_data <- panel %>%
    filter(year <= 2018, year > min(year))

  validation_data <- panel %>%
    filter(year == 2019)

  historical_predictions <- panel %>%
    filter(year <= 2018) %>%
    group_by(municipality_id) %>%
    summarise(
      historical_average_prediction = mean(ordinary_mortality),
      .groups = "drop"
    )

  candidate_models <- mlcm_fit_candidate_models(
    training_data = training_data,
    seed = seed + 960L
  )

  validation_predictions <- validation_data %>%
    left_join(historical_predictions, by = "municipality_id") %>%
    mutate(
      lasso_prediction =
        mlcm_predict_lasso(candidate_models$lasso, validation_data),
      random_forest_prediction =
        mlcm_predict_random_forest(
          candidate_models$random_forest,
          validation_data
        ),
      historical_average_error =
        historical_average_prediction - ordinary_mortality,
      lasso_error = lasso_prediction - ordinary_mortality,
      random_forest_error = random_forest_prediction - ordinary_mortality
    )

  validation_results <- data.frame(
    method = c("Historical average", "LASSO", "Random forest"),
    validation_year = 2019,
    rmse = c(
      sqrt(mean(validation_predictions$historical_average_error^2)),
      sqrt(mean(validation_predictions$lasso_error^2)),
      sqrt(mean(validation_predictions$random_forest_error^2))
    ),
    mae = c(
      mean(abs(validation_predictions$historical_average_error)),
      mean(abs(validation_predictions$lasso_error)),
      mean(abs(validation_predictions$random_forest_error))
    ),
    tuning_choice = c(
      "No tuning",
      paste0("lambda = ", signif(candidate_models$lasso$lambda, 3)),
      paste0("mtry = ", candidate_models$random_forest$mtry)
    ),
    internal_cv_rmse = c(
      NA_real_,
      candidate_models$lasso$cv_rmse,
      candidate_models$random_forest$cv_rmse
    )
  )

  ml_rows <- validation_results$method %in% c("LASSO", "Random forest")
  selected_method <- validation_results$method[
    which.min(ifelse(ml_rows, validation_results$rmse, Inf))
  ]

  validation_results$selected_for_2020 <- validation_results$method ==
    selected_method

  list(
    validation_results = validation_results,
    validation_predictions = validation_predictions,
    selected_method = selected_method,
    candidate_models = candidate_models
  )
}


# 6. Build the figure -------------------------------------------------------

mlcm_make_figure <- function(
    municipality_estimates,
    area_estimates,
    validation_predictions,
    selected_method) {
  ideal_area <- area_estimates %>%
    filter(scenario == "Ideal: pandemic shock only") %>%
    select(area, true_excess, intuitive_excess, mlcm_excess) %>%
    tidyr::pivot_longer(
      cols = c(true_excess, intuitive_excess, mlcm_excess),
      names_to = "series",
      values_to = "excess"
    ) %>%
    mutate(
      series = recode(
        series,
        true_excess = "True excess mortality",
        intuitive_excess = "Historical average",
        mlcm_excess = "MLCM estimate"
      )
    )

  area_panel <- ggplot(
    ideal_area,
    aes(x = area, y = excess, color = series, shape = series)
  ) +
    geom_point(
      position = position_dodge(width = 0.55),
      size = 3.3
    ) +
    scale_color_manual(
      values = c(
        "True excess mortality" = "#238B45",
        "Historical average" = "#8C8C8C",
        "MLCM estimate" = "#275D8C"
      )
    ) +
    scale_shape_manual(
      values = c(
        "True excess mortality" = 17,
        "Historical average" = 16,
        "MLCM estimate" = 15
      )
    ) +
    labs(
      title = paste0("A. Average excess mortality by area: ", selected_method),
      x = NULL,
      y = "Deaths per 10,000 inhabitants",
      color = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )

  distribution_panel <- municipality_estimates %>%
    filter(scenario == "Ideal: pandemic shock only") %>%
    ggplot(aes(x = area, y = mlcm_excess, fill = area)) +
    geom_boxplot(width = 0.55, alpha = 0.72, outlier.alpha = 0.25) +
    scale_fill_manual(
      values = c(
        "North West" = "#D4513F",
        "North East" = "#D9794A",
        "Center" = "#E6B95D",
        "South and Islands" = "#4C8BCB"
      )
    ) +
    labs(
      title = "B. Municipality-level MLCM estimates",
      x = NULL,
      y = "Estimated excess mortality"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )

  failure_area <- area_estimates %>%
    filter(scenario == "Failure: extra reporting shock") %>%
    select(area, true_excess, mlcm_excess, average_non_pandemic_shock) %>%
    tidyr::pivot_longer(
      cols = c(true_excess, mlcm_excess),
      names_to = "series",
      values_to = "excess"
    ) %>%
    mutate(
      series = recode(
        series,
        true_excess = "True pandemic effect",
        mlcm_excess = "MLCM estimate"
      )
    )

  failure_panel <- ggplot(
    failure_area,
    aes(x = area, y = excess, color = series, shape = series)
  ) +
    geom_col(
      data = area_estimates %>%
        filter(scenario == "Failure: extra reporting shock"),
      aes(x = area, y = average_non_pandemic_shock),
      inherit.aes = FALSE,
      fill = "#F2DCA2",
      alpha = 0.75,
      width = 0.45
    ) +
    geom_point(
      position = position_dodge(width = 0.55),
      size = 3.3
    ) +
    scale_color_manual(
      values = c(
        "True pandemic effect" = "#238B45",
        "MLCM estimate" = "#D4513F"
      )
    ) +
    scale_shape_manual(
      values = c(
        "True pandemic effect" = 17,
        "MLCM estimate" = 15
      )
    ) +
    labs(
      title = "C. Failure: non-pandemic shock by area",
      subtitle = "Gold bars show the extra reporting shock",
      x = NULL,
      y = "Deaths per 10,000 inhabitants",
      color = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1)
    )

  prediction_error_panel <- municipality_estimates %>%
    filter(scenario == "Ideal: pandemic shock only") %>%
    transmute(
      area,
      `Historical average` = abs(intuitive_error),
      MLCM = abs(mlcm_error)
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Historical average`, MLCM),
      names_to = "method",
      values_to = "absolute_error"
    ) %>%
    ggplot(aes(x = method, y = absolute_error, fill = method)) +
    geom_boxplot(width = 0.56, alpha = 0.75, outlier.alpha = 0.18) +
    scale_fill_manual(
      values = c(
        "Historical average" = "#8C8C8C",
        "MLCM" = "#275D8C"
      )
    ) +
    labs(
      title = "D. 2020 prediction error in the ideal scenario",
      x = NULL,
      y = "Absolute error"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 20, hjust = 1)
    )

  placebo_error_panel <- validation_predictions %>%
    transmute(
      `Historical average` = abs(historical_average_error),
      LASSO = abs(lasso_error),
      `Random forest` = abs(random_forest_error)
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Historical average`, LASSO, `Random forest`),
      names_to = "method",
      values_to = "absolute_error"
    ) %>%
    mutate(
      selected = ifelse(method == selected_method, "Selected", "Not selected")
    ) %>%
    ggplot(aes(x = method, y = absolute_error, fill = method)) +
    geom_boxplot(width = 0.56, alpha = 0.75, outlier.alpha = 0.18) +
    scale_fill_manual(
      values = c(
        "Historical average" = "#8C8C8C",
        "LASSO" = "#4C8BCB",
        "Random forest" = "#D9794A"
      )
    ) +
    labs(
      title = "E. Placebo-year prediction error",
      subtitle = paste0("Selected MLCM algorithm: ", selected_method),
      x = NULL,
      y = "Absolute error in 2019"
    ) +
    theme_minimal(base_size = 12.5) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 20, hjust = 1)
    )

  (area_panel + distribution_panel) /
    (failure_panel + prediction_error_panel) /
    placebo_error_panel +
    plot_layout(heights = c(1, 1, 0.85))
}


# 7. Main simulation function ----------------------------------------------

# This is the function called from the chapter and from the master runner.
# It returns all key objects and, by default, writes the datasets, tables,
# and figure used in the book.
run_mlcm_simulation <- function(
    seed = 123L,
    write_outputs = TRUE,
    show_plot = interactive(),
    data_directory = "data/simulations",
    image_directory = "images") {

  # Step 1: generate municipalities and their ordinary mortality paths.
  municipalities <- mlcm_simulate_municipalities(seed = seed)
  panel <- mlcm_simulate_panel(municipalities, seed = seed)

  # Step 2: validate predictive accuracy in an ordinary pre-pandemic year.
  validation <- mlcm_validate_prediction(panel, seed = seed)
  validation_results <- validation$validation_results
  selected_method <- validation$selected_method

  # Step 3: estimate excess mortality in the ideal scenario.
  ideal <- mlcm_estimate_scenario(
    panel = panel,
    observed_outcome = "observed_mortality_ideal",
    scenario_label = "Ideal: pandemic shock only",
    selected_method = selected_method,
    seed = seed
  )

  # Step 4: estimate excess mortality in the failure scenario.
  failure <- mlcm_estimate_scenario(
    panel = panel,
    observed_outcome = "observed_mortality_failure",
    scenario_label = "Failure: extra reporting shock",
    selected_method = selected_method,
    seed = seed
  )

  municipality_estimates <- bind_rows(
    ideal$municipality_estimates,
    failure$municipality_estimates
  )

  area_estimates <- bind_rows(
    ideal$area_estimates,
    failure$area_estimates
  )

  overall_estimates <- bind_rows(
    ideal$overall_estimates,
    failure$overall_estimates
  )

  mlcm_figure <- mlcm_make_figure(
    municipality_estimates,
    area_estimates,
    validation$validation_predictions,
    selected_method
  )

  # Step 5: save book outputs.
  if (write_outputs) {
    dir.create(data_directory, recursive = TRUE, showWarnings = FALSE)
    dir.create(image_directory, recursive = TRUE, showWarnings = FALSE)

    write.csv(
      panel,
      file.path(data_directory, "mlcm_mortality_panel.csv"),
      row.names = FALSE
    )

    write.csv(
      municipality_estimates,
      file.path(data_directory, "mlcm_mortality_municipality_estimates.csv"),
      row.names = FALSE
    )

    write.csv(
      area_estimates,
      file.path(data_directory, "mlcm_mortality_area_estimates.csv"),
      row.names = FALSE
    )

    write.csv(
      overall_estimates,
      file.path(data_directory, "mlcm_mortality_overall_estimates.csv"),
      row.names = FALSE
    )

    write.csv(
      validation_results,
      file.path(data_directory, "mlcm_mortality_validation.csv"),
      row.names = FALSE
    )

    write.csv(
      validation$validation_predictions,
      file.path(data_directory, "mlcm_mortality_placebo_predictions.csv"),
      row.names = FALSE
    )

    ggsave(
      file.path(image_directory, "mlcm-local-mortality-simulation.png"),
      mlcm_figure,
      width = 12,
      height = 12.2,
      dpi = 320,
      bg = "white"
    )
  }

  if (show_plot) {
    print(mlcm_figure)
  }

  list(
    panel = panel,
    municipality_estimates = municipality_estimates,
    area_estimates = area_estimates,
    overall_estimates = overall_estimates,
    validation_results = validation_results,
    validation_predictions = validation$validation_predictions,
    selected_method = selected_method,
    figure = mlcm_figure
  )
}


# 8. Run the script directly ------------------------------------------------

# When this file is run directly, it reproduces the MLCM outputs and prints the
# main tables. When it is sourced by another file, this block is skipped.
if (sys.nframe() == 0L) {
  mlcm_results <- run_mlcm_simulation(seed = 123L, show_plot = TRUE)

  cat("\nMLCM validation results\n")
  print(mlcm_results$validation_results, row.names = FALSE, digits = 3)

  cat("\nSelected MLCM algorithm:", mlcm_results$selected_method, "\n")

  cat("\nMLCM overall estimates\n")
  print(mlcm_results$overall_estimates, row.names = FALSE, digits = 3)

  cat("\nMLCM area estimates\n")
  print(mlcm_results$area_estimates, row.names = FALSE, digits = 3)
}
