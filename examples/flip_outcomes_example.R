#' Example: Using flip_outcomes in margot_causal_forest
#'
#' This example demonstrates how to use the flip_outcomes parameter
#' to reverse-score outcomes in causal forest analysis.

library(margot)

# Simulated data (included in package)
data <- margot::margot_sim_data

# Select a subset of outcomes
outcomes <- c("t2_anxiety_z", "t2_depression_z")

# Basic flipping with z-score method (default)
results_flipped <- margot_causal_forest(
  data = data,
  outcome_vars = outcomes,
  covariates = data[, c("age", "gender", "income")],
  W = data$treatment,
  weights = data$weights,
  flip_outcomes = outcomes,  # flip both outcomes
  verbose = TRUE
)

# Advanced: Different methods for different outcomes
results_custom <- margot_causal_forest(
  data = data,
  outcome_vars = outcomes,
  covariates = data[, c("age", "gender", "income")],
  W = data$treatment,
  weights = data$weights,
  flip_outcomes = list(
    t2_anxiety_z = list(method = "zscore"),
    t2_depression_z = list(method = "ordinal", scale_bounds = c(0, 10))
  ),
  verbose = TRUE
)

# The results now contain flipped outcomes with "_r" suffix
# model_t2_anxiety_z_r and model_t2_depression_z_r

# Compare original vs flipped
print(results_flipped$combined_table)