# Test the new flip functionality in margot_causal_forest

library(margot)
library(cli)

# simulate some data
set.seed(123)
n <- 500

# covariates
X <- data.frame(
  age = rnorm(n, 50, 10),
  gender = rbinom(n, 1, 0.5),
  income = rlnorm(n, 10, 1)
)

# treatment (binary)
W <- rbinom(n, 1, 0.5)

# outcomes - anxiety on 1-7 scale, depression on 0-21 scale
data <- data.frame(
  anxiety = round(3 + 0.5 * X$age/10 - 0.3 * W + rnorm(n, 0, 1), 0),
  depression = round(10 + 0.7 * X$age/10 - 0.5 * W + rnorm(n, 0, 2), 0)
)

# ensure values are within expected bounds
data$anxiety <- pmax(1, pmin(7, data$anxiety))
data$depression <- pmax(0, pmin(21, data$depression))

cli_h1("Testing margot_causal_forest with flip_outcomes")

# test 1: flip using z-score method (default)
cli_h2("Test 1: Z-score flipping")
results_zscore <- margot_causal_forest(
  data = data,
  outcome_vars = c("anxiety", "depression"),
  covariates = as.matrix(X),
  W = W,
  weights = NULL,
  flip_outcomes = c("anxiety", "depression"),
  flip_method = "zscore",
  save_models = TRUE,
  save_data = TRUE,
  verbose = TRUE,
  use_train_test_split = TRUE
)

cli_alert_success("Z-score flipping completed")
cli_alert_info("Results contain: {paste(names(results_zscore$results), collapse = ', ')}")

# test 2: flip using ordinal method with bounds
cli_h2("Test 2: Ordinal scale flipping")
results_ordinal <- margot_causal_forest(
  data = data,
  outcome_vars = c("anxiety", "depression"),
  covariates = as.matrix(X),
  W = W,
  weights = NULL,
  flip_outcomes = list(
    anxiety = list(method = "ordinal", scale_bounds = c(1, 7)),
    depression = list(method = "ordinal", scale_bounds = c(0, 21))
  ),
  save_models = TRUE,
  save_data = TRUE,
  verbose = TRUE,
  use_train_test_split = TRUE
)

cli_alert_success("Ordinal flipping completed")
cli_alert_info("Results contain: {paste(names(results_ordinal$results), collapse = ', ')}")

# test 3: mixed - flip only anxiety
cli_h2("Test 3: Selective flipping")
results_mixed <- margot_causal_forest(
  data = data,
  outcome_vars = c("anxiety", "depression"),
  covariates = as.matrix(X),
  W = W,
  weights = NULL,
  flip_outcomes = "anxiety",
  flip_method = "ordinal",
  flip_scale_bounds = c(1, 7),
  save_models = TRUE,
  save_data = TRUE,
  verbose = TRUE,
  use_train_test_split = TRUE
)

cli_alert_success("Selective flipping completed")
cli_alert_info("Results contain: {paste(names(results_mixed$results), collapse = ', ')}")

# compare ATEs
cli_h2("ATE Comparison")
cli_alert_info("Original anxiety ATE would be negative (treatment reduces anxiety)")
cli_alert_info("Flipped anxiety_r ATE should be positive (treatment increases reversed anxiety)")

# display some results
if ("model_anxiety_r" %in% names(results_ordinal$results)) {
  ate_flipped <- results_ordinal$results$model_anxiety_r$ate
  cli_alert_info("Flipped anxiety_r ATE: {round(ate_flipped['estimate'], 3)}")
}

if ("model_depression_r" %in% names(results_ordinal$results)) {
  ate_flipped <- results_ordinal$results$model_depression_r$ate
  cli_alert_info("Flipped depression_r ATE: {round(ate_flipped['estimate'], 3)}")
}

cli_alert_success("All tests completed successfully!")