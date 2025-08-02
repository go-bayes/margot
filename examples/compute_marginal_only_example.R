#' Example: Using compute_marginal_only parameter in margot_causal_forest
#'
#' This example demonstrates the new compute_marginal_only parameter
#' and its interaction with train_proportion.

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

# outcomes
data <- data.frame(
  anxiety_z = rnorm(n, -0.3 * W, 1),
  depression_z = rnorm(n, -0.2 * W, 1)
)

cli_h1("Example 1: Marginal effects only (fast)")
cli_alert_info("Computing only average treatment effects, no heterogeneity")

# Note: train_proportion must be NULL when compute_marginal_only = TRUE
results_marginal <- margot_causal_forest(
  data = data,
  outcome_vars = c("anxiety_z", "depression_z"),
  covariates = as.matrix(X),
  W = W,
  weights = NULL,
  compute_marginal_only = TRUE,
  train_proportion = NULL,  # Required to be NULL
  verbose = TRUE
)

cli_alert_success("Marginal analysis complete")
cli_alert_info("ATE computed: {!is.null(results_marginal$results$model_anxiety_z$ate)}")
cli_alert_info("QINI data empty: {nrow(results_marginal$results$model_anxiety_z$qini_data) == 0}")

cli_h1("Example 2: Full heterogeneity analysis (default)")
cli_alert_info("Computing ATE plus heterogeneity metrics with 50/50 split")

# Note: train_proportion must be specified when compute_marginal_only = FALSE
results_full <- margot_causal_forest(
  data = data,
  outcome_vars = c("anxiety_z", "depression_z"),
  covariates = as.matrix(X),
  W = W,
  weights = NULL,
  compute_marginal_only = FALSE,  # default
  train_proportion = 0.5,          # required when compute_marginal_only = FALSE
  verbose = TRUE
)

cli_alert_success("Full analysis complete")
cli_alert_info("QINI data computed: {!is.null(results_full$results$model_anxiety_z$qini_data) && nrow(results_full$results$model_anxiety_z$qini_data) > 0}")

cli_h1("Example 3: Invalid parameter combinations")
cli_alert_info("These would produce errors:")

# Error: compute_marginal_only = TRUE with train_proportion specified
tryCatch({
  margot_causal_forest(
    data = data,
    outcome_vars = "anxiety_z",
    covariates = as.matrix(X),
    W = W,
    weights = NULL,
    compute_marginal_only = TRUE,
    train_proportion = 0.5  # This causes error!
  )
}, error = function(e) {
  cli_alert_danger("Expected error: {e$message}")
})

# Error: compute_marginal_only = FALSE with train_proportion = NULL
tryCatch({
  margot_causal_forest(
    data = data,
    outcome_vars = "anxiety_z",
    covariates = as.matrix(X),
    W = W,
    weights = NULL,
    compute_marginal_only = FALSE,
    train_proportion = NULL  # This causes error!
  )
}, error = function(e) {
  cli_alert_danger("Expected error: {e$message}")
})

cli_h1("Summary")
cli_alert_info("Use compute_marginal_only = TRUE, train_proportion = NULL for fast ATE-only analysis")
cli_alert_info("Use compute_marginal_only = FALSE, train_proportion = 0.5 for full heterogeneity analysis")