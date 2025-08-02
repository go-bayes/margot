#' Example: Using compute_heterogeneity parameter in margot_causal_forest
#'
#' This example demonstrates how to use the compute_heterogeneity parameter
#' to speed up computation when only average treatment effects are needed.

library(margot)
library(cli)

# simulate some data
set.seed(123)
n <- 1000

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

cli_h1("Example 1: Full analysis (default)")
cli_alert_info("Computing ATE with heterogeneity metrics (QINI, RATE, policy trees)")

system.time({
  results_full <- margot_causal_forest(
    data = data,
    outcome_vars = c("anxiety_z", "depression_z"),
    covariates = as.matrix(X),
    W = W,
    weights = NULL,
    compute_heterogeneity = TRUE,  # default - computes everything
    verbose = TRUE
  )
})

cli_alert_success("Full analysis complete")
cli_alert_info("Results contain QINI data: {!is.null(results_full$results$model_anxiety_z$qini_data)}")

cli_h1("Example 2: ATE only (faster)")
cli_alert_info("Computing only average treatment effects")

system.time({
  results_ate_only <- margot_causal_forest(
    data = data,
    outcome_vars = c("anxiety_z", "depression_z"),
    covariates = as.matrix(X),
    W = W,
    weights = NULL,
    compute_heterogeneity = FALSE,  # skip heterogeneity computations
    verbose = TRUE
  )
})

cli_alert_success("ATE-only analysis complete")
cli_alert_info("Results contain empty QINI data: {nrow(results_ate_only$results$model_anxiety_z$qini_data) == 0}")
cli_alert_info("QINI metadata shows: computed = {results_ate_only$results$model_anxiety_z$qini_metadata$computed}")

# compare ATEs (should be identical)
cli_h2("ATE Comparison")
ate_full <- results_full$results$model_anxiety_z$ate["estimate"]
ate_fast <- results_ate_only$results$model_anxiety_z$ate["estimate"]
cli_alert_info("Full analysis ATE: {round(ate_full, 4)}")
cli_alert_info("Fast analysis ATE: {round(ate_fast, 4)}")
cli_alert_info("ATEs are identical: {abs(ate_full - ate_fast) < 1e-10}")

# check computation status
cli_h2("Computation Status")
cli_alert_info("Full analysis computed heterogeneity: {results_full$computation_status$heterogeneity_computed}")
cli_alert_info("Fast analysis computed heterogeneity: {results_ate_only$computation_status$heterogeneity_computed}")

# downstream compatibility
cli_h2("Pipeline Compatibility")
tryCatch({
  # this would normally fail with NULL, but now gives informative error
  margot_plot_qini(results_ate_only, "anxiety_z")
}, error = function(e) {
  cli_alert_warning("Expected error when trying to plot non-existent QINI data")
  cli_alert_info("Error message would guide user to rerun with compute_heterogeneity = TRUE")
})