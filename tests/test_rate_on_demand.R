# Test script for on-demand RATE computation functionality
# This script tests the new features added to margot_rate, margot_plot_rate, and margot_plot_rate_batch

library(margot)
library(grf)
library(ggplot2)

# create synthetic data for testing
set.seed(123)
n <- 1000
p <- 10

# generate covariates
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("var", 1:p)

# generate treatment
W <- rbinom(n, 1, 0.5)

# generate outcomes with heterogeneous treatment effects
tau <- X[, 1] + X[, 2]
Y1 <- 2 + X[, 1] + tau * W + rnorm(n)
Y2 <- 1 + X[, 2] - 0.5 * tau * W + rnorm(n)

# create data frame
data <- data.frame(
  outcome1 = Y1,
  outcome2 = Y2
)

# run margot_causal_forest with honest evaluation
cat("Running margot_causal_forest...\n")
results <- margot_causal_forest(
  data = data,
  outcome_vars = c("outcome1", "outcome2"),
  covariates = X,
  W = W,
  weights = NULL,
  train_proportion = 0.7,
  save_models = TRUE,
  save_data = TRUE,
  verbose = TRUE
)

cat("\n\nTest 1: margot_rate with custom q grid\n")
cat("=====================================\n")
rate_results <- margot_rate(
  models = results,
  q = seq(0.2, 1, by = 0.2), # custom q grid
  use_evaluation_subset = TRUE
)

cat("\nAUTOC results:\n")
print(rate_results$rate_autoc)
cat("\nQINI results:\n")
print(rate_results$rate_qini)

cat("\n\nTest 2: margot_plot_rate with on-demand computation\n")
cat("==================================================\n")
# test plotting with existing RATE object
p1 <- margot_plot_rate(
  x = results$results$model_outcome1$rate_result,
  outcome_var = "outcome1",
  title = "AUTOC from pre-computed RATE"
)

# test plotting with on-demand computation from forest
p2 <- margot_plot_rate(
  x = results$full_models$model_outcome1,
  outcome_var = "outcome1",
  target = "QINI",
  q = seq(0.1, 1, by = 0.1),
  title = "QINI computed on-demand"
)

cat("\n\nTest 3: margot_plot_rate_batch with target parameter\n")
cat("===================================================\n")
# test batch plotting for AUTOC
plots_autoc <- margot_plot_rate_batch(
  models_binary = results,
  target = "AUTOC"
)

# test batch plotting for QINI
plots_qini <- margot_plot_rate_batch(
  models_binary = results,
  target = "QINI"
)

# test batch plotting with on-demand computation
plots_on_demand <- margot_plot_rate_batch(
  models_binary = results,
  target = "QINI",
  compute_on_demand = TRUE,
  q = seq(0.2, 1, by = 0.2)
)

cat("\n\nTest 4: Validation warnings\n")
cat("==========================\n")
# test with use_evaluation_subset = FALSE to trigger warning
rate_no_validation <- margot_rate(
  models = results,
  use_evaluation_subset = FALSE
)

cat("\n\nAll tests completed successfully!\n")
cat("Summary of new features tested:\n")
cat("- Custom q grid in margot_rate\n")
cat("- On-demand RATE computation in margot_plot_rate\n")
cat("- Target parameter (AUTOC/QINI) in margot_plot_rate_batch\n")
cat("- Validation warnings when not using evaluation subsets\n")
