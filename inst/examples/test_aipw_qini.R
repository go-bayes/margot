# Example: Testing AIPW QINI curves
# This script demonstrates how to use margot_recompute_qini_aipw()

library(margot)

# Create simulated data for testing
set.seed(42)
n <- 2000

# Covariates
X <- matrix(rnorm(n * 5), n, 5)
colnames(X) <- paste0("X", 1:5)

# Treatment assignment (binary)
propensity <- plogis(0.5 * X[,1] - 0.3 * X[,2])
W <- rbinom(n, 1, propensity)

# Outcome with heterogeneous treatment effect
tau <- 0.5 + 0.3 * X[,1] - 0.2 * X[,2]  # True CATE
Y <- 2 + X[,1] + 0.5 * X[,2] + tau * W + rnorm(n)

# Create data frame
data <- data.frame(
  id = 1:n,
  Y = Y,
  W = W,
  X
)

# Fit causal forest with margot
cat("Fitting causal forest...\n")
cf_results <- margot_causal_forest(
  data = data,
  outcome = "Y",
  treatment = "W",
  covariates = c("X1", "X2", "X3", "X4", "X5"),
  save_data = TRUE,
  save_models = TRUE,
  verbose = TRUE
)

# Compare IPW and AIPW QINI curves
cat("\n\nOriginal QINI (IPW):\n")
print(cf_results$results$model_Y$qini_objects$cate[["_path"]]$gain[1:10])

cat("\n\nRecomputing with AIPW...\n")
cf_results_aipw <- margot_recompute_qini_aipw(
  cf_results,
  method = "regression_forest",
  verbose = TRUE
)

cat("\n\nNew QINI (AIPW):\n")
print(cf_results_aipw$results$model_Y$qini_objects$cate[["_path"]]$gain[1:10])

# Plot comparison (if in interactive session)
if (interactive()) {
  # Original IPW plot
  p1 <- margot_plot_qini(cf_results, "model_Y") + 
    labs(title = "QINI Curves with IPW Scores")
  
  # AIPW plot
  p2 <- margot_plot_qini(cf_results_aipw, "model_Y") + 
    labs(title = "QINI Curves with AIPW Scores")
  
  # Show side by side
  library(patchwork)
  p1 + p2
}

cat("\n\nTest completed successfully!\n")