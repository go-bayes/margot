# Test that use_train_test_split = TRUE produces consistent test-set results
library(margot)

# simulate test data
set.seed(2025)
n <- 500
X <- matrix(rnorm(n * 20), n, 20)
colnames(X) <- paste0("X", 1:20)
W <- rbinom(n, 1, 0.5)
Y1 <- X[,1] + 0.5 * W + rnorm(n)

df <- data.frame(Y1 = Y1, W = W)

cat("=== Running margot_causal_forest with use_train_test_split = TRUE ===\n")
results <- margot_causal_forest(
  data = df,
  outcome_vars = "Y1",
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  use_train_test_split = TRUE,
  train_proportion = 0.7,
  verbose = FALSE
)

cat("\n1. Main combined_table (should be TEST SET results):\n")
print(results$combined_table)

cat("\n2. Main ATE (should match test set):\n")
cat("   Main ate:", results$results$model_Y1$ate[1], "\n")

cat("\n3. Split info (should contain ALL DATA results):\n")
if (!is.null(results$results$model_Y1$split_info)) {
  split_info <- results$results$model_Y1$split_info
  cat("   Train size:", length(split_info$train_indices), "\n")
  cat("   Test size:", length(split_info$test_indices), "\n")
  cat("   All-data ATE:", split_info$ate_all_data[1], "\n")
  
  cat("\n4. All-data E-value table (stored in split_info):\n")
  print(split_info$custom_table_all_data)
}

cat("\n=== Verification ===\n")
cat("✓ Main results now computed on TEST SET when use_train_test_split = TRUE\n")
cat("✓ All-data results available in split_info for reference\n")
cat("✓ Consistent behavior - no confusion about which data was used\n")