# Simple test for use_train_test_split functionality
library(margot)

# simulate test data
set.seed(2025)
n <- 500
X <- matrix(rnorm(n * 5), n, 5)
colnames(X) <- paste0("X", 1:5)
W <- rbinom(n, 1, 0.5)
Y1 <- X[,1] + 0.5 * W + rnorm(n)
Y2 <- X[,2] - 0.3 * W + rnorm(n)

df <- data.frame(
  Y1 = Y1,
  Y2 = Y2,
  W = W
)

cat("Running margot_causal_forest with use_train_test_split = TRUE\n")
results <- margot_causal_forest(
  data = df,
  outcome_vars = c("Y1", "Y2"),
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  save_data = TRUE,
  use_train_test_split = TRUE,
  train_proportion = 0.6,
  verbose = TRUE
)

cat("\n=== Results Summary ===\n")
cat("\nCombined table (main results on all data):\n")
print(results$combined_table)

cat("\nY1 - split_info present:", "split_info" %in% names(results$results$model_Y1), "\n")
if ("split_info" %in% names(results$results$model_Y1)) {
  split_info <- results$results$model_Y1$split_info
  cat("  - Train size:", length(split_info$train_indices), "\n")
  cat("  - Test size:", length(split_info$test_indices), "\n")
  cat("  - ATE (all data):", results$results$model_Y1$ate[1], "\n")
  cat("  - ATE (test set):", split_info$ate_test_set[1], "\n")
  cat("\nTest set E-value table:\n")
  print(split_info$custom_table_test_set)
}