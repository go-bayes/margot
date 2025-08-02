# Compare results with and without train/test split
library(margot)

# simulate test data
set.seed(2025)
n <- 500
X <- matrix(rnorm(n * 20), n, 20)
colnames(X) <- paste0("X", 1:20)
W <- rbinom(n, 1, 0.5)
Y1 <- X[, 1] + 0.5 * W + rnorm(n)
Y2 <- X[, 2] - 0.3 * W + rnorm(n)

df <- data.frame(Y1 = Y1, Y2 = Y2, W = W)

cat("=== Comparison: use_train_test_split FALSE vs TRUE ===\n\n")

# Run without split
cat("1. Standard approach (use_train_test_split = FALSE):\n")
results_no_split <- margot_causal_forest(
  data = df,
  outcome_vars = c("Y1", "Y2"),
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  verbose = FALSE
)
cat("   Combined table uses ALL DATA:\n")
print(results_no_split$combined_table)

# Run with split
cat("\n2. Train/test split approach (use_train_test_split = TRUE):\n")
results_split <- margot_causal_forest(
  data = df,
  outcome_vars = c("Y1", "Y2"),
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  use_train_test_split = TRUE,
  train_proportion = 0.7,
  verbose = FALSE
)
cat("   Combined table uses TEST SET (30% of data):\n")
print(results_split$combined_table)

cat("\n   All-data results available in split_info:\n")
all_data_results <- do.call(rbind, lapply(results_split$results, function(x) {
  if (!is.null(x$split_info)) x$split_info$custom_table_all_data else NULL
}))
print(all_data_results)

cat("\n=== Key Differences ===\n")
cat("- Without split: Results computed on 100% of data\n")
cat("- With split: Main results on 30% test set, more conservative\n")
cat("- Test set results often have wider CIs due to smaller sample size\n")
cat("- Both approaches valid, choice depends on your goals\n")
