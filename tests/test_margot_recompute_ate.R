# test script for margot_recompute_ate functions
# this is a basic functionality test, not a formal unit test

library(margot)
library(grf)

# create some example data
set.seed(123)
n <- 1000
p <- 10

# covariates
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", 1:p)

# treatment assignment (binary)
W <- rbinom(n, 1, 0.5)

# outcomes with heterogeneous treatment effect
tau <- pmax(X[, 1], 0)
Y1 <- X[, 1] + X[, 2] + tau + rnorm(n)
Y2 <- X[, 3] - X[, 4] + 0.5 * tau + rnorm(n)

# create data frame
data <- data.frame(
  outcome1 = Y1,
  outcome2 = Y2
)

# test 1: basic functionality
cat("Test 1: Basic functionality\n")
cat("==========================\n")

# fit causal forest with saved models
cf_results <- margot_causal_forest(
  data = data,
  outcome_vars = c("outcome1", "outcome2"),
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  save_data = TRUE,
  verbose = TRUE
)

cat("\nOriginal combined table:\n")
print(cf_results$combined_table)

cat("\nChecking top-level structure:\n")
print(names(cf_results))

cat("\nChecking saved models:\n")
print(names(cf_results$full_models))
if (is.null(cf_results$full_models)) {
  cat("full_models is NULL\n")
} else if (length(cf_results$full_models) == 0) {
  cat("full_models is empty list\n")
} else {
  cat("full_models has", length(cf_results$full_models), "elements\n")
}

cat("\nChecking results structure:\n")
print(names(cf_results$results))

if (length(cf_results$results) > 0) {
  cat("\nChecking first result structure:\n")
  result_names <- names(cf_results$results[[1]])
  print(result_names)

  # check if model field exists
  if ("model" %in% result_names) {
    cat("\nModel field exists in results\n")
  }

  # Check for other model-related fields
  model_fields <- grep("model|tree|blp", result_names, value = TRUE)
  if (length(model_fields) > 0) {
    cat("\nModel-related fields found:", paste(model_fields, collapse = ", "), "\n")
  }
}

# test 2: recompute with different target samples
cat("\n\nTest 2: Recompute with different target samples\n")
cat("===============================================\n")

# overlap sample
ate_overlap <- margot_recompute_ate(
  cf_results,
  target.sample = "overlap"
)
cat("\nOverlap sample ATE:\n")
print(ate_overlap$combined_table)

# treated sample
ate_treated <- margot_recompute_ate(
  cf_results,
  target.sample = "treated"
)
cat("\nTreated sample ATE (ATT):\n")
print(ate_treated$combined_table)

# control sample
ate_control <- margot_recompute_ate(
  cf_results,
  target.sample = "control"
)
cat("\nControl sample ATE (ATC):\n")
print(ate_control$combined_table)

# test 3: batch recomputation
cat("\n\nTest 3: Batch recomputation\n")
cat("===========================\n")

batch_results <- margot_recompute_ate_batch(
  cf_results,
  target.samples = c("all", "treated", "control", "overlap")
)

cat("\nComparison table:\n")
print(batch_results$comparison_table)

# test 4: check metadata
cat("\n\nTest 4: Check metadata\n")
cat("======================\n")
cat("ATE parameters stored:\n")
print(ate_overlap$ate_params)

cat("\n\nAll tests completed successfully!\n")
