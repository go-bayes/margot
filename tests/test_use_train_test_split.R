# Test script for use_train_test_split functionality
library(margot)
library(testthat)

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

# test 1: Default behavior (use_train_test_split = FALSE)
cat("Test 1: Default behavior (use_train_test_split = FALSE)\n")
results_default <- margot_causal_forest(
  data = df,
  outcome_vars = c("Y1", "Y2"),
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  save_data = TRUE,
  verbose = FALSE
)

# check output structure
test_that("Default output structure is correct", {
  expect_true("combined_table" %in% names(results_default))
  expect_true("results" %in% names(results_default))
  expect_true("model_Y1" %in% names(results_default$results))
  expect_true("ate" %in% names(results_default$results$model_Y1))
  expect_true("custom_table" %in% names(results_default$results$model_Y1))
  expect_false("split_info" %in% names(results_default$results$model_Y1))
})

cat("✓ Default behavior test passed\n\n")

# test 2: With train/test split (use_train_test_split = TRUE)
cat("Test 2: With train/test split (use_train_test_split = TRUE)\n")
results_split <- margot_causal_forest(
  data = df,
  outcome_vars = c("Y1", "Y2"),
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  save_data = TRUE,
  use_train_test_split = TRUE,
  train_proportion = 0.6,
  verbose = FALSE
)

# check split info exists
test_that("Split info is created when use_train_test_split = TRUE", {
  expect_true("split_info" %in% names(results_split$results$model_Y1))
  expect_true("train_indices" %in% names(results_split$results$model_Y1$split_info))
  expect_true("test_indices" %in% names(results_split$results$model_Y1$split_info))
  expect_true("ate_test_set" %in% names(results_split$results$model_Y1$split_info))
  expect_true("custom_table_test_set" %in% names(results_split$results$model_Y1$split_info))
})

# check main output structure remains the same
test_that("Main output structure unchanged with use_train_test_split = TRUE", {
  expect_true("combined_table" %in% names(results_split))
  expect_true("ate" %in% names(results_split$results$model_Y1))
  expect_true("custom_table" %in% names(results_split$results$model_Y1))
  expect_true("E_Val_bound" %in% names(results_split$results$model_Y1$custom_table))
})

# check train/test split sizes
test_that("Train/test split sizes are correct", {
  n_complete <- sum(complete.cases(X))
  expected_train_size <- floor(0.6 * n_complete)
  actual_train_size <- length(results_split$results$model_Y1$split_info$train_indices)
  expect_equal(actual_train_size, expected_train_size)
  
  actual_test_size <- length(results_split$results$model_Y1$split_info$test_indices)
  expect_equal(actual_train_size + actual_test_size, n_complete)
})

cat("✓ Train/test split test passed\n\n")

# test 3: margot_recompute_ate with split info
cat("Test 3: margot_recompute_ate respects train/test split\n")
ate_overlap <- margot_recompute_ate(
  results_split,
  target_sample = "overlap",
  respect_train_test_split = TRUE
)

test_that("margot_recompute_ate preserves split_info", {
  expect_true("split_info" %in% names(ate_overlap$results$model_Y1))
  expect_true("ate_test_set" %in% names(ate_overlap$results$model_Y1$split_info))
})

cat("✓ margot_recompute_ate test passed\n\n")

# test 4: Compare ATEs
cat("Test 4: Compare ATEs between all data and test set\n")
ate_all_Y1 <- results_split$results$model_Y1$ate[1]
ate_test_Y1 <- results_split$results$model_Y1$split_info$ate_test_set[1]

cat(sprintf("Y1 - ATE (all data): %.3f\n", ate_all_Y1))
cat(sprintf("Y1 - ATE (test set): %.3f\n", ate_test_Y1))
cat(sprintf("Difference: %.3f\n\n", abs(ate_all_Y1 - ate_test_Y1)))

# Summary
cat("All tests passed! Summary:\n")
cat("- Default behavior preserved (backward compatibility ✓)\n")
cat("- Train/test split functionality working (✓)\n")
cat("- Split info stored correctly (✓)\n") 
cat("- Main output format unchanged (✓)\n")
cat("- margot_recompute_ate respects splits (✓)\n")