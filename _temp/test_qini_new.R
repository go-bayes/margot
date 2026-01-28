# Test script for new QINI implementation
# Following GRF methodology exactly

library(margot)
library(grf)
library(maq)
set.seed(12345)

# generate test data (from GRF example)
n <- 2000
p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)

# create data frame for margot
test_data <- data.frame(
  Y = Y,
  W = W,
  X
)

# 1) GRF approach (reference implementation)
cat("=== GRF Reference Implementation ===\n")

# train/test split
train <- sample(n/2)
test <- -train

# train CATE function
c.forest <- causal_forest(X[train, ], Y[train], W[train])

# predict on test set
tau.hat <- predict(c.forest, X[test, ])$predictions

# compute IPW scores for test set using maq
Y_test <- as.matrix(Y[test])
W_test <- as.factor(W[test])
IPW.scores <- maq::get_ipw_scores(Y_test, W_test)

# compute QINI curves
qini_grf <- maq::maq(
  reward = as.matrix(tau.hat), 
  cost = matrix(1, length(tau.hat), 1), 
  DR.scores = IPW.scores, 
  R = 200, 
  seed = 12345
)
qini_baseline_grf <- maq::maq(
  reward = as.matrix(tau.hat), 
  cost = matrix(1, length(tau.hat), 1), 
  DR.scores = IPW.scores,
  target.with.covariates = FALSE, 
  R = 200, 
  seed = 12345
)

# show gains at key spend levels
cat("\nGRF QINI gains at 10% spend:\n")
ag_cate_10 <- maq::average_gain(qini_grf, spend = 0.1)
ag_baseline_10 <- maq::average_gain(qini_baseline_grf, spend = 0.1)
cat("CATE:", ag_cate_10["estimate"], "\n")
cat("Baseline:", ag_baseline_10["estimate"], "\n")

cat("\nGRF QINI gains at 40% spend:\n")
ag_cate_40 <- maq::average_gain(qini_grf, spend = 0.4)
ag_baseline_40 <- maq::average_gain(qini_baseline_grf, spend = 0.4)
cat("CATE:", ag_cate_40["estimate"], "\n")
cat("Baseline:", ag_baseline_40["estimate"], "\n")

# 2) Margot approach (our implementation)
cat("\n\n=== Margot Implementation ===\n")

# prepare data for margot - ensure column names match
covariates <- as.data.frame(X)
names(covariates) <- paste0("X", 1:5)

# add covariates to test_data
test_data <- cbind(test_data, covariates)

# run margot_causal_forest with train/test split
cf_results <- margot_causal_forest(
  data = test_data,
  outcome_vars = "Y",
  covariates = covariates,
  W = W,
  weights = NULL,
  use_train_test_split = TRUE,
  train_proportion = 0.5,
  seed = 12345,
  save_data = TRUE,
  save_models = TRUE,
  verbose = FALSE
)

# source our new functions
source("/Users/joseph/GIT/margot/R/margot_qini_alternative.R")
source("/Users/joseph/GIT/margot/R/margot_plot_qini_simple.R")

# compute QINI curves with alternative function
cf_results <- margot_qini_alternative(cf_results, seed = 12345)

# extract results from the results structure
qini_margot <- cf_results$results$model_Y

cat("\nMargot QINI gains at 10% spend:\n")
mag_cate_10 <- maq::average_gain(qini_margot$qini_objects$cate, spend = 0.1)
mag_baseline_10 <- maq::average_gain(qini_margot$qini_objects$ate, spend = 0.1)
cat("CATE:", mag_cate_10["estimate"], "\n")
cat("Baseline:", mag_baseline_10["estimate"], "\n")

cat("\nMargot QINI gains at 40% spend:\n")
mag_cate_40 <- maq::average_gain(qini_margot$qini_objects$cate, spend = 0.4)
mag_baseline_40 <- maq::average_gain(qini_margot$qini_objects$ate, spend = 0.4)
cat("CATE:", mag_cate_40["estimate"], "\n")
cat("Baseline:", mag_baseline_40["estimate"], "\n")

# compare test set sizes
cat("\n\nTest set sizes:\n")
cat("GRF:", length(test), "\n")
cat("Margot:", qini_margot$qini_metadata$n_test, "\n")

# plot with new function
plot_qini <- margot_plot_qini_simple(
  cf_results,
  model_name = "model_Y",
  show_ci = TRUE,
  title = "QINI Curves: Margot Implementation",
  subtitle = "Following GRF methodology"
)

print(plot_qini)

# verify gains match
cat("\n\n=== Verification ===\n")
cate_diff_10 <- abs(ag_cate_10["estimate"] - mag_cate_10["estimate"])
cate_diff_40 <- abs(ag_cate_40["estimate"] - mag_cate_40["estimate"])

cat("CATE gain difference at 10%:", cate_diff_10, "\n")
cat("CATE gain difference at 40%:", cate_diff_40, "\n")

if (cate_diff_10 < 0.01 && cate_diff_40 < 0.01) {
  cat("\n✓ Results match GRF implementation!\n")
} else {
  cat("\n✗ Results differ from GRF implementation\n")
}

# also test plotting without CI
plot_qini_no_ci <- margot_plot_qini_simple(
  cf_results,
  model_name = "model_Y",
  show_ci = FALSE
)

# test data extraction
qini_data <- margot_plot_qini_simple(
  cf_results,
  model_name = "model_Y",
  return_data = TRUE
)

cat("\nExtracted QINI data (first 5 rows):\n")
print(head(qini_data, 5))