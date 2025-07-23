# test updated margot_recompute_ate with new parameter names and column headings

library(margot)
library(grf)

# minimal example to test the updated function
set.seed(123)
n <- 500
p <- 5

X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", 1:p)
W <- rbinom(n, 1, 0.5)
Y <- X[,1] + 0.5 * W + rnorm(n)

data <- data.frame(outcome1 = Y)

# fit model
cf_results <- margot_causal_forest(
  data = data,
  outcome_vars = "outcome1",
  covariates = X,
  W = W,
  weights = NULL,
  save_models = TRUE,
  save_data = TRUE,  # also save data for completeness
  verbose = TRUE,  # see what's happening
  qini_split = FALSE,  # disable to avoid errors
  compute_rate = FALSE,  # disable to simplify
  compute_conditional_means = FALSE
)

cat("Original table:\n")
print(cf_results$combined_table)

# test new parameter name
ate_overlap <- margot_recompute_ate(
  cf_results,
  target_sample = "overlap"  # new parameter name
)

cat("\n\nOverlap table (should show ATO column):\n")
print(ate_overlap$combined_table)

# test batch with all samples
batch_results <- margot_recompute_ate_batch(
  cf_results,
  target_samples = c("all", "treated", "control", "overlap")
)

cat("\n\nComparison table (should show ATE, ATT, ATC, ATO columns):\n")
print(batch_results$comparison_table)

cat("\n\nTest completed successfully!\n")