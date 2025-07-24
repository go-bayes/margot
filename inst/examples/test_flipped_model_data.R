# Test to verify flipped model data handling
# This demonstrates that the current approach correctly uses flipped data

library(margot)
library(cli)

# create a simple test case
set.seed(123)
n <- 100

# original outcome (higher values = worse)
Y_original <- rnorm(n, mean = 5, sd = 2)

# treatment and covariates
W <- rbinom(n, 1, 0.5)
X <- matrix(rnorm(n * 5), ncol = 5)
colnames(X) <- paste0("X", 1:5)

# create data frames
data_original <- data.frame(anxiety = Y_original)
data_flipped <- data.frame(anxiety_r = -Y_original)  # flipped outcome

cli::cli_h1("Testing Flipped Model Data Handling")

# 1. original model
cli::cli_h2("Original Model (anxiety)")
cli::cli_alert_info("Mean outcome: {round(mean(Y_original), 3)}")

results_original <- margot_causal_forest(
  data = data_original,
  outcome_vars = "anxiety",
  covariates = X,
  W = W,
  verbose = FALSE,
  save_data = TRUE
)

# check stored outcome
if (!is.null(results_original$results$model_anxiety$model)) {
  Y_stored_original <- results_original$results$model_anxiety$model$Y.orig
  cli::cli_alert_success("Original model Y.orig mean: {round(mean(Y_stored_original), 3)}")
}

# 2. flipped model (as created by margot_flip_forests)
cli::cli_h2("Flipped Model (anxiety_r)")
cli::cli_alert_info("Mean flipped outcome: {round(mean(-Y_original), 3)}")

results_flipped <- margot_causal_forest(
  data = data_flipped,
  outcome_vars = "anxiety_r",
  covariates = X,
  W = W,
  verbose = FALSE,
  save_data = TRUE
)

# check stored outcome
if (!is.null(results_flipped$results$model_anxiety_r$model)) {
  Y_stored_flipped <- results_flipped$results$model_anxiety_r$model$Y.orig
  cli::cli_alert_success("Flipped model Y.orig mean: {round(mean(Y_stored_flipped), 3)}")
  
  # verify it's the negated data
  cli::cli_alert_info("Y.orig in flipped model equals -Y_original: {all.equal(Y_stored_flipped, -Y_original)}")
}

# 3. test QINI generation for both
cli::cli_h2("QINI Data Generation")

# generate qini for original
qini_original <- margot_generate_qini_data(
  model_result = results_original$results$model_anxiety,
  outcome_data = Y_original,
  treatment = W,
  baseline_method = "simple",
  verbose = TRUE
)

# generate qini for flipped using Y.orig (which is already negated)
qini_flipped <- margot_generate_qini_data(
  model_result = results_flipped$results$model_anxiety_r,
  outcome_data = Y_stored_flipped,  # this is already -Y_original
  treatment = W,
  baseline_method = "simple",
  verbose = TRUE
)

cli::cli_h2("Key Insight")
cli::cli_alert_info("For flipped models created by margot_flip_forests():")
cli::cli_bullet("The outcome is negated ONCE when creating the model")
cli::cli_bullet("Y.orig in the forest object contains the negated values")
cli::cli_bullet("QINI should use Y.orig as-is (no additional negation)")
cli::cli_bullet("This ensures consistency between model training and QINI calculation")

# show treatment effects
cli::cli_h2("Treatment Effect Comparison")
ate_original <- results_original$results$model_anxiety$ATE
ate_flipped <- results_flipped$results$model_anxiety_r$ATE
cli::cli_alert_info("Original model ATE: {round(ate_original, 3)}")
cli::cli_alert_info("Flipped model ATE: {round(ate_flipped, 3)}")
cli::cli_alert_info("Relationship: flipped ≈ -original? {abs(ate_flipped + ate_original) < 0.1}")