# Example: Different Baseline Methods for QINI Curves
# This example demonstrates how to use different baseline methods when plotting QINI curves

library(margot)

# Assuming you have results from margot_causal_forest()
# mc_result <- margot_causal_forest(...)

# Method 1: Auto (default) - tries maq with constant rewards, falls back to straight line
plot_auto <- margot_plot_qini(
  mc_result, 
  outcome_var = "model_t2_anxiety_z",
  baseline_method = "auto"  # default
)

# Method 2: Always use simple baseline - straight line from (0,0) to (1, mean(tau_hat))
# This represents the expected gain under random allocation and always succeeds
plot_simple <- margot_plot_qini(
  mc_result, 
  outcome_var = "model_t2_anxiety_z",
  baseline_method = "simple"
)

# Method 3: Use maq with target.with.covariates = FALSE
# This follows maq's approach for no-covariate targeting
plot_maq_no_cov <- margot_plot_qini(
  mc_result, 
  outcome_var = "model_t2_anxiety_z",
  baseline_method = "maq_no_covariates"
)

# Method 4: No baseline curve (only show CATE curve)
plot_no_baseline <- margot_plot_qini(
  mc_result, 
  outcome_var = "model_t2_anxiety_z",
  baseline_method = "none"
)

# Batch processing with custom baseline method
qini_plots <- margot_plot_qini_batch(
  mc_result,
  model_names = c("t2_anxiety_z", "t2_depression_z"),
  baseline_method = "simple",  # use simple baseline for all plots
  show_ci = TRUE
)

# When to use each method:
# - "auto": Good default, tries maq first then falls back to simple baseline
# - "simple": When you want a guaranteed-to-work baseline (p * E[tau])
# - "maq_no_covariates": When you want maq's approach without covariate adjustment (may fail)
# - "maq_only": When you want standard maq with constant rewards (may fail)
# - "none": When you only want to show the CATE curve without comparison

# Note: If mc_result$data is NULL (common when loading saved results), the function
# will try to add a simple baseline to existing QINI data by extracting the ATE
# from various sources (tau_hat, ATE field, custom_table, etc.)

# Example of handling data-less results:
# If your cf_result has no data but you want simple baselines:
qini_plots_no_data <- margot_plot_qini_batch(
  cf_result_without_data,
  baseline_method = "simple",  # will add baseline to existing CATE curves
  show_ci = FALSE  # CI requires regeneration, which needs data
)