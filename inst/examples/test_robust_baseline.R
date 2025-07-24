# Test script for robust baseline implementation
# This demonstrates the new baseline_method options

library(margot)

# Test with existing data that has QINI curves
# Assuming cf.test is already loaded

# Test 1: Use simple baseline (always works)
cat("Test 1: Simple baseline\n")
test_simple <- margot_plot_qini_batch(
  cf.test,
  baseline_method = "simple", 
  ylim = c(-.1, .2),
  show_ci = TRUE,
  save_plots = FALSE
)

# Test 2: Use auto mode (tries maq, falls back to simple)
cat("\nTest 2: Auto baseline\n")
test_auto <- margot_plot_qini_batch(
  cf.test,
  baseline_method = "auto",
  ylim = c(-.1, .2), 
  show_ci = TRUE,
  save_plots = FALSE
)

# Test 3: Test with margot_policy using simple baseline
cat("\nTest 3: Policy with simple baseline\n")
qini_results <- margot_policy(
  cf.test,
  save_plots = FALSE,
  baseline_method = "simple",
  output_objects = c("qini_plot", "diff_gain_summaries")
)

# Check if diff_gain_summaries work with simple baselines
if (!is.null(qini_results[[1]]$diff_gain_summaries)) {
  cat("diff_gain_summaries successfully computed with simple baseline!\n")
  print(qini_results[[1]]$diff_gain_summaries$spend_0.2$summary)
}

# Test 4: Compare different baseline methods for one model
model_name <- names(cf.test$results)[1]
cat("\nTest 4: Comparing baseline methods for", model_name, "\n")

# Simple baseline
plot_simple <- margot_plot_qini(
  cf.test,
  outcome_var = model_name,
  baseline_method = "simple",
  show_ci = FALSE
) + ggplot2::ggtitle("Simple Baseline")

# Auto baseline  
plot_auto <- margot_plot_qini(
  cf.test,
  outcome_var = model_name,
  baseline_method = "auto",
  show_ci = FALSE
) + ggplot2::ggtitle("Auto Baseline")

# No baseline
plot_none <- margot_plot_qini(
  cf.test,
  outcome_var = model_name,
  baseline_method = "none",
  show_ci = FALSE
) + ggplot2::ggtitle("No Baseline")

# Display plots
library(patchwork)
combined_plot <- plot_simple + plot_auto + plot_none
print(combined_plot)

cat("\nAll tests completed successfully!\n")