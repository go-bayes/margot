# Example: Using improved Qini analysis functions in margot 1.0.80

# Load package
library(margot)

# Assume you have results from margot_batch_policy or similar
# multi_batch <- margot_batch_policy(model_results, spend = c(0.2, 0.5))

# Example 1: Basic usage with default settings
qini_results <- margot_interpret_qini(multi_batch)

# View the concise summary - perfect for reports
cat(qini_results$concise_summary)
# Output example:
# "CATE-based targeting improves outcomes for Agreeableness, Conscientiousness, and Neuroticism (reversed). 
#  CATE-based targeting worsens outcomes for Extraversion, Honesty Humility, and Openness. 
#  (Based on Qini curve analysis at 20% and 50% spend levels.)"

# View the full explanation with intro
cat(qini_results$qini_explanation)

# Example 2: Custom spend levels (e.g., 10%, 25%, 50%, 75%)
qini_custom <- margot_interpret_qini(
  multi_batch,
  spend_levels = c(0.1, 0.25, 0.5, 0.75)
)

# Example 3: Without explanatory intro (just results)
qini_brief <- margot_interpret_qini(
  multi_batch,
  include_intro = FALSE
)

# Example 4: Plot with matching spend lines
plot <- margot_plot_qini(
  mc_result, 
  outcome_var = "model_t2_agreeableness_z",
  spend_levels = c(0.2, 0.5),  # matches analysis
  theme = "minimal"  # clean modern look
)

# Example 5: Customize spend line appearance
plot_custom <- margot_plot_qini(
  mc_result,
  outcome_var = "model_t2_agreeableness_z", 
  spend_levels = c(0.1, 0.25, 0.5, 0.75),
  spend_line_color = "darkred",
  spend_line_alpha = 0.3,
  theme = "classic"
)

# Example 6: Use with custom labels
label_mapping <- list(
  "t2_agreeableness_z" = "Agreeableness",
  "t2_neuroticism_z_r" = "Emotional Stability"  # reversed neuroticism
)

qini_labeled <- margot_interpret_qini(
  multi_batch,
  label_mapping = label_mapping
)

plot_labeled <- margot_plot_qini(
  mc_result,
  outcome_var = "model_t2_agreeableness_z",
  label_mapping = label_mapping
)