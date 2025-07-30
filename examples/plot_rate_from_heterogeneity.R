# How to Plot RATE Results from margot_interpret_heterogeneity()
# Using the standard margot_plot_rate() and margot_plot_rate_batch() functions

library(margot)

# Run heterogeneity analysis (with either standard or CV method)
het_evidence <- margot_interpret_heterogeneity(
  models = your_causal_forest_results,
  use_cross_validation = FALSE,  # set to TRUE for CV
  spend_levels = c(0.1, 0.4),
  flipped_outcomes = c("anxiety", "depression"),
  label_mapping = list(
    "t2_charity_donate_z" = "Charity Donations",
    "t2_social_support_z" = "Social Support"
  )
)

# Extract the raw RATE results
rate_results <- het_evidence$rate_results$raw_results

# Now you can use the standard plotting functions!

# 1. Plot individual RATE results (AUTOC and QINI)
plot_autoc <- margot_plot_rate(
  rate_results$rate_autoc,
  target = "AUTOC",
  title = "RATE AUTOC Results"
)

plot_qini <- margot_plot_rate(
  rate_results$rate_qini,
  target = "QINI",
  title = "RATE QINI Results"
)

# 2. Plot both AUTOC and QINI together
plot_batch <- margot_plot_rate_batch(
  rate_results,
  label_mapping = list(
    "t2_charity_donate_z" = "Charity Donations",
    "t2_social_support_z" = "Social Support"
  )
)

# 3. For cross-validation results
# If you used use_cross_validation = TRUE, the structure is the same:
het_evidence_cv <- margot_interpret_heterogeneity(
  models = your_causal_forest_results,
  use_cross_validation = TRUE,
  num_folds = 5,
  alpha = 0.2  # recommended for Bonferroni with CV
)

# The raw results are still available
rate_results_cv <- het_evidence_cv$rate_results$raw_results

# But these will have the CV structure, so you might need to use:
# margot_plot_cv_results() instead, which is specifically designed for CV results
plot_cv <- margot_plot_cv_results(het_evidence_cv$cv_results)

# However, if you want to extract the aggregated CV results in a format
# compatible with margot_plot_rate(), you can access:
if (!is.null(rate_results_cv$rate_autoc)) {
  plot_autoc_cv <- margot_plot_rate(
    rate_results_cv$rate_autoc,
    target = "AUTOC",
    title = "Cross-Validated RATE AUTOC Results"
  )
}

# Save plots
ggsave("rate_autoc_plot.png", plot_autoc, width = 10, height = 8)
ggsave("rate_qini_plot.png", plot_qini, width = 10, height = 8)
ggsave("rate_batch_plot.png", plot_batch, width = 12, height = 8)

# Display plots
print(plot_autoc)
print(plot_qini)
print(plot_batch)

# Combine plots if needed
library(patchwork)
combined_plot <- plot_autoc / plot_qini
print(combined_plot)

# Note: The key is accessing het_evidence$rate_results$raw_results
# This contains the exact same structure as direct margot_rate() output