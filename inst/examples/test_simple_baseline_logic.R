# Test the simple baseline logic
# This tests the scenario where we have existing QINI data without baseline
# and want to add a simple baseline

library(margot)

# Create mock data structure similar to what margot_plot_qini expects
mock_result <- list(
  results = list(
    model_test = list(
      # Existing QINI data with only CATE curve
      qini_data = data.frame(
        proportion = seq(0, 1, length.out = 100),
        gain = cumsum(rnorm(100, mean = 0.02, sd = 0.01)),
        curve = "cate"
      ),
      # Custom table with ATE estimate
      custom_table = data.frame(
        Estimate = c(0.05, 0.1, 0.02),
        row.names = c("E[Y(1)]", "E[Y(0)]", "E[Y(1)]-E[Y(0)]")
      )
    )
  ),
  data = NULL,  # No data available for regeneration
  W = NULL
)

# Try to plot with simple baseline
tryCatch({
  plot <- margot_plot_qini(
    mock_result,
    outcome_var = "model_test",
    baseline_method = "simple",
    show_ci = FALSE
  )
  
  # Check if both curves are present
  curves_present <- unique(plot$data$curve)
  cat("Curves in plot data:", paste(curves_present, collapse = ", "), "\n")
  
  # Verify the ATE curve is a straight line
  if ("ATE" %in% curves_present) {
    ate_data <- plot$data[plot$data$curve == "ATE", ]
    # Check if gain is proportional to proportion
    expected_gain <- ate_data$proportion * 0.02  # ATE from custom_table
    max_diff <- max(abs(ate_data$gain - expected_gain))
    cat("Maximum deviation from straight line:", max_diff, "\n")
    if (max_diff < 0.001) {
      cat("SUCCESS: ATE curve is a straight line!\n")
    }
  }
  
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})