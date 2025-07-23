# Debug script to test ATE line straightness

library(margot)

# Create a simple test case
set.seed(123)
n <- 1000
X <- matrix(rnorm(n * 5), n, 5)
W <- rbinom(n, 1, 0.5)
Y <- 2 + 0.5 * W + rnorm(n)  # True ATE = 0.5

# Create simple data frame
df_test <- data.frame(
  outcome = Y,
  W = W
)

# Run margot_causal_forest
test_model <- margot_causal_forest(
  data = df_test,
  outcome_vars = "outcome",
  covariates = X,
  W = W,
  weights = rep(1, n),
  save_data = TRUE,
  save_models = TRUE,
  verbose = TRUE
)

# Extract qini data
qini_data <- test_model$results$model_outcome$qini_data

# Check if ATE line is straight
ate_data <- qini_data[qini_data$curve == "ate", ]
if (nrow(ate_data) > 0) {
  # Calculate what a straight line should look like
  ate_value <- tail(ate_data$gain, 1)
  expected_gain <- ate_data$proportion * ate_value
  
  # Compare actual vs expected
  diff <- abs(ate_data$gain - expected_gain)
  max_diff <- max(diff)
  
  cat("ATE final value:", ate_value, "\n")
  cat("Maximum deviation from straight line:", max_diff, "\n")
  cat("Relative deviation:", max_diff / abs(ate_value) * 100, "%\n")
  
  # Plot to visualize
  plot(ate_data$proportion, ate_data$gain, type = "l", col = "red", lwd = 2,
       main = "ATE Qini Curve", xlab = "Proportion", ylab = "Gain")
  lines(ate_data$proportion, expected_gain, col = "blue", lty = 2, lwd = 2)
  legend("topleft", c("Actual", "Expected (straight)"), col = c("red", "blue"), lty = c(1, 2))
  
  # Print first few rows
  cat("\nFirst 10 rows of ATE data:\n")
  print(head(ate_data, 10))
}

# Also check the qini objects directly
qini_obj <- test_model$results$model_outcome$qini_objects$ate
if (!is.null(qini_obj)) {
  cat("\nQini object path length:", length(qini_obj[["_path"]]$gain), "\n")
  cat("First 10 gain values from maq:\n")
  print(head(qini_obj[["_path"]]$gain, 10))
}