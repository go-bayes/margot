# Test script for treatment cost functionality
# This demonstrates how QINI curves change with different treatment costs

library(margot)
library(grf)
library(ggplot2)

# create synthetic data
set.seed(123)
n <- 2000
p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)

# create data frame
data <- data.frame(outcome = Y)

cat("Testing treatment cost parameter in margot functions\n")
cat("===================================================\n\n")

# test 1: margot_causal_forest with different treatment costs
cat("Test 1: Running margot_causal_forest with different qini_treatment_costs\n")

# low cost (cheap treatment)
results_low_cost <- margot_causal_forest(
  data = data,
  outcome_vars = "outcome",
  covariates = X,
  W = W,
  weights = NULL,
  qini_split = TRUE,
  train_prop = 0.5,
  qini_treatment_cost = 0.2,
  verbose = TRUE
)

# medium cost (default)
results_med_cost <- margot_causal_forest(
  data = data,
  outcome_vars = "outcome",
  covariates = X,
  W = W,
  weights = NULL,
  qini_split = TRUE,
  train_prop = 0.5,
  qini_treatment_cost = 1,
  verbose = TRUE
)

# high cost (expensive treatment)
results_high_cost <- margot_causal_forest(
  data = data,
  outcome_vars = "outcome",
  covariates = X,
  W = W,
  weights = NULL,
  qini_split = TRUE,
  train_prop = 0.5,
  qini_treatment_cost = 5,
  verbose = TRUE
)

cat("\n\nTest 2: Using margot_qini with different treatment costs\n")

# test margot_qini with different costs
qini_low <- margot_qini(results_med_cost, treatment_cost = 0.2)
qini_med <- margot_qini(results_med_cost, treatment_cost = 1)
qini_high <- margot_qini(results_med_cost, treatment_cost = 5)

cat("\n\nTest 3: Comparing QINI curve shapes\n")

# extract qini data for plotting
get_qini_data <- function(qini_result, cost_label) {
  df <- qini_result$model_outcome$qini_data
  df$cost <- cost_label
  df
}

# combine data
plot_data <- rbind(
  get_qini_data(qini_low, "Low cost (0.2)"),
  get_qini_data(qini_med, "Medium cost (1)"),
  get_qini_data(qini_high, "High cost (5)")
)

# create plot showing how cost affects curve shape
p <- ggplot(plot_data[plot_data$curve == "cate",], 
            aes(x = proportion, y = gain, color = cost)) +
  geom_line(size = 1.2) +
  labs(
    title = "QINI Curves with Different Treatment Costs",
    subtitle = "Lower costs create steeper curves (more treated), higher costs create shallower curves",
    x = "Proportion of Population",
    y = "Gain",
    color = "Treatment Cost"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p)

cat("\n\nKey insights:\n")
cat("1. With low cost (0.2): Treatment is cheap, so more people can be treated cost-effectively\n")
cat("2. With medium cost (1): Balanced scenario - this is the default\n")
cat("3. With high cost (5): Treatment is expensive, so only highest-effect individuals justify treatment\n")
cat("\nThe x-axis represents budget spent (not just population treated), which is why curves differ\n")