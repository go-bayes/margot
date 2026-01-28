# Test to verify that different treatment costs produce different gain scales

# Test data
set.seed(123)
n <- 100
tau_hat <- runif(n, 0.1, 0.5)  # treatment effects
Y <- rnorm(n)
W <- rbinom(n, 1, 0.5)

# Compute IPW scores
IPW_scores <- matrix(Y, ncol = 1)  # simplified for testing

# Test with cost = 1
qini_cost1 <- maq::maq(
  reward = matrix(tau_hat, ncol = 1),
  cost = matrix(1, n, 1),
  DR.scores = IPW_scores,
  R = 100
)

# Test with cost = 0.5 (cheaper treatment)
qini_cost05 <- maq::maq(
  reward = matrix(tau_hat, ncol = 1),
  cost = matrix(0.5, n, 1),
  DR.scores = IPW_scores,
  R = 100
)

# Extract gains at 20% spend
gain1 <- maq::average_gain(qini_cost1, spend = 0.2)
gain05 <- maq::average_gain(qini_cost05, spend = 0.2)

cat("Gain at cost=1: ", gain1$estimate, "\n")
cat("Gain at cost=0.5: ", gain05$estimate, "\n")
cat("Ratio (should be ~2): ", gain05$estimate / gain1$estimate, "\n")

# Check the path gains
cat("\nPath gains at cost=1 (first 5):\n")
print(head(qini_cost1[["_path"]]$gain, 5))

cat("\nPath gains at cost=0.5 (first 5):\n") 
print(head(qini_cost05[["_path"]]$gain, 5))