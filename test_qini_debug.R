# Debug average_gain output
library(maq)
library(grf)

# simple test
set.seed(123)
n <- 100
X <- matrix(rnorm(n * 2), n, 2)
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)

# get IPW scores
Y_mat <- as.matrix(Y)
W_fac <- as.factor(W)
ipw <- maq::get_ipw_scores(Y_mat, W_fac)

# create simple qini
tau_hat <- rnorm(n, mean = 0.5)
q <- maq::maq(
  reward = as.matrix(tau_hat),
  cost = matrix(1, n, 1),
  DR.scores = ipw,
  R = 10
)

# check average_gain output
ag <- maq::average_gain(q, spend = 0.1)
cat("Class of average_gain output:", class(ag), "\n")
cat("Names:", names(ag), "\n")
cat("Structure:\n")
str(ag)

# try to access estimate
if (is.list(ag) && "estimate" %in% names(ag)) {
  cat("\nEstimate:", ag$estimate, "\n")
} else if (is.numeric(ag)) {
  cat("\nNumeric value:", ag, "\n")
} else {
  cat("\nCannot access estimate\n")
}