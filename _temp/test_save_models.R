# Test save_models functionality
library(margot)
library(grf)

# simple test data
set.seed(123)
n <- 100
X <- matrix(rnorm(n * 3), n, 3)
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)

test_data <- data.frame(Y = Y, W = W)
covariates <- data.frame(X1 = X[, 1], X2 = X[, 2], X3 = X[, 3])

# run with save_models = TRUE
cat("Running margot_causal_forest with save_models = TRUE...\n")
result <- margot_causal_forest(
  data = test_data,
  outcome_vars = "Y",
  covariates = covariates,
  W = W,
  weights = NULL,
  save_models = TRUE,
  save_data = TRUE,
  verbose = TRUE
)

# check what was saved
cat("\n\nChecking saved components:\n")
cat("- results exists:", !is.null(result$results), "\n")
cat("- full_models exists:", !is.null(result$full_models), "\n")
cat("- data exists:", !is.null(result$data), "\n")

if (!is.null(result$full_models)) {
  cat("\nfull_models structure:\n")
  cat("- Type:", typeof(result$full_models), "\n")
  cat("- Length:", length(result$full_models), "\n")
  cat("- Names:", paste(names(result$full_models), collapse = ", "), "\n")
  
  if (length(result$full_models) > 0) {
    cat("\nFirst model:\n")
    cat("- Name:", names(result$full_models)[1], "\n")
    cat("- Class:", class(result$full_models[[1]]), "\n")
    cat("- Has Y.orig:", !is.null(result$full_models[[1]]$Y.orig), "\n")
    cat("- Has W.orig:", !is.null(result$full_models[[1]]$W.orig), "\n")
    cat("- Has X.orig:", !is.null(result$full_models[[1]]$X.orig), "\n")
  }
}

# check if models in results
model_name <- "model_Y"
if (model_name %in% names(result$results)) {
  cat("\n\nChecking results$", model_name, ":\n", sep = "")
  cat("- Has model:", !is.null(result$results[[model_name]]$model), "\n")
  cat("- Has tau_hat:", !is.null(result$results[[model_name]]$tau_hat), "\n")
  cat("- tau_hat length:", length(result$results[[model_name]]$tau_hat), "\n")
}