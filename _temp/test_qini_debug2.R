# Debug margot structure
library(margot)
set.seed(12345)

# generate test data
n <- 200
p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)

test_data <- data.frame(Y = Y, W = W, X)
covariates <- as.data.frame(X)
names(covariates) <- paste0("X", 1:5)

# run margot_causal_forest
cf_results <- margot_causal_forest(
  data = test_data,
  outcome_vars = "Y",
  covariates = covariates,
  W = W,
  weights = NULL,
  use_train_test_split = TRUE,
  train_proportion = 0.5,
  seed = 12345,
  save_data = TRUE,
  save_models = TRUE,
  verbose = FALSE
)

# check structure
cat("Names in cf_results:\n")
print(names(cf_results))

cat("\nNames in cf_results$results:\n")
print(names(cf_results$results))

if (!is.null(cf_results$full_models)) {
  cat("\nNames in cf_results$full_models:\n")
  print(names(cf_results$full_models))
}

# check model storage
model_name <- "model_Y"
cat("\n\nChecking model storage for", model_name, ":\n")

if (!is.null(cf_results$results[[model_name]])) {
  cat("- results[[model_name]] exists\n")
  if (!is.null(cf_results$results[[model_name]]$model)) {
    cat("- results[[model_name]]$model exists\n")
  } else {
    cat("- results[[model_name]]$model is NULL\n")
  }
}

if (!is.null(cf_results$full_models)) {
  if (!is.null(cf_results$full_models[[model_name]])) {
    cat("- full_models[[model_name]] exists\n")
  } else {
    cat("- full_models[[model_name]] is NULL\n")
  }
}

# check split info
if (!is.null(cf_results$results[[model_name]]$split_info)) {
  cat("\nsplit_info found:\n")
  print(names(cf_results$results[[model_name]]$split_info))
  cat("use_train_test_split:", cf_results$results[[model_name]]$split_info$use_train_test_split, "\n")
  cat("test indices length:", length(cf_results$results[[model_name]]$split_info$test_indices), "\n")
}

# check tau_hat
if (!is.null(cf_results$results[[model_name]]$tau_hat)) {
  cat("\ntau_hat found, length:", length(cf_results$results[[model_name]]$tau_hat), "\n")
}

# check if full_models is a list
cat("\nType of full_models:", typeof(cf_results$full_models), "\n")
cat("Length of full_models:", length(cf_results$full_models), "\n")

# see what's in the first element
if (length(cf_results$full_models) > 0) {
  cat("First element name:", names(cf_results$full_models)[1], "\n")
  cat("First element class:", class(cf_results$full_models[[1]]), "\n")
}