# tests for margot_flip_forests_dev()

test_that("margot_flip_forests_dev works with basic functionality", {
  # create synthetic data
  set.seed(123)
  n <- 500
  p <- 10
  
  # generate covariates
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("X", 1:p)
  X_df <- as.data.frame(X)
  
  # generate treatment
  W <- rbinom(n, 1, 0.5)
  
  # generate outcomes with heterogeneous effects
  tau <- X[, 1] + X[, 2]  # true effect varies with X1 and X2
  Y1 <- X[, 1] + X[, 3] + W * tau + rnorm(n)
  Y2 <- X[, 2] + X[, 4] - W * tau + rnorm(n)  # negative effect
  
  # create data frame
  df <- data.frame(
    Y1 = Y1,
    Y2 = Y2,
    W = W,
    X_df
  )
  
  # run margot_causal_forest
  cf_results <- margot::margot_causal_forest(
    data = df,
    outcome_vars = c("Y1", "Y2"),
    covariates = X_df,
    W = W,
    weights = rep(1, n),
    grf_defaults = list(
      num.trees = 500,
      honesty = TRUE,
      honesty.fraction = 0.5,
      min.node.size = 5,
      alpha = 0.05,
      mtry = ceiling(sqrt(p))
    ),
    save_data = TRUE,
    save_models = TRUE,
    top_n_vars = 5
  )
  
  # test 1: basic flip functionality (existing behavior)
  result_flip <- margot_flip_forests_dev(
    cf_results,
    flip_outcomes = "Y2",
    verbose = FALSE
  )
  
  expect_true("flip_outcomes" %in% names(result_flip))
  expect_equal(result_flip$flip_outcomes, "Y2")
  expect_true(!is.null(result_flip$results$model_Y2$tau_hat_original))
  expect_true(all(result_flip$results$model_Y2$tau_hat == -result_flip$results$model_Y2$tau_hat_original))
})

test_that("margot_flip_forests_dev works with custom covariates", {
  # create synthetic data
  set.seed(456)
  n <- 300
  p <- 15
  
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("covar_", 1:p)
  X_df <- as.data.frame(X)
  
  W <- rbinom(n, 1, 0.5)
  Y <- X[, 1] + X[, 5] + W * (X[, 3] + X[, 7]) + rnorm(n)
  
  df <- data.frame(Y = Y, W = W, X_df)
  
  # run margot_causal_forest
  cf_results <- margot::margot_causal_forest(
    data = df,
    outcome_vars = "Y",
    covariates = X_df,
    W = W,
    weights = rep(1, n),
    grf_defaults = list(num.trees = 200),
    save_data = TRUE,
    save_models = TRUE,
    top_n_vars = 5
  )
  
  # test custom covariates only
  custom_vars <- c("covar_3", "covar_7", "covar_10")
  result_custom <- margot_flip_forests_dev(
    cf_results,
    flip_outcomes = NULL,
    custom_covariates = custom_vars,
    covariate_mode = "custom",
    verbose = FALSE
  )
  
  # check that policy trees were recalculated with custom covariates
  expect_true("policy_tree_covariates" %in% names(result_custom$results$model_Y))
  expect_equal(result_custom$results$model_Y$policy_tree_covariates, custom_vars)
  expect_equal(result_custom$results$model_Y$policy_tree_covariate_mode, "custom")
})

test_that("margot_flip_forests_dev handles different covariate modes", {
  # create minimal test data
  set.seed(789)
  n <- 200
  p <- 8
  
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("var_", 1:p)
  X_df <- as.data.frame(X)
  
  W <- rbinom(n, 1, 0.5)
  Y <- X[, 1] + W * X[, 2] + rnorm(n)
  
  df <- data.frame(Y = Y, W = W, X_df)
  
  cf_results <- margot::margot_causal_forest(
    data = df,
    outcome_vars = "Y",
    covariates = X_df,
    W = W,
    weights = rep(1, n),
    grf_defaults = list(num.trees = 100),
    save_data = TRUE,
    save_models = TRUE,
    top_n_vars = 3
  )
  
  # test "add" mode
  custom_vars <- c("var_5", "var_6")
  result_add <- margot_flip_forests_dev(
    cf_results,
    custom_covariates = custom_vars,
    covariate_mode = "add",
    verbose = FALSE
  )
  
  # should include both original top vars and custom vars
  policy_covars <- result_add$results$model_Y$policy_tree_covariates
  expect_true(all(custom_vars %in% policy_covars))
  expect_true(length(policy_covars) >= length(custom_vars))
  
  # test "all" mode with warning suppression
  result_all <- suppressWarnings(margot_flip_forests_dev(
    cf_results,
    covariate_mode = "all",
    verbose = FALSE
  ))
  
  # should use all covariates
  expect_equal(length(result_all$results$model_Y$policy_tree_covariates), p)
})

test_that("margot_flip_forests_dev validates inputs correctly", {
  # create minimal valid results object
  mock_results <- list(
    results = list(
      model_Y1 = list(
        tau_hat = rnorm(100),
        dr_scores = matrix(rnorm(200), 100, 2),
        top_vars = c("X1", "X2", "X3")
      )
    ),
    covariates = data.frame(
      X1 = rnorm(100),
      X2 = rnorm(100),
      X3 = rnorm(100),
      X4 = rnorm(100)
    )
  )
  
  # test invalid covariate names
  expect_error(
    margot_flip_forests_dev(
      mock_results,
      custom_covariates = c("X1", "NonExistent"),
      verbose = FALSE
    ),
    "covariates are not available"
  )
  
  # test missing covariates in results
  mock_no_covars <- mock_results
  mock_no_covars$covariates <- NULL
  
  expect_error(
    margot_flip_forests_dev(
      mock_no_covars,
      custom_covariates = c("X1", "X2"),
      verbose = FALSE
    ),
    "covariates not found"
  )
  
  # test invalid model_results structure
  expect_error(
    margot_flip_forests_dev(
      list(invalid = "structure"),
      verbose = FALSE
    ),
    "must be a list containing a 'results' element"
  )
})

test_that("margot_flip_forests_dev combines flip and custom covariates", {
  # create test data
  set.seed(321)
  n <- 250
  p <- 6
  
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("pred_", 1:p)
  X_df <- as.data.frame(X)
  
  W <- rbinom(n, 1, 0.5)
  Y1 <- X[, 1] + W * X[, 2] + rnorm(n)
  Y2 <- X[, 3] - W * X[, 4] + rnorm(n)
  
  df <- data.frame(Y1 = Y1, Y2 = Y2, W = W, X_df)
  
  cf_results <- margot::margot_causal_forest(
    data = df,
    outcome_vars = c("Y1", "Y2"),
    covariates = X_df,
    W = W,
    weights = rep(1, n),
    grf_defaults = list(num.trees = 100),
    save_data = TRUE,
    save_models = TRUE,
    top_n_vars = 3
  )
  
  # flip Y2 and use custom covariates
  custom_vars <- c("pred_4", "pred_5", "pred_6")
  result_both <- margot_flip_forests_dev(
    cf_results,
    flip_outcomes = "Y2",
    custom_covariates = custom_vars,
    covariate_mode = "custom",
    verbose = FALSE
  )
  
  # check Y2 was flipped
  expect_true(all(result_both$results$model_Y2$tau_hat == -result_both$results$model_Y2$tau_hat_original))
  
  # check custom covariates were used for both models
  expect_equal(result_both$results$model_Y1$policy_tree_covariates, custom_vars)
  expect_equal(result_both$results$model_Y2$policy_tree_covariates, custom_vars)
})

test_that("margot_flip_forests_dev handles edge cases", {
  # minimal valid structure
  mock_results <- list(
    results = list(
      model_Y = list(
        tau_hat = rnorm(50),
        top_vars = c("X1", "X2")
      )
    )
  )
  
  # no flip, no custom covariates, not "all" mode - should return unchanged
  result_noop <- margot_flip_forests_dev(
    mock_results,
    flip_outcomes = NULL,
    custom_covariates = NULL,
    covariate_mode = "custom",
    verbose = FALSE
  )
  
  expect_identical(result_noop, mock_results)
  
  # empty flip_outcomes vector
  result_empty <- margot_flip_forests_dev(
    mock_results,
    flip_outcomes = character(0),
    verbose = FALSE
  )
  
  expect_identical(result_empty, mock_results)
})

test_that("margot_flip_forests_dev handles single covariate case", {
  # create test data with more covariates but select only 1
  set.seed(999)
  n <- 200
  p <- 5
  
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("X", 1:p)
  X_df <- as.data.frame(X)
  
  W <- rbinom(n, 1, 0.5)
  Y <- X[, 1] + W * 2 + X[, 2] + rnorm(n)
  
  df <- data.frame(Y = Y, W = W, X_df)
  
  # run margot_causal_forest
  cf_results <- margot::margot_causal_forest(
    data = df,
    outcome_vars = "Y",
    covariates = X_df,
    W = W,
    weights = rep(1, n),
    grf_defaults = list(num.trees = 100),
    save_data = TRUE,
    save_models = TRUE,
    top_n_vars = 3
  )
  
  # test with single custom covariate - should auto-expand for depth-2
  result_single <- suppressWarnings(margot_flip_forests_dev(
    cf_results,
    custom_covariates = "X1",
    covariate_mode = "custom",
    verbose = FALSE
  ))
  
  # check that both trees were created
  expect_true(!is.null(result_single$results$model_Y$policy_tree_depth_1))
  expect_true(!is.null(result_single$results$model_Y$policy_tree_depth_2))
  
  # check metadata
  expect_equal(result_single$results$model_Y$policy_tree_depth_1_covariates, "X1")
  expect_true(length(result_single$results$model_Y$policy_tree_depth_2_covariates) > 1)
  expect_true(result_single$results$model_Y$policy_tree_depth_2_auto_expanded)
  
  # test with warning message about auto-expansion
  expect_message(
    margot_flip_forests_dev(
      cf_results,
      custom_covariates = "X1",
      covariate_mode = "custom",
      verbose = TRUE
    ),
    "automatically adding original top variables"
  )
  
  # test edge case where even after expansion we don't have enough covariates
  # create data with truly only 1 covariate
  X_single <- matrix(rnorm(n), n, 1)
  colnames(X_single) <- "only_var"
  X_single_df <- as.data.frame(X_single)
  
  df_single <- data.frame(Y = Y, W = W, X_single_df)
  
  cf_single <- margot::margot_causal_forest(
    data = df_single,
    outcome_vars = "Y",
    covariates = X_single_df,
    W = W,
    weights = rep(1, n),
    grf_defaults = list(num.trees = 100),
    save_data = TRUE,
    save_models = TRUE,
    top_n_vars = 1
  )
  
  # with truly only 1 covariate, auto-expansion won't help, but policytree
  # can still create a depth-2 tree using the same variable at both levels
  result_truly_single <- suppressWarnings(margot_flip_forests_dev(
    cf_single,
    custom_covariates = "only_var",
    covariate_mode = "custom",
    verbose = FALSE
  ))
  
  expect_true(!is.null(result_truly_single$results$model_Y$policy_tree_depth_1))
  # policytree can create depth-2 tree even with 1 covariate (using same var at both levels)
  expect_false(result_truly_single$results$model_Y$policy_tree_depth_2_auto_expanded)
})