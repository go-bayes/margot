test_that("margot_rate_cv can refit from saved data without full models", {
  captures <- new.env(parent = emptyenv())
  model_results <- list(
    results = list(model_y = list()),
    outcome_vars = "y",
    data = data.frame(y = seq_len(6)),
    covariates = data.frame(x1 = seq_len(6), x2 = rev(seq_len(6))),
    W = c(0, 1, 0, 1, 0, 1),
    weights = rep(1, 6),
    not_missing = seq_len(6),
    grf_defaults = list(num.trees = 5000, min.node.size = 50)
  )

  testthat::local_mocked_bindings(
    rate_sequential_cv = function(X, Y, W, weights, num_folds, target,
                                  grf_defaults, seed, verbose, model_name, ...) {
      captures$model_name <- model_name
      captures$n <- nrow(X)
      captures$grf_defaults <- grf_defaults
      list(
        p_value = 0.5,
        t_statistic = 0.67,
        cv_estimate = 0.12,
        cv_std_error = 0.03,
        fold_t_statistics = c(0.3, 0.7),
        fold_t_statistics_raw = c(0.3, 0.7),
        nonfinite_fold_t_statistics = c(FALSE, FALSE),
        fold_sizes = c(3, 3),
        fold_estimates = c(0.10, 0.14),
        fold_std_errors = c(0.02, 0.03),
        num_folds = num_folds,
        target = target
      )
    }
  )

  out <- margot_rate_cv(
    model_results,
    target = "AUTOC",
    num_folds = 3,
    verbose = FALSE
  )

  expect_s3_class(out, "margot_cv_results")
  expect_equal(captures$model_name, "model_y")
  expect_equal(captures$n, 6L)
  expect_equal(captures$grf_defaults$num.trees, 5000)
  expect_equal(captures$grf_defaults$min.node.size, 50)
  expect_equal(out$cv_results$estimate, 0.12)
})

test_that("RATE CV fold repair treats non-finite t-statistics as zero evidence", {
  out <- .repair_rate_cv_folds(
    t_statistics = c(NaN, 1, Inf),
    fold_sizes = c(10, 10, 10),
    fold_estimates = c(0, 0.2, 0.1),
    fold_std_errors = c(0, 0.1, 0)
  )

  expect_true(is.nan(out$fold_t_statistics_raw[1]))
  expect_equal(out$fold_t_statistics, c(0, 1, 0))
  expect_equal(out$nonfinite_fold_t_statistics, c(TRUE, FALSE, TRUE))
  expect_equal(out$t_statistic, 1 / sqrt(3))
  expect_true(is.finite(out$p_value))
  expect_true(is.finite(out$cv_estimate))
})
