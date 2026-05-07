test_that("margot_grf_overlap handles matrix covariates without deprecation output", {
  W <- rep(c(0, 1), 10)
  W_hat <- seq(0.12, 0.88, length.out = 20)

  model_results <- list(
    full_models = list(
      model_y = list(
        W.orig = W,
        W.hat = W_hat
      )
    ),
    covariates = cbind(
      x1 = seq(-1, 1, length.out = 20),
      x2 = seq(1, -1, length.out = 20)
    ),
    results = list(
      model_y = list(
        top_vars = c("x1", "x2"),
        test_calibration = structure(
          c(0, 0.2, 0.1, 0.2, 0, 1, 0.5, 0.25),
          dim = c(2, 4),
          dimnames = list(
            c("mean.forest.prediction", "differential.forest.prediction"),
            c("Estimate", "Std. Error", "t value", "Pr(>t)")
          )
        )
      )
    )
  )

  expect_message(
    overlap <- margot_grf_overlap(
      model_results,
      exposure_name = "binary_exposure",
      plot = TRUE,
      verbose = FALSE
    ),
    NA
  )

  expect_equal(overlap$overlap_summary$n_total, 20)
  expect_equal(overlap$overlap_summary$good_overlap_pct, 100)
  expect_equal(overlap$overlap_summary$test_calibration_estimate, 0.2)
  expect_equal(overlap$overlap_summary$test_calibration_pvalue, 0.25)
  expect_s3_class(overlap$propensity_plots$exposure, "ggplot")
  expect_equal(overlap$balance_tables$exposure$variable, c("x1", "x2"))
})

test_that("margot_assess_overlap keeps the soft-deprecation notice", {
  model_results <- list(
    full_models = list(
      model_y = list(
        W.orig = c(0, 0, 1, 1),
        W.hat = c(0.2, 0.3, 0.7, 0.8)
      )
    ),
    results = list(
      model_y = list(test_calibration = c(estimate = 0, p.value = 0.5))
    )
  )

  expect_message(
    margot_assess_overlap(model_results, plot = FALSE, verbose = FALSE),
    "soft-deprecated"
  )
})
