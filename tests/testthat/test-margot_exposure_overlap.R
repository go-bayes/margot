test_that("margot_exposure_overlap summarises supplied propensity scores without outcomes", {
  dat <- data.frame(
    exposure = c(0, 0, 1, 1),
    x1 = c(0, 1, 2, 3),
    x2 = c(1, 1, 0, 0),
    x3 = c("a", "b", "a", "b"),
    weights = c(1, 1, 2, 2)
  )

  overlap <- margot_exposure_overlap(
    data = dat,
    exposure = "exposure",
    covariates = c("x1", "x2", "x3"),
    weights = "weights",
    method = "supplied",
    propensity = c(0.02, 0.20, 0.80, 0.98),
    bounds = c(0.05, 0.95),
    plot = TRUE,
    verbose = FALSE
  )

  expect_s3_class(overlap, "margot_exposure_overlap")
  expect_equal(overlap$overlap_summary$n_total, 4)
  expect_equal(overlap$overlap_summary$poor_overlap_pct, 50)
  expect_equal(overlap$trimming_summary$n_trimmed_lower, 1)
  expect_equal(overlap$trimming_summary$n_trimmed_upper, 1)
  expect_equal(overlap$effective_sample_size$effective_n_all, 3.6)
  expect_equal(overlap$effective_sample_size$effective_n_in_bounds, 1.8)
  expect_equal(overlap$prevalence_summary$prevalence_unweighted, 0.5)
  expect_equal(overlap$prevalence_summary$prevalence_weighted, 4 / 6)
  expect_true(all(c("x1", "x2", "x3_a", "x3_b") %in% overlap$balance_summary$variable))
  expect_s3_class(overlap$propensity_plot, "ggplot")
  expect_true(overlap$metadata$outcome_blind)
})

test_that("margot_exposure_overlap blocks outcome-like inputs", {
  dat <- data.frame(
    exposure = c(0, 1, 0, 1),
    x1 = c(0, 1, 2, 3)
  )

  expect_error(
    margot_exposure_overlap(
      data = dat,
      exposure = "exposure",
      covariates = "x1",
      method = "supplied",
      propensity = c(0.2, 0.8, 0.3, 0.7),
      outcome = "Y"
    ),
    "Outcome variables are not allowed"
  )

  expect_error(
    margot_exposure_overlap(
      data = dat,
      exposure = "exposure",
      covariates = "x1",
      method = "supplied",
      propensity = c(0.2, 0.8, 0.3, 0.7),
      outcome_var = "Y"
    ),
    "Outcome variables are not allowed"
  )

  expect_error(
    margot_exposure_overlap(
      data = dat,
      exposure = "exposure",
      covariates = "x1",
      method = "supplied",
      propensity = c(0.2, 0.8, 0.3, 0.7),
      y_obs = "Y"
    ),
    "Outcome variables are not allowed"
  )

  expect_error(
    margot_exposure_overlap(
      data = list(full_models = list()),
      exposure = "exposure",
      covariates = "x1",
      method = "supplied",
      propensity = c(0.2, 0.8, 0.3, 0.7)
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )
})

test_that("margot_exposure_overlap estimates logistic propensities without outcome columns", {
  dat <- data.frame(
    exposure = c(0, 1, 0, 1, 0, 1, 0, 1),
    x1 = c(-1.0, -0.8, -0.2, 0.1, 0.2, 0.3, 0.9, 1.1),
    x2 = c(1, 0, 1, 0, 0, 1, 0, 1)
  )

  overlap <- margot_exposure_overlap(
    data = dat,
    exposure = "exposure",
    covariates = c("x1", "x2"),
    method = "logistic",
    bounds = c(0.05, 0.95),
    plot = FALSE,
    verbose = FALSE
  )

  expect_length(overlap$propensity, nrow(dat))
  expect_true(all(overlap$propensity > 0 & overlap$propensity < 1))
  expect_null(overlap$propensity_plot)
  expect_equal(overlap$metadata$method, "logistic")
})

test_that("logistic exposure model uses reduced-rank categorical encoding", {
  dat <- data.frame(
    exposure = c(0, 1, 0, 1, 1, 0, 0, 1, 0),
    x1 = c(-1, -0.5, 0, 0.5, 1, 1.5, -0.2, 0.2, 0.8),
    x2 = factor(c("a", "a", "b", "b", "c", "c", "a", "b", "c"))
  )

  full_covariates <- .margot_exposure_covariates(dat, c("x1", "x2"), drop_reference = FALSE)
  model_covariates <- .margot_exposure_covariates(dat, c("x1", "x2"), drop_reference = TRUE)

  expect_true(all(c("x2_a", "x2_b", "x2_c") %in% names(full_covariates)))
  expect_true(!"x2_a" %in% names(model_covariates))
  expect_true(all(c("x2_b", "x2_c") %in% names(model_covariates)))

  overlap <- margot_exposure_overlap(
    data = dat,
    exposure = "exposure",
    covariates = c("x1", "x2"),
    method = "logistic",
    bounds = c(0.05, 0.95),
    plot = FALSE,
    verbose = FALSE
  )

  expect_true(all(c("x2_a", "x2_b", "x2_c") %in% overlap$balance_summary$variable))
  expect_true(all(overlap$propensity > 0 & overlap$propensity < 1))
})
