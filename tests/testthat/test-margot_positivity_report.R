test_that("margot_positivity_report bundles components", {
  dr <- matrix(c(2, 0, 1,
                 1.5, 0.5, 2), nrow = 2, byrow = TRUE)
  fit <- list(
    models = list(
      outcome = list(
        outcome_null = list(
          density_ratios = dr,
          exposure_by_wave = matrix(c(0, 1,
                                      1, 0), nrow = 2, byrow = TRUE)
        )
      )
    )
  )

  rep <- margot_positivity_report(
    x = fit,
    outcome = "outcome",
    shifts = "null",
    include_policy_rates = FALSE,
    include_plot = FALSE,
    interpret_args = list(include_tests = FALSE, include_diagnostics = FALSE)
  )

  expect_true("summary_table" %in% names(rep))
  expect_true("diagnostics" %in% names(rep))
  expect_true("method_statement" %in% names(rep))
  expect_true(is.data.frame(rep$summary_table))
  expect_true(is.list(rep$diagnostics))
  expect_true(is.character(rep$method_statement))
  expect_true(is.null(rep$censoring_summary) || is.character(rep$censoring_summary))
  expect_true(is.null(rep$wave_summary_table) || is.data.frame(rep$wave_summary_table))
  expect_false(is.null(rep$narrative))
  expect_true(any(names(rep$summary_table) == "ESS per N% (precision)"))
})

test_that("margot_positivity_report_single_model wraps single LMTP outputs", {
  dr <- matrix(c(2, 0, 1,
                 1.5, 0.5, 2), nrow = 2, byrow = TRUE)
  single_fit <- list(
    estimator = "TMLE",
    density_ratios = dr,
    shift = "(gain_A)"
  )

  rep <- margot_positivity_report_single_model(
    x = single_fit,
    outcome = "t2_meaning_purpose_z",
    include_policy_rates = FALSE,
    include_plot = FALSE,
    interpret_args = list(include_tests = FALSE, include_diagnostics = FALSE)
  )

  expect_true(is.list(rep))
  expect_true(is.data.frame(rep$summary_table))
  expect_true(is.list(rep$diagnostics))
  expect_equal(rep$metadata$outcome, "t2_meaning_purpose_z")
  expect_equal(rep$metadata$shifts, "(gain_A)")
})
