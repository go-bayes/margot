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
  expect_true("relationship_summary" %in% names(rep))
  expect_true(is.data.frame(rep$summary_table))
  expect_true(is.list(rep$diagnostics))
  expect_true(is.character(rep$method_statement))
  expect_true(is.character(rep$relationship_summary))
  expect_false(is.null(rep$narrative))
})
