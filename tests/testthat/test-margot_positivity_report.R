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
  expect_true(is.null(rep$precision_table) || is.data.frame(rep$precision_table))
  expect_false(is.null(rep$narrative))
  expect_true("Support" %in% names(rep$summary_table))
  expect_true("Zero %" %in% names(rep$summary_table))
  expect_true("Cum ESS" %in% names(rep$summary_table))
  expect_true(any(grepl("^Outside \\[", names(rep$summary_table))))
  expect_true(!any(names(rep$summary_table) == "ESS/N_pt (precision)"))
})

test_that("margot_positivity_report defaults to first outcome and all shifts", {
  dr1 <- matrix(c(2, 0, 1,
                  1.5, 0.5, 2), nrow = 2, byrow = TRUE)
  dr2 <- matrix(c(1, 1, 1,
                  2, 2, 2), nrow = 2, byrow = TRUE)
  fit <- list(
    models = list(
      first_outcome = list(
        first_outcome_shift_zero = list(density_ratios = dr2),
        first_outcome_null = list(density_ratios = dr1)
      ),
      second_outcome = list(
        second_outcome_null = list(density_ratios = dr1)
      )
    )
  )

  rep <- margot_positivity_report(
    x = fit,
    include_policy_rates = FALSE,
    include_plot = FALSE,
    interpret_args = list(include_tests = FALSE, include_diagnostics = FALSE)
  )

  expect_equal(rep$metadata$outcome, "first_outcome")
  expect_equal(rep$metadata$shifts, c("first_outcome_null", "first_outcome_shift_zero"))
  expect_equal(rep$summary_table$Shift, c("Identity", "Shift Zero"))
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

test_that("margot_positivity_report keeps thresholds aligned in manuscript mode", {
  dr_null <- matrix(c(1, 1, 1, 1), ncol = 1)
  dr_shift <- matrix(c(0.05, 1, 1, 1), ncol = 1)
  fit <- list(
    models = list(
      outcome = list(
        outcome_null = list(density_ratios = dr_null),
        outcome_shift_up = list(density_ratios = dr_shift)
      )
    )
  )

  rep <- margot_positivity_report(
    x = fit,
    outcome = "outcome",
    shifts = c("null", "shift_up"),
    include_policy_rates = FALSE,
    include_plot = FALSE,
    interpret_args = list(
      include_tests = FALSE,
      include_diagnostics = FALSE,
      output_style = "manuscript",
      test_thresholds = list(prod_frac_warn = 0.30)
    )
  )

  expect_equal(rep$summary_table$Support, c("Adequate", "Caution"))
  expect_equal(rep$metadata$test_thresholds$prod_frac_warn, 0.30)
  expect_equal(rep$narrative$output_style, "manuscript")
  expect_true(length(rep$narrative$paragraphs) >= 2)
  expect_false(grepl("^##", rep$narrative$text))
  expect_match(rep$narrative$text, "interpreted cautiously")
  expect_true(is.data.frame(rep$narrative$support_metrics))
})
