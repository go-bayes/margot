# the descriptive positivity surface retains quantities while excluding verdicts

make_run <- function(dr = matrix(c(2, 0, 1, 1.5, 0.5, 2), nrow = 2, byrow = TRUE),
                     outcome = "outcome",
                     shift = "outcome_null") {
  models <- list()
  models[[shift]] <- list(density_ratios = dr)
  fit <- list(models = list())
  fit$models[[outcome]] <- models
  fit
}

# --- margot_lmtp_positivity(): thresholds and flags removed -----------------

test_that("margot_lmtp_positivity() returns descriptive summaries with no flags", {
  pos <- margot_lmtp_positivity(make_run(), verbose = FALSE)

  expect_named(pos, c("by_wave", "overall"))
  expect_false("flags" %in% names(pos))
  expect_true(nrow(pos$by_wave) > 0L)
  expect_false(any(grepl("flag", names(pos$by_wave))))
})

test_that("margot_lmtp_positivity() errors on every removed threshold argument", {
  for (arg in c("ess_warn", "zero_warn", "tail_warn")) {
    call_args <- list(x = make_run(), verbose = FALSE)
    call_args[[arg]] <- 0.5
    expect_error(
      do.call(margot_lmtp_positivity, call_args),
      class = "margot_error_removed_argument"
    )
  }
})

test_that("the removed-argument error names the argument", {
  err <- tryCatch(
    margot_lmtp_positivity(make_run(), ess_warn = 0.5, verbose = FALSE),
    margot_error_removed_argument = function(e) e
  )
  expect_match(conditionMessage(err), "ess_warn")
})

# --- margot_lmtp_overlap(): flags and threshold arguments removed -----------

test_that("margot_lmtp_overlap() returns no flags field", {
  ol <- suppressWarnings(
    margot_lmtp_overlap(make_run(), plot = FALSE, verbose = FALSE)
  )
  expect_false("flags" %in% names(ol))
  expect_true(all(c("overlap_summary", "ratio_plots", "text_summary") %in% names(ol)))
})

test_that("margot_lmtp_overlap() errors on the retired arguments", {
  expect_error(
    margot_lmtp_overlap(make_run(), plot = FALSE, verbose = FALSE,
                        test_thresholds = list(prod_frac_warn = 0.3)),
    class = "margot_error_removed_argument"
  )
  expect_error(
    margot_lmtp_overlap(make_run(), plot = FALSE, verbose = FALSE,
                        policy_rate_strict = FALSE),
    class = "margot_error_removed_argument"
  )
})

# --- status columns removed ------------------------------------------------

test_that("margot_positivity_summary() carries no status, verdict, or screen", {
  tbl <- suppressWarnings(
    margot_positivity_summary(make_run(), compact = FALSE, include_explanation = FALSE)
  )
  expect_false(any(c("support_status", "verdict") %in% names(tbl)))
  # the descriptive quantities the screen used to grade remain
  expect_true("prod_frac_outside" %in% names(tbl))
  expect_true("prop_zero_prod_pct" %in% names(tbl))

  compact <- suppressWarnings(
    margot_positivity_summary(make_run(), compact = TRUE, include_explanation = TRUE)
  )
  expect_false("Support" %in% names(compact))
  expect_false(grepl("Adequate|Caution|Limited", attr(compact, "explanation")))
})

test_that("margot_interpret_lmtp_positivity() carries no support status", {
  res <- suppressWarnings(margot_interpret_lmtp_positivity(
    make_run(),
    include_tests = TRUE,
    include_diagnostics = FALSE,
    return = "list"
  ))
  expect_true(is.data.frame(res$support_metrics))
  expect_false("support_status" %in% names(res$support_metrics))
  expect_false(grepl("Adequate|Caution|Limited", res$text))
})

test_that("margot_report_lmtp_positivity() returns no flags field", {
  rep <- suppressWarnings(margot_report_lmtp_positivity(
    make_run(), outcome = "outcome", include_plots = FALSE
  ))
  expect_false("flags" %in% names(rep))
  expect_true("overall" %in% names(rep))
})

test_that("Margot positivity reporters remain active without package-redirection warnings", {
  expect_no_warning(margot_positivity_summary(make_run(), include_explanation = FALSE))
  expect_no_warning(margot_interpret_lmtp_positivity(make_run(), include_tests = FALSE))
  expect_no_warning(margot_interpret_lmtp_positivity_overview(list()))
  expect_no_error(suppressWarnings(
    margot_report_lmtp_positivity(make_run(), outcome = "outcome", include_plots = FALSE)
  ))
  expect_no_error(suppressWarnings(
    margot_lmtp_positivity_report(make_run(), outcome = "outcome", include_plots = FALSE)
  ))
})
