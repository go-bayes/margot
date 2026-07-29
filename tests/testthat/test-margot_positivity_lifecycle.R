# the dispositions the margot.lmtp reporting family imposes on the legacy
# positivity surface: removed arguments, removed verdict fields, and the
# soft-deprecation warnings that fire once per session

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

# --- soft deprecation ------------------------------------------------------

# lifecycle caches a deprecation signal by its message for the session, which is
# what "once per session" means. clearing the cache lets each assertion below
# stand on its own rather than on the order the test files happen to run in.
clear_lifecycle_cache <- function() {
  env <- tryCatch(get("deprecation_env", envir = asNamespace("lifecycle")),
                  error = function(e) NULL)
  if (is.environment(env)) rm(list = ls(env, all.names = TRUE), envir = env)
  invisible(NULL)
}

wrapper_calls <- function() {
  list(
    margot_positivity_summary = function() {
      margot_positivity_summary(make_run(), include_explanation = FALSE)
    },
    margot_interpret_lmtp_positivity = function() {
      margot_interpret_lmtp_positivity(make_run(), include_tests = FALSE)
    },
    margot_interpret_lmtp_positivity_overview = function() {
      margot_interpret_lmtp_positivity_overview(list())
    },
    margot_report_lmtp_positivity = function() {
      margot_report_lmtp_positivity(make_run(), outcome = "outcome", include_plots = FALSE)
    },
    margot_lmtp_positivity_report = function() {
      margot_lmtp_positivity_report(make_run(), outcome = "outcome", include_plots = FALSE)
    },
    margot_positivity_report = function() {
      margot_positivity_report(
        make_run(), include_plot = FALSE,
        interpret_args = list(include_tests = FALSE, include_diagnostics = FALSE)
      )
    },
    margot_positivity_report_single_model = function() {
      margot_positivity_report_single_model(
        list(density_ratios = matrix(c(2, 1, 1.5, 2), nrow = 2)),
        outcome = "outcome", include_plot = FALSE,
        interpret_args = list(include_tests = FALSE, include_diagnostics = FALSE)
      )
    }
  )
}

# collects the deprecation messages one call signals, muffling them
collect_deprecations <- function(expr) {
  seen <- character()
  suppressWarnings(withCallingHandlers(
    try(expr, silent = TRUE),
    lifecycle_warning_deprecated = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  seen
}

test_that("each of the seven soft-deprecated wrappers warns", {
  wrappers <- wrapper_calls()
  expect_length(wrappers, 7L)

  for (nm in names(wrappers)) {
    clear_lifecycle_cache()
    seen <- collect_deprecations(wrappers[[nm]]())
    expect_true(any(grepl(nm, seen, fixed = TRUE)), info = nm)
  }
  clear_lifecycle_cache()
})

test_that("the deprecation message names the replacement and the schedule", {
  clear_lifecycle_cache()
  seen <- collect_deprecations(
    margot_interpret_lmtp_positivity(make_run(), include_tests = FALSE)
  )
  expect_length(seen, 1L)
  msg <- seen[[1]]
  expect_match(msg, "margot.lmtp", fixed = TRUE)
  expect_match(msg, "margot_lmtp_ratio_report", fixed = TRUE)
  expect_match(msg, "one full release cycle", fixed = TRUE)
  expect_match(msg, "removed the cycle after", fixed = TRUE)
  expect_match(msg, "carry no identification judgement", fixed = TRUE)
  clear_lifecycle_cache()
})

test_that("a soft-deprecated wrapper warns once per session", {
  # testthat raises lifecycle verbosity during checks, which defeats the
  # per-session cache; the default verbosity is what a user sees
  old <- options(lifecycle_verbosity = NULL)
  on.exit(options(old), add = TRUE)
  clear_lifecycle_cache()
  first <- collect_deprecations(
    margot_positivity_summary(make_run(), include_explanation = FALSE)
  )
  second <- collect_deprecations(
    margot_positivity_summary(make_run(), include_explanation = FALSE)
  )
  expect_length(first, 1L)
  expect_length(second, 0L)
  clear_lifecycle_cache()
})

test_that("the deprecation warning attributes the call to the user, not to margot", {
  clear_lifecycle_cache()
  seen <- collect_deprecations(
    margot_positivity_summary(make_run(), include_explanation = FALSE)
  )
  expect_length(seen, 1L)
  # lifecycle adds "likely used in the <pkg> package" when the user environment it
  # is handed belongs to the package that signalled. forwarding user_env keeps that
  # sentence out of a warning the user provoked from the global environment.
  expect_false(grepl("likely used in the margot package", seen[[1]], fixed = TRUE))
  clear_lifecycle_cache()

  # and the argument is forwarded rather than defaulted away
  expect_true("user_env" %in% names(formals(margot_deprecate_positivity)))
  clear_lifecycle_cache()
})
