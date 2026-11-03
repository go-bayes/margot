# D19: one uncapped density fit per policy serves the capped primary and the
# untrimmed sensitivity; each must equal a native lmtp_sdr call at its own cap.

untrimmed_fixture <- function(n = 160L, seed = 7L) {
  set.seed(seed)
  data <- data.frame(id = seq_len(n), w = stats::rnorm(n), a1 = stats::rnorm(n),
    l2 = stats::rnorm(n), a2 = stats::rnorm(n))
  data$y <- data$a1 + data$a2 + data$w + stats::rnorm(n)
  data$z <- 0.5 * data$a2 - data$w + stats::rnorm(n)
  data
}
untrimmed_shift <- function(data, trt) data[[trt]] - 0.5
untrimmed_defaults <- function(cap) {
  list(baseline = "w", time_vary = list(NULL, "l2"), outcome_type = "continuous", id = "id",
    folds = 2L, learners_trt = c("SL.glm", "SL.mean"), learners_outcome = c("SL.glm", "SL.mean"),
    control = lmtp::lmtp_control(.trim = cap))
}
native_fit <- function(data, outcome, cap, seed = 3031L) {
  set.seed(seed)
  do.call(lmtp::lmtp_sdr, c(list(data = data, trt = c("a1", "a2"), outcome = outcome,
    shift = untrimmed_shift), untrimmed_defaults(cap)))
}
run_untrimmed <- function(data, cap = 0.9, ...) {
  suppressMessages(margot_lmtp(data, outcome_vars = c("y", "z"), trt = c("a1", "a2"),
    shift_functions = list(down = untrimmed_shift), include_null_shift = FALSE,
    lmtp_model_type = lmtp::lmtp_sdr, lmtp_defaults = untrimmed_defaults(cap), seed = 3031L,
    progress = "none", reuse_density_ratios = TRUE, untrimmed_sensitivity = TRUE, ...))
}
check_against_native <- function(output, data, cap) {
  for (outcome in c("y", "z")) {
    capped <- output$models[[outcome]][[paste0(outcome, "_down")]]
    untrimmed <- output$untrimmed_sensitivity$models[[outcome]][[paste0(outcome, "_down")]]
    expect_identical(shared_ratio_numerics(capped), shared_ratio_numerics(native_fit(data, outcome, cap)))
    expect_identical(shared_ratio_numerics(untrimmed), shared_ratio_numerics(native_fit(data, outcome, 1)))
    expect_identical(capped$fits_m, native_fit(data, outcome, cap)$fits_m)
    expect_identical(untrimmed$fits_m, native_fit(data, outcome, 1)$fits_m)
  }
}

test_that("capped and untrimmed results equal native lmtp calls exactly (sequential plan)", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- untrimmed_fixture()
  output <- run_untrimmed(data, cap = 0.9)
  check_against_native(output, data, cap = 0.9)
  record <- output$untrimmed_sensitivity$capping$y$y_down
  expect_equal(record$cap, 0.9)
  expect_gt(record$capped_entries, 0L)
  expect_equal(record$capped_proportion, record$capped_entries / record$entries)
  expect_lte(record$capped_range[2L], record$threshold)
  expect_gt(record$raw_range[2L], record$threshold)
  # the primary output keeps its usual structure, with the capping record beside it.
  expect_null(output$models$y$y_down$untrimmed_model)
  expect_identical(output$models$y$y_down$untrimmed_record, record)
  expect_length(output$untrimmed_sensitivity$failures, 0L)
})

test_that("results do not depend on worker timing (multisession plan)", {
  skip_on_cran()
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  data <- untrimmed_fixture()
  future::plan(future::sequential)
  sequential <- run_untrimmed(data, cap = 0.9)
  future::plan(future::multisession, workers = 2L)
  parallel <- run_untrimmed(data, cap = 0.9)
  for (outcome in c("y", "z")) {
    name <- paste0(outcome, "_down")
    expect_identical(shared_ratio_numerics(parallel$models[[outcome]][[name]]),
      shared_ratio_numerics(sequential$models[[outcome]][[name]]))
    expect_identical(shared_ratio_numerics(parallel$untrimmed_sensitivity$models[[outcome]][[name]]),
      shared_ratio_numerics(sequential$untrimmed_sensitivity$models[[outcome]][[name]]))
    expect_identical(parallel$models[[outcome]][[name]]$fits_m, sequential$models[[outcome]][[name]]$fits_m)
    expect_identical(parallel$untrimmed_sensitivity$models[[outcome]][[name]]$fits_m,
      sequential$untrimmed_sensitivity$models[[outcome]][[name]]$fits_m)
  }
})

test_that("the untrimmed route combines with the learner audit and stays lean", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- untrimmed_fixture()
  output <- run_untrimmed(data, cap = 0.9, learner_audit = TRUE)
  margot_lmtp_assert_lean(output$models)
  margot_lmtp_assert_lean(output$untrimmed_sensitivity$models)
  untrimmed <- output$untrimmed_sensitivity$models$y$y_down
  expect_s3_class(untrimmed$learner_audit, "margot_lmtp_learner_audit")
  untrimmed$learner_audit <- NULL
  expect_identical(shared_ratio_numerics(untrimmed), shared_ratio_numerics(native_fit(data, "y", 1)))
})

test_that("the untrimmed route refuses calls without a registered cap or on another route", {
  data <- untrimmed_fixture()
  expect_error(run_untrimmed(data, cap = 1), class = "margot_error_untrimmed_route")
  expect_error(suppressMessages(margot_lmtp(data, outcome_vars = "y", trt = c("a1", "a2"),
    shift_functions = list(down = untrimmed_shift), include_null_shift = FALSE,
    lmtp_model_type = lmtp::lmtp_sdr, lmtp_defaults = untrimmed_defaults(0.9), progress = "none",
    untrimmed_sensitivity = TRUE)), class = "margot_error_untrimmed_route")
  expect_error(run_untrimmed(data, cap = 0.9, manage_future_plan = TRUE),
    class = "margot_error_untrimmed_route")
})

test_that("censoring, two policies and a stochastic learner match native calls and give contrasts", {
  skip_if_not_installed("ranger")
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  set.seed(19)
  n <- 220L
  data <- data.frame(id = seq_len(n), w = stats::rnorm(n), a1 = stats::rnorm(n), l2 = stats::rnorm(n),
    a2 = stats::rnorm(n))
  data$c1 <- as.numeric(stats::runif(n) > 0.1)
  data$c2 <- ifelse(data$c1 == 1, as.numeric(stats::runif(n) > 0.1), 0)
  data$y <- data$a1 + data$a2 + data$w + stats::rnorm(n)
  data$y[data$c2 == 0] <- NA_real_
  data$a2[data$c1 == 0] <- NA_real_
  data$l2[data$c1 == 0] <- NA_real_
  shifts <- list(down = function(data, trt) data[[trt]] - 0.5, up = function(data, trt) data[[trt]] + 0.5)
  defaults <- function(cap) list(baseline = "w", time_vary = list(NULL, "l2"), cens = c("c1", "c2"),
    outcome_type = "continuous", id = "id", folds = 2L, learners_trt = c("SL.glm", "SL.ranger"),
    learners_outcome = c("SL.glm", "SL.mean"), control = lmtp::lmtp_control(.trim = cap))
  output <- suppressMessages(margot_lmtp(data, outcome_vars = "y", trt = c("a1", "a2"),
    shift_functions = shifts, include_null_shift = FALSE, lmtp_model_type = lmtp::lmtp_sdr,
    lmtp_defaults = defaults(0.9), seed = 3031L, progress = "none", reuse_density_ratios = TRUE,
    untrimmed_sensitivity = TRUE))
  native <- function(shift, cap) {
    set.seed(3031L)
    do.call(lmtp::lmtp_sdr, c(list(data = data, trt = c("a1", "a2"), outcome = "y", shift = shift), defaults(cap)))
  }
  for (name in names(shifts)) {
    model_name <- paste0("y_", name)
    expect_identical(shared_ratio_numerics(output$models$y[[model_name]]),
      shared_ratio_numerics(native(shifts[[name]], 0.9)))
    expect_identical(shared_ratio_numerics(output$untrimmed_sensitivity$models$y[[model_name]]),
      shared_ratio_numerics(native(shifts[[name]], 1)))
    expect_gt(output$untrimmed_sensitivity$capping$y[[model_name]]$capped_entries, 0L)
  }
  # the sensitivity has its own contrasts and tables, in the primary's structure.
  expect_identical(names(output$untrimmed_sensitivity$contrasts), names(output$contrasts))
  expect_true(length(output$untrimmed_sensitivity$contrasts) > 0L)
  expect_identical(names(output$untrimmed_sensitivity$combined_tables), names(output$combined_tables))
})

test_that("a failed untrimmed recursion is reported and the capped primary is kept", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- untrimmed_fixture()
  real_fit <- margot_lmtp_fit_sdr_outcome
  calls <- 0L
  # every second outcome recursion is an untrimmed one; make the first of those fail.
  testthat::local_mocked_bindings(margot_lmtp_fit_sdr_outcome = function(...) {
    calls <<- calls + 1L
    if (calls == 2L) stop("simulated learner failure on raw ratios")
    real_fit(...)
  })
  output <- run_untrimmed(data, cap = 0.9)
  expect_identical(shared_ratio_numerics(output$models$y$y_down), shared_ratio_numerics(native_fit(data, "y", 0.9)))
  expect_length(output$untrimmed_sensitivity$failures, 1L)
  expect_match(output$untrimmed_sensitivity$failures[[1L]]$message, "simulated learner failure")
  expect_null(output$untrimmed_sensitivity$models$y$y_down)
  expect_identical(shared_ratio_numerics(output$untrimmed_sensitivity$models$z$z_down),
    shared_ratio_numerics(native_fit(data, "z", 1)))
})
