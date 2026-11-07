# margot_lmtp() refuses an lmtp control that keeps full SuperLearner fits unless
# allow_full_fits gives a reason. Generated data only.

full_fits_data <- function(n = 60L) {
  make_shared_ratio_data(n = n, seed = 11L)
}

full_fits_defaults <- function(control = NULL, learners = "SL.mean") {
  defaults <- list(
    baseline = "baseline",
    outcome_type = "continuous",
    id = "id",
    folds = 2L,
    learners_trt = learners,
    learners_outcome = learners
  )
  if (!is.null(control)) defaults$control <- control
  defaults
}

# call margot_lmtp quietly on the generated frame
run_full_fits <- function(lmtp_defaults, ..., outcome_vars = "perfectionism",
                          lmtp_model_type = lmtp::lmtp_sdr) {
  suppressMessages(margot_lmtp(
    data = full_fits_data(),
    outcome_vars = outcome_vars,
    trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down),
    include_null_shift = FALSE,
    lmtp_model_type = lmtp_model_type,
    lmtp_defaults = lmtp_defaults,
    progress = "none",
    seed = 2026L,
    ...
  ))
}

with_sequential_plan <- function(code) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan, substitute = FALSE), add = TRUE)
  future::plan(future::sequential)
  force(code)
}

test_that("an explicit TRUE is refused before any model is fitted", {
  with_sequential_plan({
    fits <- 0L
    counting_estimator <- function(...) {
      fits <<- fits + 1L
      stop("an estimator was called")
    }
    error <- expect_error(
      run_full_fits(
        full_fits_defaults(lmtp::lmtp_control(.return_full_fits = TRUE)),
        lmtp_model_type = counting_estimator
      ),
      class = "margot_error_full_fits_refused"
    )
    expect_identical(fits, 0L)
    message_text <- cli::ansi_strip(conditionMessage(error))
    expect_match(message_text, "allow_full_fits", fixed = TRUE)
    expect_match(message_text, ".return_full_fits = TRUE", fixed = TRUE)
    # a plain list is resolved the same way as an lmtp_control() object
    expect_error(
      run_full_fits(
        full_fits_defaults(list(.return_full_fits = TRUE)),
        lmtp_model_type = counting_estimator
      ),
      class = "margot_error_full_fits_refused"
    )
    expect_identical(fits, 0L)
  })
})

test_that("every fitting route is refused at entry", {
  with_sequential_plan({
    # an absent learner would fail any fit, so the refusal class shows that no
    # fit was attempted
    defaults <- full_fits_defaults(
      lmtp::lmtp_control(.return_full_fits = TRUE, .trim = 0.9),
      learners = "SL.absent_learner"
    )
    output_dir <- tempfile("full-fits-")
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
    routes <- list(
      independent = list(),
      independent_tmle = list(lmtp_model_type = lmtp::lmtp_tmle),
      independent_managed_plan = list(manage_future_plan = TRUE),
      shared_sequential = list(reuse_density_ratios = TRUE),
      shared_task = list(
        reuse_density_ratios = TRUE, manage_future_plan = TRUE,
        save_output = TRUE, save_path = output_dir
      ),
      shared_density_stage = list(
        reuse_density_ratios = TRUE, manage_future_plan = TRUE,
        save_output = TRUE, save_path = output_dir, stages = "density"
      ),
      shared_outcome_stage = list(
        reuse_density_ratios = TRUE, manage_future_plan = TRUE,
        save_output = TRUE, save_path = output_dir, stages = "outcome"
      ),
      untrimmed_sensitivity = list(
        reuse_density_ratios = TRUE, untrimmed_sensitivity = TRUE
      ),
      supplied_components = list(
        lmtp_model_type = margot_lmtp_sdr_from_components
      )
    )
    for (route in names(routes)) {
      expect_error(
        do.call(run_full_fits, c(list(lmtp_defaults = defaults), routes[[route]])),
        class = "margot_error_full_fits_refused",
        info = route
      )
    }
    # the refusal comes before any checkpoint directory is created
    expect_false(dir.exists(output_dir))
  })
})

test_that("partial names, shortened control names and non-logical values are refused", {
  with_sequential_plan({
    estimator <- function(...) stop("an estimator was called")
    refused <- list(
      shortened = list(.return_full = TRUE),
      shortened_one = list(.return_full = 1),
      lengthened = list(.return_full_fits_keep = TRUE),
      na = list(.return_full_fits = NA),
      number = list(.return_full_fits = 1),
      text = list(.return_full_fits = "TRUE"),
      length_two = list(.return_full_fits = c(FALSE, TRUE)),
      null_value = list(.return_full_fits = NULL)
    )
    for (case in names(refused)) {
      # a reason does not excuse a malformed or misspelt setting
      expect_error(
        run_full_fits(
          full_fits_defaults(refused[[case]]),
          lmtp_model_type = estimator,
          allow_full_fits = "testing the guard"
        ),
        class = "margot_error_full_fits_refused",
        info = case
      )
    }
    # R would match `contr` to `control`, so the shortened name is refused
    defaults <- full_fits_defaults()
    defaults$contr <- lmtp::lmtp_control(.return_full_fits = TRUE)
    expect_error(
      run_full_fits(defaults, lmtp_model_type = estimator),
      class = "margot_error_full_fits_refused"
    )
    # a control that is not a list is refused
    expect_error(
      run_full_fits(full_fits_defaults("keep"), lmtp_model_type = estimator),
      class = "margot_error_invalid_input"
    )
  })
})

test_that("allow_full_fits must be NULL or one non-empty string", {
  with_sequential_plan({
    estimator <- function(...) stop("an estimator was called")
    for (value in list("", "   ", NA_character_, c("a", "b"), 1, TRUE)) {
      expect_error(
        run_full_fits(
          full_fits_defaults(lmtp::lmtp_control(.return_full_fits = TRUE)),
          lmtp_model_type = estimator,
          allow_full_fits = value
        ),
        class = "margot_error_invalid_input"
      )
    }
  })
})

test_that("the guard resolves the control over lmtp's defaults", {
  expect_false(margot_lmtp_check_full_fits(NULL))
  expect_false(margot_lmtp_check_full_fits(list()))
  expect_false(margot_lmtp_check_full_fits(list(.trim = 0.9)))
  expect_false(margot_lmtp_check_full_fits(lmtp::lmtp_control()))
  expect_false(margot_lmtp_check_full_fits(list(.return_full = FALSE)))
  expect_true(suppressMessages(
    margot_lmtp_check_full_fits(list(.return_full_fits = TRUE), allow_full_fits = "why")
  ))
})

test_that("the default control and an explicit FALSE fit and keep lean fits", {
  with_sequential_plan({
    for (control in list(NULL, lmtp::lmtp_control(), lmtp::lmtp_control(.return_full_fits = FALSE))) {
      output <- run_full_fits(full_fits_defaults(control))
      model <- output$models$perfectionism$perfectionism_down
      expect_s3_class(model, "lmtp")
      expect_null(attr(output, "margot_full_fits"))
      # lean fits hold learner weights, not SuperLearner objects
      expect_false(inherits(model$fits_m[[1L]][[1L]], "SuperLearner"))
    }
    shared <- run_full_fits(
      full_fits_defaults(lmtp::lmtp_control(.return_full_fits = FALSE)),
      reuse_density_ratios = TRUE
    )
    expect_s3_class(shared$models$perfectionism$perfectionism_down, "lmtp")
    expect_null(attr(shared, "margot_full_fits"))
  })
})

test_that("TRUE with a reason proceeds, reports the reason and records it", {
  with_sequential_plan({
    reason <- "inspect learner predictions in a test"
    defaults <- full_fits_defaults(lmtp::lmtp_control(.return_full_fits = TRUE))
    expect_message(
      output <- margot_lmtp(
        data = full_fits_data(), outcome_vars = "perfectionism", trt = "exposure",
        shift_functions = list(down = shared_ratio_shift_down),
        include_null_shift = FALSE, lmtp_model_type = lmtp::lmtp_sdr,
        lmtp_defaults = defaults, progress = "none", seed = 2026L,
        allow_full_fits = reason
      ),
      reason,
      fixed = TRUE
    )
    expect_identical(
      attr(output, "margot_full_fits"),
      list(return_full_fits = TRUE, reason = reason)
    )
    model <- output$models$perfectionism$perfectionism_down
    expect_s3_class(model$fits_m[[1L]][[1L]], "SuperLearner")

    # the shared route keeps full fits too and carries the same record
    shared <- run_full_fits(defaults, reuse_density_ratios = TRUE, allow_full_fits = reason)
    expect_identical(attr(shared, "margot_full_fits")$reason, reason)
    shared_model <- shared$models$perfectionism$perfectionism_down
    expect_s3_class(shared_model$fits_m[[1L]][[1L]], "SuperLearner")

    # a reason with lmtp's default control changes nothing and records nothing
    lean <- run_full_fits(full_fits_defaults(), allow_full_fits = reason)
    expect_null(attr(lean, "margot_full_fits"))
  })
})

test_that("an estimator that checks the control itself receives the reason", {
  with_sequential_plan({
    received <- new.env()
    checking_estimator <- function(data, trt, outcome, shift, ..., allow_full_fits = NULL) {
      received$reason <- allow_full_fits
      lmtp::lmtp_sdr(data = data, trt = trt, outcome = outcome, shift = shift, ...)
    }
    reason <- "forwarded reason"
    output <- run_full_fits(
      full_fits_defaults(lmtp::lmtp_control(.return_full_fits = TRUE)),
      lmtp_model_type = checking_estimator,
      allow_full_fits = reason
    )
    expect_identical(received$reason, reason)
    expect_identical(attr(output, "margot_full_fits")$reason, reason)
  })
})

test_that("margot_lmtp_sdr_from_components refuses full fits before reading its inputs", {
  expect_error(
    margot_lmtp_sdr_from_components(
      data = NULL, trt = NULL, outcome = NULL, shift = NULL, components = NULL,
      baseline = NULL, time_vary = NULL, learners_outcome = NULL, bounds = NULL,
      control = lmtp::lmtp_control(.trim = 1, .return_full_fits = TRUE)
    ),
    class = "margot_error_full_fits_refused"
  )
  # with a reason the guard passes, and the invalid inputs fail afterwards
  error <- expect_error(suppressMessages(
    margot_lmtp_sdr_from_components(
      data = NULL, trt = NULL, outcome = NULL, shift = NULL, components = NULL,
      baseline = NULL, time_vary = NULL, learners_outcome = NULL, bounds = NULL,
      control = lmtp::lmtp_control(.trim = 1, .return_full_fits = TRUE),
      allow_full_fits = "testing the component route"
    )
  ))
  expect_false(inherits(error, "margot_error_full_fits_refused"))
})

test_that("an estimator named by string also receives the reason", {
  reason <- "forwarded to a named estimator"
  forwarded <- new.env()
  # a stub that records the reason it receives; the real call is not needed here
  assign("margot_test_named_estimator", function(data, trt, outcome, shift, ..., allow_full_fits = NULL) {
    forwarded$reason <- allow_full_fits
    stop("stop after recording the reason")
  }, envir = globalenv())
  withr::defer(rm("margot_test_named_estimator", envir = globalenv()))
  with_sequential_plan(suppressWarnings(try(run_full_fits(
    full_fits_defaults(lmtp::lmtp_control(.return_full_fits = TRUE)),
    lmtp_model_type = "margot_test_named_estimator",
    allow_full_fits = reason
  ), silent = TRUE)))
  expect_identical(forwarded$reason, reason)
})
