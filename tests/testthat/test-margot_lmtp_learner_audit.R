# the fixtures live in helper-lmtp-shared.R.

audited_models <- function(output) unlist(output$models, recursive = FALSE)

test_that("reducing full fits reproduces lmtp's default lean model exactly", {
  data <- make_shared_ratio_data()
  fit <- function(full) {
    set.seed(3031L)
    do.call(lmtp::lmtp_sdr, c(list(data = data, trt = "exposure", outcome = "perfectionism",
      shift = shared_ratio_shift_down, control = lmtp::lmtp_control(.return_full_fits = full)),
      shared_ratio_defaults()))
  }
  lean <- fit(FALSE)
  full <- fit(TRUE)
  expect_error(margot_lmtp_assert_lean(full))
  expect_identical(margot_lmtp_reduce_fits(full), lean)
  audit <- margot_lmtp_learner_audit(full)
  expect_s3_class(audit, "margot_lmtp_learner_audit")
  # two learners in each of one outcome time x two folds and one density time x two folds.
  expect_equal(nrow(audit$learners), 2L * (2L + 2L))
  expect_setequal(unique(audit$learners$stage), c("outcome", "density"))
  expect_true(all(is.finite(audit$learners$cv_risk)))
  # the audit's coefficients are the weights lmtp keeps in the lean model.
  outcome_rows <- audit$learners[audit$learners$stage == "outcome" & audit$learners$fold == 1L, ]
  expect_equal(outcome_rows$coefficient, unname(lean$fits_m[[1]][[1]]))
  expect_error(margot_lmtp_learner_audit(lean), "needs full fits")
})

test_that("learner_audit leaves every estimate unchanged on the per-fit route", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data()
  run <- function(audit) suppressMessages(margot_lmtp(data, outcome_vars = c("perfectionism", "distress"),
    trt = "exposure", shift_functions = list(down = shared_ratio_shift_down), include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_sdr, lmtp_defaults = shared_ratio_defaults(), seed = 3031L,
    progress = "none", learner_audit = audit))
  plain <- run(FALSE)
  audited <- run(TRUE)
  plain_models <- audited_models(plain)
  audited_models_list <- audited_models(audited)
  expect_identical(names(audited_models_list), names(plain_models))
  for (name in names(plain_models)) {
    audit <- audited_models_list[[name]]$learner_audit
    expect_s3_class(audit, "margot_lmtp_learner_audit")
    stripped <- audited_models_list[[name]]
    stripped$learner_audit <- NULL
    expect_identical(stripped, plain_models[[name]])
  }
  margot_lmtp_assert_lean(audited$models)
})

test_that("learner_audit leaves every estimate unchanged on the shared route", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  data <- make_shared_ratio_data()
  run <- function(audit) suppressMessages(margot_lmtp(data, outcome_vars = c("perfectionism", "distress"),
    trt = "exposure", shift_functions = list(down = shared_ratio_shift_down), include_null_shift = TRUE,
    lmtp_model_type = lmtp::lmtp_sdr, lmtp_defaults = shared_ratio_defaults(), seed = 3031L,
    progress = "none", reuse_density_ratios = TRUE, learner_audit = audit))
  plain <- audited_models(run(FALSE))
  audited <- audited_models(run(TRUE))
  for (name in names(plain)) {
    expect_s3_class(audited[[name]]$learner_audit, "margot_lmtp_learner_audit")
    stripped <- audited[[name]]
    stripped$learner_audit <- NULL
    expect_identical(stripped, plain[[name]])
  }
  margot_lmtp_assert_lean(audited)
})

test_that("learner_audit refuses requests it cannot honour", {
  data <- make_shared_ratio_data()
  defaults <- shared_ratio_defaults()
  call <- function(...) suppressMessages(margot_lmtp(data, outcome_vars = "perfectionism", trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down), include_null_shift = FALSE,
    progress = "none", learner_audit = TRUE, ...))
  full_defaults <- c(defaults, list(control = lmtp::lmtp_control(.return_full_fits = TRUE)))
  expect_error(call(lmtp_model_type = lmtp::lmtp_sdr, lmtp_defaults = full_defaults),
    class = "margot_error_learner_audit_route")
  custom <- function(data, trt, outcome, shift, ...) stop("not reached")
  expect_error(call(lmtp_model_type = custom, lmtp_defaults = defaults),
    class = "margot_error_learner_audit_route")
  expect_error(call(lmtp_model_type = lmtp::lmtp_sdr, lmtp_defaults = defaults,
    reuse_density_ratios = TRUE, manage_future_plan = TRUE),
    class = "margot_error_learner_audit_route")
  expect_error(suppressMessages(margot_lmtp(data, outcome_vars = "perfectionism", trt = "exposure",
    shift_functions = list(down = shared_ratio_shift_down), lmtp_model_type = lmtp::lmtp_sdr,
    lmtp_defaults = defaults, progress = "none", learner_audit = NA)), "learner_audit")
})

test_that("exposure ratios keep only learner records when retain_fits = FALSE", {
  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  future::plan(future::sequential)
  n <- 100L
  d <- data.frame(a = rep(c(1, 1.5, 2, 4, 7), length.out = n), x = seq_len(n) / n)
  args <- list(data = d, exposure = "a", history = "x", shifted = pmax(1, d$a - 1),
    id = seq_len(n), fold_id = as.integer(rep(1:5, length.out = n)), reached = rep(TRUE, n),
    learners = c("SL.mean", "SL.glm"))
  set.seed(42)
  full <- do.call(margot_lmtp_exposure_ratio, args)
  set.seed(42)
  lean <- do.call(margot_lmtp_exposure_ratio, c(args, list(retain_fits = FALSE)))
  expect_identical(lean$ratio, full$ratio)
  expect_identical(lean$probability, full$probability)
  expect_s3_class(lean$fits[[1]]$model, "margot_lmtp_lean_fit")
  expect_equal(lean$fits[[1]]$model$coefficient, unname(full$fits[[1]]$model$coef))
  expect_error(margot_lmtp_assert_lean(full))
  margot_lmtp_assert_lean(lean)
})

test_that("the audit labels every fold and time point and checkpoints stay lean", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  set.seed(7)
  n <- 150L
  data <- data.frame(id = seq_len(n), w = stats::rnorm(n), a1 = stats::rnorm(n), l2 = stats::rnorm(n),
    a2 = stats::rnorm(n))
  data$y <- data$a1 + data$a2 + data$w + stats::rnorm(n)
  shift <- function(data, trt) data[[trt]] - 0.2
  defaults <- list(baseline = "w", time_vary = list(NULL, "l2"), outcome_type = "continuous", id = "id",
    folds = 2L, learners_trt = c("SL.glm", "SL.mean"), learners_outcome = c("SL.glm", "SL.mean"))
  fit <- function(full) {
    set.seed(11L)
    do.call(lmtp::lmtp_sdr, c(list(data = data, trt = c("a1", "a2"), outcome = "y", shift = shift,
      control = lmtp::lmtp_control(.return_full_fits = full)), defaults))
  }
  lean <- fit(FALSE)
  audit <- margot_lmtp_learner_audit(fit(TRUE))
  # every (stage, fold, time) coefficient matches the lean model's fits[[fold]][[time]].
  stored <- list(outcome = lean$fits_m, density = lean$fits_r)
  keys <- unique(audit$learners[c("stage", "fold", "time")])
  expect_equal(nrow(keys), 2L * 2L * 2L)
  for (i in seq_len(nrow(keys))) {
    rows <- audit$learners[audit$learners$stage == keys$stage[i] & audit$learners$fold == keys$fold[i] &
      audit$learners$time == keys$time[i], ]
    expect_equal(rows$coefficient, unname(stored[[keys$stage[i]]][[keys$fold[i]]][[keys$time[i]]]))
  }
  destination <- tempfile("audited-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)
  output <- suppressMessages(margot_lmtp(data, outcome_vars = "y", trt = c("a1", "a2"),
    shift_functions = list(down = shift), include_null_shift = FALSE, lmtp_model_type = lmtp::lmtp_sdr,
    lmtp_defaults = defaults, seed = 11L, progress = "none", save_output = TRUE,
    save_path = destination, learner_audit = TRUE))
  saved <- list.files(destination, "[.]rds$", recursive = TRUE, full.names = TRUE)
  expect_true(length(saved) >= 1L)
  for (path in saved) margot_lmtp_assert_lean(readRDS(path), what = basename(path))
  expect_s3_class(output$models$y$y_down$learner_audit, "margot_lmtp_learner_audit")
})
