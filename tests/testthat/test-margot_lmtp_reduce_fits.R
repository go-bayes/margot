# the fixtures live in helper-lmtp-shared.R.

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
  margot_lmtp_assert_lean(lean)
})

test_that("reducing full fits covers every fold and time point", {
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
  expect_identical(margot_lmtp_reduce_fits(fit(TRUE)), fit(FALSE))
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
