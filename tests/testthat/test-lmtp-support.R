# invented probabilities and deterministic learner substitutes; no model fitting.

support_fixture <- function() {
  ids <- c("a", "b")
  g <- matrix(c(.2, .5, .1, .2, .2, .5, .1, .2), 2, byrow = TRUE,
    dimnames = list(ids, c("zero", "low", "threshold", "high")))
  q <- g * 0
  q[, "threshold"] <- rowSums(g[, c("zero", "low", "threshold")])
  q[, "high"] <- g[, "high"]
  list(ids = ids, g = g, q = q, w = setNames(c(1, 1), ids),
    reached = setNames(c(TRUE, TRUE), ids))
}

test_that("conditional concentration uses the supplied threshold and IPC measure", {
  x <- support_fixture()
  assess <- function(...) margot_lmtp_support_concentration(x$g, x$q, x$w,
    x$reached, x$ids, ...)
  value <- assess(minimum = .05)
  expect_equal(value$fraction, 1 / 6.6)
  expect_equal(value$equivalent, 2 / 6.6)
  expect_identical(value$state, "pass")
  expect_identical(assess(minimum = value$fraction)$state, "pass")
  expect_identical(assess(minimum = value$fraction + .01)$state, "fail")
  expect_identical(assess(minimum = .05, observation_weight = x$w), value)
  for (minimum in list(0, -1, 2, NA_real_, Inf, c(.05, .1), "0.05")) {
    expect_error(assess(minimum = minimum), "minimum")
  }
  x$g <- cbind(zero = c(.5, .04), positive = c(.5, .96))
  rownames(x$g) <- x$ids
  x$q <- x$g * 0
  x$q[, "zero"] <- 1
  correction <- setNames(c(.2, .8), x$ids)
  expect_equal(assess(minimum = .05)$fraction, 2 / (1/.5 + 1/.04))
  expect_equal(assess(minimum = .05, observation_weight = correction)$fraction,
    1 / (.2/.5 + .8/.04))
  expect_identical(assess(minimum = .05, observation_weight = correction)$state, "fail")
})

test_that("zero prior weights remain in the averaging population", {
  x <- support_fixture()
  x$w[1] <- 0
  x$g[1, ] <- c(1, 0, 0, 0)
  x$q[1, ] <- c(0, 0, 1, 0)
  result <- margot_lmtp_support_concentration(x$g, x$q, x$w, x$reached, x$ids,
    minimum = .05, observation_weight = setNames(c(.8, .2), x$ids))
  expect_identical(result$state, "fail")
  expect_equal(result$fraction, .2 / 6.6)
  expect_identical(result$n, 2L)
  x$w[1] <- 1
  boundary <- margot_lmtp_support_concentration(x$g, x$q, x$w, x$reached, x$ids,
    minimum = .05)
  expect_identical(boundary$state, "boundary_fail")
  expect_true(is.na(boundary$fraction))
  expect_identical(boundary$boundary_records, 1L)
})

test_that("concentration rejects misaligned and invalid inputs", {
  x <- support_fixture()
  run <- function(g = x$g, q = x$q, w = x$w, reached = x$reached,
                  ids = x$ids, observation = NULL) {
    margot_lmtp_support_concentration(g, q, w, reached, ids,
      minimum = .05, observation_weight = observation)
  }
  expect_identical(run(g = x$g[2:1, ])$state, "unassessable")
  expect_identical(run(ids = c("a", "a"))$state, "unassessable")
  expect_identical(run(w = c(1, 1))$state, "unassessable")
  expect_identical(run(reached = setNames(c(FALSE, FALSE), x$ids))$state, "unassessable")
  g <- x$g
  g[1, 1] <- NA_real_
  expect_identical(run(g = g)$state, "unassessable")
  expect_identical(run(g = g, reached = setNames(c(FALSE, TRUE), x$ids))$state, "pass")
  expect_identical(run(observation = setNames(c(1, 0), x$ids))$state, "unassessable")
  expect_identical(run(w = setNames(c(1e-300, 1), x$ids))$state, "unassessable")
})

test_that("observation correction is stable and retains every reached record", {
  ids <- c("a", "b", "c")
  reached <- setNames(c(TRUE, TRUE, FALSE), ids)
  p <- matrix(c(1, .5, NA, 1, .5, NA), 3,
    dimnames = list(ids, c("c1", "c2")))
  result <- margot_lmtp_support_weights(p, reached, ids)
  expect_identical(result$state, "assessable")
  expect_equal(unname(result$weight), c(.2, .8, NA))
  expect_equal(result$observation_effective_n, 1 / (.2^2 + .8^2))
  p[1:2, ] <- 1e-250
  expect_equal(unname(margot_lmtp_support_weights(p, reached, ids)$weight), c(.5, .5, NA))
  p[1, ] <- 1
  expect_identical(margot_lmtp_support_weights(p, reached, ids)$state, "unassessable")
  p[1, ] <- 0
  expect_identical(margot_lmtp_support_weights(p, reached, ids)$state, "unassessable")
  expect_identical(margot_lmtp_support_weights(p, unname(reached), ids)$state, "unassessable")
})

test_that("prefix propagation uses only required observed ratios", {
  x <- support_fixture()
  observed <- setNames(c("threshold", "low"), x$ids)
  run <- function(propagate = TRUE, excluded = c("zero", "low")) {
    margot_lmtp_support_prefix(x$g, x$q, observed, x$w, x$reached, x$ids,
      minimum = .05, excluded = excluded, propagate = propagate)
  }
  value <- run()
  expect_identical(value$state, "pass")
  expect_equal(unname(value$realised_ratio), c(8, 0))
  expect_identical(value$next_weight, value$realised_ratio)
  expect_null(run(FALSE)$next_weight)
  expect_identical(run(excluded = "threshold")$state, "unassessable")
  x$g[, ] <- rep(c(.2, .5, .3, 0), each = 2)
  x$q[, ] <- rep(c(0, 0, 1, 0), each = 2)
  observed[1] <- "high"
  expect_identical(run()$state, "unassessable")
  expect_identical(run(FALSE)$state, "pass")
  x$w[1] <- 0
  expect_identical(run()$state, "pass")
  expect_equal(unname(run()$next_weight), c(0, 0))
})

test_that("constant probability paths need no learner fit", {
  run <- function(y) margot_lmtp_support_probability(y, data.frame(x = seq_along(y)),
    data.frame(x = 1:2), as.character(seq_along(y)), c("SL.mean", "SL.glm"),
    inner_folds = 3L, seed = 42L)
  expect_identical(run(numeric())$state, "constant")
  expect_equal(run(numeric())$probability, c(0, 0))
  expect_equal(run(c(1, 1))$probability, c(1, 1))
  expect_null(run(c(0, 0))$fit)
  expect_identical(run(c(0, 1))$state, "unassessable")
  expect_error(run(c(0, 2)))
})

test_that("probability fitting preserves settings and returns a lean diagnostic record", {
  called <- NULL
  fake_fit <- function(Y, X, newX, family, SL.library, method, id, cvControl, control, env) {
    called <<- list(y = Y, x = X, new_x = newX, learners = SL.library,
      method = method, id = id, inner = cvControl$V, control = control, random = runif(1))
    warning("retained learner warning")
    structure(list(SL.predict = c(-1e-13, 1 + 1e-13),
      library.predict = matrix(c(0, 1, .2, .8), 2),
      coef = c(SL.mean_All = .4, SL.glm_All = .6), cvRisk = c(.3, .2),
      errorsInLibrary = c(FALSE, FALSE), errorsInCVLibrary = matrix(FALSE, 2, 2),
      validRows = list(1:2, 3:4), library = list(large = raw(100))),
      class = "SuperLearner")
  }
  local_mocked_bindings(SuperLearner = fake_fit, .package = "SuperLearner")
  y <- c(0, 1, 0, 1)
  x <- data.frame(x = 1:4)
  new_x <- data.frame(x = 5:6)
  value <- margot_lmtp_support_probability(y, x, new_x, letters[1:4],
    c("SL.mean", "SL.glm"), inner_folds = 2L, seed = 42L)
  expect_identical(value$state, "fitted")
  expect_equal(value$probability, c(0, 1))
  expect_identical(value$warnings, "retained learner warning")
  expect_identical(called$y, y)
  expect_identical(called$learners, c("SL.mean", "SL.glm"))
  expect_identical(called$control, list(saveFitLibrary = TRUE))
  expect_identical(called$inner, 2L)
  set.seed(42L)
  expect_identical(called$random, runif(1))
  expect_s3_class(value$fit, "margot_lmtp_lean_fit")
  expect_equal(value$fit$coefficient, c(.4, .6))
  expect_identical(value$fit$inner_fold_sizes, c(2L, 2L))
  expect_invisible(margot_lmtp_assert_lean(value))
})

test_that("failed learners and invalid predictions remain unassessable", {
  fake_fit <- function(...) structure(list(SL.predict = c(.2, .8),
    library.predict = matrix(c(.2, .8), 2),
    coef = c(SL.mean_All = 1), cvRisk = .3,
    errorsInLibrary = TRUE, errorsInCVLibrary = FALSE,
    validRows = list(1:2, 3:4)), class = "SuperLearner")
  local_mocked_bindings(SuperLearner = fake_fit, .package = "SuperLearner")
  run <- function() margot_lmtp_support_probability(c(0, 1, 0, 1),
    data.frame(x = 1:4), data.frame(x = 5:6), letters[1:4],
    "SL.mean", inner_folds = 2L, seed = 42L)
  expect_identical(run()$state, "unassessable")
  expect_match(run()$reason, "requested learner failed")
})
