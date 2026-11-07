# generated data only; compare with the same direct SuperLearner call.

test_that("support predictions and diagnostics match a direct four-learner fit", {
  for (package in c("SuperLearner", "ranger", "xgboost", "glmnet")) {
    skip_if_not_installed(package)
  }
  withr::local_envvar(c(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
    R_RANGER_NUM_THREADS = "1"))
  set.seed(731L)
  x <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
  y <- rbinom(100, 1, plogis(.3 * x$a - .2 * x$b))
  new_x <- x[1:12, ]
  ids <- as.character(seq_along(y))
  learners <- c("SL.mean", "SL.ranger", "SL.xgboost", "SL.glmnet")
  set.seed(42L)
  expected <- SuperLearner::SuperLearner(Y = y, X = x, newX = new_x,
    family = stats::binomial(), SL.library = learners, method = "method.NNLS",
    id = ids, cvControl = list(V = 10L), control = list(saveFitLibrary = TRUE),
    env = environment(SuperLearner::SuperLearner))
  expected_seed <- .Random.seed
  actual <- margot_lmtp_support_probability(y, x, new_x, ids, learners, 10L, 42L)
  expect_identical(actual$state, "fitted")
  expect_identical(actual$probability, as.numeric(expected$SL.predict))
  expect_identical(actual$learner_probability, as.matrix(expected$library.predict))
  expect_identical(actual$fit$coefficient, unname(expected$coef))
  expect_identical(actual$fit$cv_risk, unname(expected$cvRisk))
  expect_identical(actual$fit$inner_validation_rows, expected$validRows)
  expect_identical(.Random.seed, expected_seed)
  expect_invisible(margot_lmtp_assert_lean(actual))
})
