test_that("exposure odds preserve zeros and reject undefined ratios", {
  expect_equal(margot_exposure_odds(c(0, .5, .9, 1), c(FALSE, FALSE, FALSE, TRUE)), c(0, 1, 9, 0))
  expect_error(margot_exposure_odds(1, FALSE), "undefined")
  expect_error(margot_exposure_odds(NA_real_, FALSE), "invalid")
  expect_error(margot_exposure_odds(-.1, FALSE), "invalid")
})

test_that("paired validation keeps participants together and restores row order", {
  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  future::plan(future::sequential)
  n <- 100L
  d <- data.frame(a = rep(c(1, 1.5, 2, 4, 7), length.out = n), x = seq_len(n))
  id <- paste0("participant-", rev(seq_len(n)))
  reached <- rep(TRUE, n)
  reached[c(4, 19)] <- FALSE
  args <- list(data = d, exposure = "a", history = "x", shifted = pmax(1, d$a - 1),
    id = id, fold_id = as.integer(rep(1:5, length.out = n)), reached = reached,
    structural_zero = d$a > 6, learners = "SL.mean")
  set.seed(42)
  fit <- do.call(margot_lmtp_exposure_ratio, args)
  expect_identical(fit$id, id)
  expect_true(all(is.na(fit$ratio[!reached])))
  expect_equal(fit$ratio[reached & d$a > 6], rep(0, sum(reached & d$a > 6)))
  expect_equal(fit$ratio[reached & d$a <= 6], rep(1, sum(reached & d$a <= 6)))
  for (f in fit$fits) {
    expect_length(intersect(f$training_ids, f$validation_ids), 0)
    for (rows in f$inner_valid_rows) expect_length(intersect(f$stack_ids[rows], f$stack_ids[-rows]), 0)
  }
  args$data$a[1] <- Inf
  expect_error(do.call(margot_lmtp_exposure_ratio, args))
  args$data <- d
  args$id[2] <- args$id[1]
  expect_error(do.call(margot_lmtp_exposure_ratio, args))
})


test_that("lasso's fixed penalty folds are checked before dispatch", {
  d <- data.frame(a = rep(1:5, 4), x = seq_len(20))
  expect_error(margot_lmtp_exposure_ratio(d, "a", "x", pmax(1, d$a - 1),
    1:20, rep(1:2, each = 10), rep(TRUE, 20), learners = "SL.glmnet", inner_folds = 2L),
    "Insufficient reached participants")
})

test_that("nonconstant classification preserves grouped lasso folds across plans", {
  skip_on_cran()
  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  n <- 140L
  set.seed(42)
  d <- data.frame(a = rep(seq(1, 7, length.out = 20), 7), x = stats::rnorm(n))
  args <- list(data = d, exposure = "a", history = "x", shifted = pmax(1, d$a - 1),
    id = seq_len(n), fold_id = as.integer(rep(1:2, length.out = n)), reached = rep(TRUE, n),
    structural_zero = d$a > 6, learners = "SL.glmnet")
  future::plan(future::sequential)
  set.seed(42)
  serial <- do.call(margot_lmtp_exposure_ratio, args)
  future::plan(future::multisession, workers = 2L)
  set.seed(42)
  parallel <- do.call(margot_lmtp_exposure_ratio, args)
  expect_equal(parallel$probability, serial$probability, tolerance = 1e-12)
  expect_equal(parallel$ratio, serial$ratio, tolerance = 1e-12)
  expect_gt(mean(serial$ratio[d$a <= 2]), mean(serial$ratio[d$a >= 5 & d$a <= 6]))
  expect_gt(stats::sd(serial$probability), .01)
  for (fit in parallel$fits) {
    expect_length(fit$lasso_folds, 11L)
    for (membership in fit$lasso_folds) {
      expect_true(all(vapply(split(membership$fold, membership$id),
        function(x) length(unique(x)) == 1L, logical(1))))
      expect_setequal(unique(membership$fold), 1:10)
    }
  }
})
