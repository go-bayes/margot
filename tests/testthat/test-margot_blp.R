# tests for the registered best linear projection reporting surface
# (margot_blp, margot_table_blp, margot_plot_blp). fixtures fit real small
# causal forests once and reuse them, so the whole file runs in a few seconds.

blp_fixture <- local({
  set.seed(2026)
  n <- 500
  p <- 4
  x <- matrix(stats::rnorm(n * p), ncol = p)
  colnames(x) <- paste0("x", seq_len(p))
  w <- stats::rbinom(n, 1, 0.5)
  # y1 carries treatment effect heterogeneity in x1; y2 a constant effect
  y1 <- x[, 1] + 0.3 * w * x[, 1] + stats::rnorm(n)
  y2 <- x[, 2] - 0.2 * w + stats::rnorm(n)
  # non-uniform weights on the second forest only, so one fixture covers both
  # the weighted and the unweighted effective-sample-size branch
  sample_weights <- stats::runif(n, 0.5, 2)

  forest_y1 <- grf::causal_forest(x, y1, w, num.trees = 200, seed = 11)
  forest_y2 <- grf::causal_forest(
    x, y2, w,
    num.trees = 200, seed = 12, sample.weights = sample_weights
  )

  list(
    x = x,
    sample_weights = sample_weights,
    models = list(
      full_models = list(model_y1 = forest_y1, model_y2 = forest_y2),
      covariates = x
    )
  )
})

# purpose: a copy of the fixture with the named forests broken so their
# projection errors inside grf. inputs: model names to break. output: a models
# list shaped like a margot_causal_forest() result.
blp_break_models <- function(model_names) {
  models <- blp_fixture$models
  for (mn in model_names) {
    # dropping the treatment vector makes grf's internal data frame ragged;
    # X.orig is untouched, so the projection matrix still resolves
    models$full_models[[mn]]$W.orig <- NULL
  }
  models
}

test_that("margot_blp returns the documented tidy structure", {
  blp <- margot_blp(blp_fixture$models)

  expect_s3_class(blp, "margot_blp")
  expect_s3_class(blp, "data.frame")
  expect_named(blp, c(
    "outcome", "term", "estimate", "std_error", "conf_low", "conf_high",
    "target_sample", "n", "ess", "matrix_fingerprint", "status"
  ))
  expect_identical(unique(blp$outcome), c("y1", "y2"))
  expect_true(all(blp$status == "ok"))
  expect_true(all(blp$target_sample == "all"))
  expect_true(all(blp$n == 500L))
  # one fingerprint for the single projection matrix, 12 characters wide
  expect_length(unique(blp$matrix_fingerprint), 1L)
  expect_equal(nchar(blp$matrix_fingerprint[1]), 12L)
  expect_identical(
    attr(blp, "relativity_note"),
    margot:::.margot_blp_relativity_note()
  )
})

test_that("margot_blp reproduces grf::best_linear_projection exactly", {
  blp <- margot_blp(blp_fixture$models)

  for (mn in names(blp_fixture$models$full_models)) {
    outcome <- sub("^model_", "", mn)
    direct <- as.matrix(grf::best_linear_projection(
      blp_fixture$models$full_models[[mn]],
      A = blp_fixture$x,
      target.sample = "all"
    ))
    rows <- blp[blp$outcome == outcome, , drop = FALSE]

    # term names must match the grf coefficient table (intercept + covariates)
    expect_identical(rows$term, rownames(direct))
    expect_identical(rows$term, c("(Intercept)", colnames(blp_fixture$x)))
    expect_equal(rows$estimate, as.numeric(direct[, 1]), tolerance = 1e-10)
    expect_equal(rows$std_error, as.numeric(direct[, 2]), tolerance = 1e-10)

    # intervals are 95% normal approximations of the reported standard errors
    z <- stats::qnorm(0.975)
    expect_equal(rows$conf_low, rows$estimate - z * rows$std_error, tolerance = 1e-12)
    expect_equal(rows$conf_high, rows$estimate + z * rows$std_error, tolerance = 1e-12)
  }
})

test_that("target_sample = 'overlap' passes through to grf", {
  blp_all <- margot_blp(blp_fixture$models, model_names = "y1")
  blp_overlap <- margot_blp(blp_fixture$models, model_names = "y1", target_sample = "overlap")

  direct <- as.matrix(grf::best_linear_projection(
    blp_fixture$models$full_models$model_y1,
    A = blp_fixture$x,
    target.sample = "overlap"
  ))

  expect_true(all(blp_overlap$target_sample == "overlap"))
  expect_equal(blp_overlap$estimate, as.numeric(direct[, 1]), tolerance = 1e-10)
  expect_equal(blp_overlap$std_error, as.numeric(direct[, 2]), tolerance = 1e-10)
  # the two target samples weight the projection differently
  expect_false(isTRUE(all.equal(blp_overlap$estimate, blp_all$estimate)))
})

test_that("ess is the Kish effective sample size, and NA without weights", {
  # observed grf behaviour: an unweighted causal_forest stores no
  # sample.weights at all (the field is NULL), it does not store uniform ones
  expect_null(blp_fixture$models$full_models$model_y1$sample.weights)
  expect_equal(
    as.numeric(blp_fixture$models$full_models$model_y2$sample.weights),
    blp_fixture$sample_weights
  )

  blp <- margot_blp(blp_fixture$models)
  w <- blp_fixture$sample_weights
  expected_ess <- sum(w)^2 / sum(w^2)

  expect_true(all(is.na(blp$ess[blp$outcome == "y1"])))
  expect_equal(unique(blp$ess[blp$outcome == "y2"]), expected_ess, tolerance = 1e-12)
  expect_lt(expected_ess, 500)

  expect_identical(margot:::.margot_blp_ess(NULL), NA_real_)
  expect_identical(margot:::.margot_blp_ess(numeric(0)), NA_real_)
  expect_identical(margot:::.margot_blp_ess(c(NA_real_, NA_real_)), NA_real_)
  expect_equal(margot:::.margot_blp_ess(rep(2, 10)), 10)
})

test_that("model_names subsets with or without the model_ prefix", {
  bare <- margot_blp(blp_fixture$models, model_names = "y2")
  prefixed <- margot_blp(blp_fixture$models, model_names = "model_y2")

  expect_identical(unique(bare$outcome), "y2")
  expect_equal(bare$estimate, prefixed$estimate, tolerance = 1e-12)
  expect_identical(bare$term, prefixed$term)

  expect_error(
    margot_blp(blp_fixture$models, model_names = "nope"),
    "no fitted forest for: nope",
    fixed = TRUE
  )
})

test_that("margot_blp rejects malformed model objects", {
  expect_error(margot_blp("not a list"), "must be a list", fixed = TRUE)
  expect_error(margot_blp(list()), "no fitted forests found", fixed = TRUE)
  expect_error(margot_blp(list(full_models = list())), "no fitted forests found", fixed = TRUE)
})

test_that("a failed projection is isolated to one row and never aborts the batch", {
  models <- blp_break_models("model_y2")
  blp <- margot_blp(models)

  ok <- blp[blp$outcome == "y1", , drop = FALSE]
  failed <- blp[blp$outcome == "y2", , drop = FALSE]

  expect_true(all(ok$status == "ok"))
  expect_equal(nrow(ok), 5L)
  expect_false(any(is.na(ok$estimate)))

  expect_equal(nrow(failed), 1L)
  expect_true(startsWith(failed$status, "failed:"))
  expect_true(is.na(failed$term))
  expect_true(is.na(failed$estimate))
  expect_true(is.na(failed$std_error))
  expect_true(is.na(failed$conf_low))
  expect_true(is.na(failed$conf_high))
  expect_true(is.na(failed$n))
  # the weight-derived effective sample size survives a failed projection
  expect_false(is.na(failed$ess))
  expect_identical(failed$matrix_fingerprint, ok$matrix_fingerprint[1])

  # the equivalent unbroken outcome is unaffected by its neighbour's failure
  solo <- margot_blp(blp_fixture$models, model_names = "y1")
  expect_equal(ok$estimate, solo$estimate, tolerance = 1e-12)
})

test_that(".margot_blp_covariates enforces one shared projection matrix", {
  models <- blp_fixture$models
  forests <- models$full_models

  # a user matrix of the wrong shape
  expect_error(
    margot_blp(models, covariates = blp_fixture$x[, 1:2, drop = FALSE]),
    "but the forests were fitted on",
    fixed = TRUE
  )

  renamed <- blp_fixture$x
  colnames(renamed) <- paste0("z", seq_len(ncol(renamed)))
  expect_error(
    margot_blp(models, covariates = renamed),
    "different column names",
    fixed = TRUE
  )

  no_design <- models
  no_design$full_models$model_y1$X.orig <- NULL
  expect_error(
    margot_blp(no_design),
    "does not carry its design matrix",
    fixed = TRUE
  )

  mismatched <- models
  mismatched$full_models$model_y2$X.orig <- blp_fixture$x + 1
  expect_error(
    margot_blp(mismatched),
    "the covariate matrix differs across outcomes (first mismatch: y2)",
    fixed = TRUE
  )

  # a matrix that matches the fitted one in every respect is accepted and
  # yields the same fingerprint as the default resolution
  supplied <- margot_blp(models, covariates = blp_fixture$x, model_names = "y1")
  default <- margot_blp(models, model_names = "y1")
  expect_identical(supplied$matrix_fingerprint, default$matrix_fingerprint)
  expect_equal(supplied$estimate, default$estimate, tolerance = 1e-12)

  expect_identical(
    margot:::.margot_blp_covariates(models, forests, NULL),
    blp_fixture$x
  )
})

test_that("margot_table_blp formats estimates with intervals and the caption", {
  blp <- margot_blp(blp_fixture$models)
  tbl <- margot_table_blp(blp, digits = 2)

  expect_s3_class(tbl, "margot_blp_table")
  expect_named(tbl, c("outcome", "term", "estimate", "conf_low", "conf_high", "estimate_ci", "status"))
  expect_equal(nrow(tbl), nrow(blp))

  expect_equal(tbl$estimate, round(blp$estimate, 2))
  expect_equal(tbl$conf_low, round(blp$conf_low, 2))
  expect_equal(tbl$conf_high, round(blp$conf_high, 2))

  expected_ci <- paste0(
    formatC(blp$estimate, format = "f", digits = 2), " [",
    formatC(blp$conf_low, format = "f", digits = 2), ", ",
    formatC(blp$conf_high, format = "f", digits = 2), "]"
  )
  expect_identical(tbl$estimate_ci, expected_ci)
  # two decimal places on both sides of every interval
  expect_true(all(grepl("^-?[0-9]+\\.[0-9]{2} \\[-?[0-9]+\\.[0-9]{2}, -?[0-9]+\\.[0-9]{2}\\]$", tbl$estimate_ci)))

  # the relativity sentence is mandatory on every rendered table
  caption <- attr(tbl, "caption")
  expect_identical(caption, margot:::.margot_blp_relativity_note())
  expect_true(grepl("interpretable only", caption, fixed = TRUE))
  expect_true(grepl("not a causal effect of the covariate", caption, fixed = TRUE))

  # digits are honoured
  tbl3 <- margot_table_blp(blp, digits = 3)
  expect_true(all(grepl("^-?[0-9]+\\.[0-9]{3} \\[", tbl3$estimate_ci)))
})

test_that("margot_table_blp respects the requested outcome and term order", {
  blp <- margot_blp(blp_fixture$models)

  reordered <- margot_table_blp(blp, outcomes = c("y2", "y1"))
  expect_identical(unique(reordered$outcome), c("y2", "y1"))

  subset_out <- margot_table_blp(blp, outcomes = "y2")
  expect_identical(unique(subset_out$outcome), "y2")
  expect_equal(nrow(subset_out), 5L)

  picked <- margot_table_blp(blp, outcomes = c("y2", "y1"), terms = c("x3", "(Intercept)"))
  expect_identical(picked$outcome, c("y2", "y2", "y1", "y1"))
  expect_identical(picked$term, c("x3", "(Intercept)", "x3", "(Intercept)"))

  expect_error(margot_table_blp(blp, outcomes = "y9"), "no projection rows for outcome: y9", fixed = TRUE)
  expect_error(margot_table_blp(blp, terms = "x9"), "no projection rows for term: x9", fixed = TRUE)
  expect_error(margot_table_blp(as.data.frame(blp)), "must be a `margot_blp` object", fixed = TRUE)
})

test_that("margot_table_blp reports failed rows without formatting them", {
  blp <- margot_blp(blp_break_models("model_y2"))
  tbl <- margot_table_blp(blp)

  failed <- tbl[tbl$outcome == "y2", , drop = FALSE]
  expect_equal(nrow(failed), 1L)
  expect_true(is.na(failed$estimate_ci))
  expect_true(startsWith(failed$status, "failed:"))
  expect_false(any(is.na(tbl$estimate_ci[tbl$outcome == "y1"])))
})

test_that("filtering to zero rows errors", {
  # every named filter is validated before subsetting, so the empty case is
  # reached only with an already empty object
  blp <- margot_blp(blp_fixture$models)
  empty <- blp[0, , drop = FALSE]
  class(empty) <- c("margot_blp", "data.frame")

  expect_error(margot_table_blp(empty), "no rows remain after filtering", fixed = TRUE)
  expect_error(margot_plot_blp(empty), "no rows remain after filtering", fixed = TRUE)
})

test_that("margot_plot_blp returns a ggplot and drops rows without estimates", {
  blp <- margot_blp(blp_fixture$models)
  p <- margot_plot_blp(blp, title = "projection")

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), nrow(blp))
  expect_identical(p$labels$title, "projection")
  expect_true(grepl("interpretable only", p$labels$caption, fixed = TRUE))
  # terms read top-to-bottom, so the factor levels run in reverse
  expect_identical(levels(p$data$term), rev(c("(Intercept)", colnames(blp_fixture$x))))

  failed_blp <- margot_blp(blp_break_models("model_y2"))
  p_failed <- suppressMessages(margot_plot_blp(failed_blp))
  expect_s3_class(p_failed, "ggplot")
  # the failure row is dropped from the drawn data, not merely warned about
  expect_equal(nrow(p_failed$data), 5L)
  expect_false(any(is.na(p_failed$data$estimate)))
  expect_identical(as.character(unique(p_failed$data$outcome)), "y1")
})

test_that("margot_plot_blp errors when every retained row failed", {
  all_failed <- margot_blp(blp_break_models(c("model_y1", "model_y2")))
  expect_true(all(startsWith(all_failed$status, "failed:")))
  expect_equal(nrow(all_failed), 2L)

  expect_error(
    suppressMessages(margot_plot_blp(all_failed)),
    "every retained row failed",
    fixed = TRUE
  )
})

test_that("the projection recovers a known linear treatment effect surface", {
  skip_on_cran()

  set.seed(2026)
  n <- 2000
  p <- 4
  x <- matrix(stats::rnorm(n * p), ncol = p)
  colnames(x) <- paste0("x", seq_len(p))
  w <- stats::rbinom(n, 1, 0.5)
  # true conditional effect is linear in x1 alone
  tau <- 0.4 + 0.6 * x[, 1]
  y <- 0.5 * x[, 2] + tau * w + stats::rnorm(n)

  # treatment is randomised by construction, so the propensity is known
  forest <- grf::causal_forest(
    x, y, w,
    W.hat = rep(0.5, n), num.trees = 500, seed = 2026
  )
  models <- list(full_models = list(model_y = forest), covariates = x)
  blp <- margot_blp(models)

  x1 <- blp[blp$term == "x1", ]
  x2 <- blp[blp$term == "x2", ]

  # the x1 coefficient recovers the true slope of 0.6 within its own interval
  expect_lt(x1$conf_low, 0.6)
  expect_gt(x1$conf_high, 0.6)
  # x2 shifts the outcome but not the treatment effect, so its interval covers 0
  expect_lt(x2$conf_low, 0)
  expect_gt(x2$conf_high, 0)
})

test_that("the level argument sets the interval width and travels with the object", {
  blp95 <- margot_blp(blp_fixture$models, model_names = "y1")
  blp90 <- margot_blp(blp_fixture$models, model_names = "y1", level = 0.90)

  expect_identical(attr(blp95, "level"), 0.95)
  expect_identical(attr(blp90, "level"), 0.90)
  # only the interval multiplier moves; estimates and standard errors are fixed
  expect_equal(blp90$estimate, blp95$estimate, tolerance = 1e-12)
  expect_equal(blp90$std_error, blp95$std_error, tolerance = 1e-12)
  z90 <- stats::qnorm(0.95)
  expect_equal(blp90$conf_low, blp90$estimate - z90 * blp90$std_error, tolerance = 1e-12)
  expect_equal(blp90$conf_high, blp90$estimate + z90 * blp90$std_error, tolerance = 1e-12)
  expect_true(all(blp90$conf_high - blp90$conf_low <
                    blp95$conf_high - blp95$conf_low))

  # the plot axis label follows the recorded level
  expect_identical(margot_plot_blp(blp90)$labels$x, "Coefficient (90% CI)")
  expect_identical(margot_plot_blp(blp95)$labels$x, "Coefficient (95% CI)")

  expect_error(margot_blp(blp_fixture$models, level = 0), "strictly between 0 and 1", fixed = TRUE)
  expect_error(margot_blp(blp_fixture$models, level = 1), "strictly between 0 and 1", fixed = TRUE)
  expect_error(margot_blp(blp_fixture$models, level = c(0.9, 0.95)), "strictly between 0 and 1", fixed = TRUE)
})
