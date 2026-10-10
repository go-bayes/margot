# software tests for linearised inference on weighted lmtp estimates

make_ife <- function(eif, weights, id = seq_along(eif)) {
  weights <- weights / mean(weights)
  ife::ife(sum(weights * eif) / sum(weights), eif, weights, as.character(id))
}

hajek_se <- function(eif, weights) {
  theta <- sum(weights * eif) / sum(weights)
  sqrt(stats::var(weights * (eif - theta) / mean(weights)) / length(eif))
}

test_that("constant weights reproduce the standard error lmtp reports", {
  skip_if_not_installed("ife")
  set.seed(2026)
  eif <- 10 + stats::rnorm(200)
  est <- make_ife(eif, rep(1, 200))
  out <- margot_lmtp_linearised_inference(est)
  expect_equal(out$estimate, est@x)
  expect_equal(out$std_error, est@std_error, tolerance = 1e-12)
  expect_equal(out$package_std_error, est@std_error)
  expect_true(is.na(out$p_value))
})

test_that("varying weights use the linearised weighted mean", {
  skip_if_not_installed("ife")
  set.seed(2027)
  eif <- 10 + stats::rnorm(400)
  weights <- rep(c(0.2, 1.8), 200)
  est <- make_ife(eif, weights)
  out <- margot_lmtp_linearised_inference(est)
  expect_equal(out$estimate, sum(weights * eif) / sum(weights))
  expect_equal(out$std_error, hajek_se(eif, weights), tolerance = 1e-12)
  # the package centres the weighted product, which inflates this raw-scale mean
  expect_gt(out$package_std_error / out$std_error, 4)
})

test_that("the standard error does not depend on the scale of the weights", {
  skip_if_not_installed("ife")
  set.seed(2028)
  eif <- stats::rnorm(300, mean = 3)
  weights <- stats::runif(300, 0.3, 3)
  normalised <- ife::ife(sum(weights * eif) / sum(weights), eif, weights / mean(weights),
    as.character(seq_along(eif)))
  scaled <- ife::ife(sum(weights * eif) / sum(weights), eif, 7 * weights / mean(weights),
    as.character(seq_along(eif)))
  expect_equal(margot_lmtp_linearised_inference(scaled)$std_error,
    margot_lmtp_linearised_inference(normalised)$std_error, tolerance = 1e-12)
})

test_that("additive contrasts pair pseudo-outcomes within participants", {
  skip_if_not_installed("ife")
  set.seed(2029)
  base <- 10 + stats::rnorm(500)
  shifted <- base + 0.4 + 0.5 * stats::rnorm(500)
  weights <- stats::runif(500, 0.2, 2.5)
  fit <- make_ife(shifted, weights)
  ref <- make_ife(base, weights)
  out <- margot_lmtp_linearised_inference(fit, ref = ref)
  difference <- shifted - base
  expect_equal(out$estimate, fit@x - ref@x)
  expect_equal(out$std_error, hajek_se(difference, weights), tolerance = 1e-12)
  expect_equal(out$package_std_error, (fit - ref)@std_error, tolerance = 1e-12)
  expect_equal(out$p_value, 2 * stats::pnorm(-abs(out$estimate / out$std_error)))
  expect_equal(c(out$conf_low, out$conf_high),
    out$estimate + c(-1, 1) * stats::qnorm(0.975) * out$std_error)
})

test_that("repeated identifiers use cluster-robust sums of centred contributions", {
  skip_if_not_installed("ife")
  set.seed(2030)
  id <- rep(seq_len(60), each = 3)
  eif <- 5 + rep(stats::rnorm(60), each = 3) + stats::rnorm(180)
  weights <- rep(stats::runif(60, 0.5, 2), each = 3)
  est <- make_ife(eif, weights, id)
  out <- margot_lmtp_linearised_inference(est)
  w <- weights / mean(weights)
  contribution <- w * (eif - est@x)
  sums <- as.numeric(rowsum(contribution, as.character(id), reorder = FALSE))
  expect_equal(out$n_clusters, 60L)
  expect_equal(out$std_error, sqrt(stats::var(sums * 60 / 180) / 60), tolerance = 1e-12)
})

test_that("unequal clusters differ from the package value under constant weights", {
  skip_if_not_installed("ife")
  set.seed(2032)
  size <- rep(c(1, 2, 5), length.out = 90)
  id <- rep(seq_along(size), times = size)
  eif <- 8 + stats::rnorm(length(id))
  est <- make_ife(eif, rep(1, length(id)), id)
  out <- margot_lmtp_linearised_inference(est)
  contribution <- eif - est@x
  sums <- as.numeric(rowsum(contribution, as.character(id), reorder = FALSE))
  expect_equal(out$std_error, sqrt(stats::var(sums * 90 / length(id)) / 90), tolerance = 1e-12)
  expect_gt(abs(out$package_std_error / out$std_error - 1), 0.1)
})

test_that("contrasts require identical weights and identifiers", {
  skip_if_not_installed("ife")
  eif <- stats::rnorm(50)
  fit <- make_ife(eif, rep(1, 50))
  ref <- make_ife(eif, rep(c(1, 2), 25))
  expect_error(margot_lmtp_linearised_inference(fit, ref = ref), "identical weights")
  expect_error(margot_lmtp_linearised_inference(list(estimate = NULL)), "carries no")
  expect_error(margot_lmtp_linearised_inference(fit, conf_level = 1.2), "conf_level")
})

test_that("an lmtp sdr fit is read through its estimate element", {
  skip_on_cran()
  skip_if_not_installed("lmtp")
  set.seed(2031)
  n <- 300
  w1 <- stats::rnorm(n)
  a <- stats::rbinom(n, 1, stats::plogis(0.4 * w1))
  y <- stats::rnorm(n, 1 + a + 0.5 * w1)
  data <- data.frame(w1 = w1, a = a, y = y)
  weights <- stats::runif(n, 0.3, 3)
  fit <- suppressWarnings(lmtp::lmtp_sdr(data, trt = "a", outcome = "y", baseline = "w1",
    shift = lmtp::static_binary_on, outcome_type = "continuous", folds = 2,
    learners_trt = "SL.glm", learners_outcome = "SL.glm", weights = weights))
  out <- margot_lmtp_linearised_inference(fit)
  expect_equal(out$estimate, fit$estimate@x)
  expect_equal(out$std_error,
    hajek_se(fit$estimate@eif, fit$estimate@weights), tolerance = 1e-10)
})
