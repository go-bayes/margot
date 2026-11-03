# generated observation masks and probabilities only.

# the ARC study calculation this report generalises, kept verbatim for comparison.
mqi_reference <- function(reached_matrix, observed_matrix, probability_matrix) {
  do.call(rbind, lapply(seq_len(ncol(reached_matrix)), function(j) {
    reached <- reached_matrix[, j]
    y <- as.numeric(observed_matrix[reached, j])
    p <- probability_matrix[reached, j]
    data.frame(reached = sum(reached), continued = sum(y),
      brier = mean((p - y)^2), log_loss = mean(-ifelse(y == 1, log(p), log1p(-p))))
  }))
}

test_that("a small example matches hand calculation", {
  reached <- cbind(c(TRUE, TRUE, TRUE, TRUE), c(TRUE, TRUE, FALSE, FALSE))
  observed <- cbind(c(TRUE, TRUE, FALSE, FALSE), c(TRUE, FALSE, FALSE, FALSE))
  probability <- cbind(c(0.8, 0.6, 0.4, 0.2), c(0.5, 0.25, 0.9, NA))
  report <- margot_lmtp_observation_report(reached, observed, probability, wave_labels = c("w1", "w2"))
  expect_s3_class(report, "margot_lmtp_observation_report")
  expect_identical(report$wave, c("w1", "w2"))
  expect_identical(report$reached, c(4L, 2L))
  expect_identical(report$continued, c(2L, 1L))
  # wave 1: squared errors 0.04, 0.16, 0.16, 0.04.
  expect_equal(report$brier[[1]], (0.04 + 0.16 + 0.16 + 0.04) / 4)
  expect_equal(report$log_loss[[1]], -(log(0.8) + log(0.6) + log(0.6) + log(0.8)) / 4)
  # wave 2: only the first two records reached; the third record's 0.9 is ignored.
  expect_equal(report$brier[[2]], (0.25 + 0.0625) / 2)
  expect_equal(report$log_loss[[2]], -(log(0.5) + log(0.75)) / 2)
  expect_identical(report$n_infinite_log_loss, c(0L, 0L))
})

test_that("valid random inputs reproduce the study calculation exactly", {
  set.seed(20261104)
  n <- 500
  waves <- 4
  reached <- matrix(FALSE, n, waves)
  observed <- matrix(NA, n, waves)
  probability <- matrix(NA_real_, n, waves)
  at_risk <- rep(TRUE, n)
  for (j in seq_len(waves)) {
    reached[, j] <- at_risk
    probability[, j] <- stats::plogis(stats::rnorm(n, 1.5, 1))
    observed[at_risk, j] <- stats::runif(sum(at_risk)) < probability[at_risk, j]
    at_risk <- at_risk & observed[, j] %in% TRUE
  }
  observed[!reached] <- FALSE
  report <- margot_lmtp_observation_report(reached, observed, probability)
  reference <- mqi_reference(reached, observed, probability)
  expect_identical(report$wave, paste0("Wave ", seq_len(waves)))
  expect_identical(report$brier, reference$brier)
  expect_identical(report$log_loss, reference$log_loss)
  expect_equal(report$reached, reference$reached)
  expect_equal(report$continued, reference$continued)
  # column names supply labels when present.
  colnames(reached) <- paste0("t", 1:4)
  expect_identical(margot_lmtp_observation_report(reached, observed, probability)$wave, paste0("t", 1:4))
})

test_that("probabilities of 0 and 1 are not clipped", {
  reached <- matrix(TRUE, 3, 1)
  # a correct certain prediction contributes zero loss.
  report <- margot_lmtp_observation_report(reached, matrix(c(TRUE, FALSE, TRUE), 3), matrix(c(1, 0, 0.5), 3))
  expect_equal(report$log_loss, -log(0.5) / 3)
  expect_identical(report$n_infinite_log_loss, 0L)
  # a contradicted certain prediction gives infinite loss and is counted.
  report <- margot_lmtp_observation_report(reached, matrix(c(TRUE, FALSE, TRUE), 3), matrix(c(0, 1, 0.5), 3))
  expect_identical(report$log_loss, Inf)
  expect_identical(report$n_infinite_log_loss, 2L)
  expect_equal(report$brier, (1 + 1 + 0.25) / 3)
})

test_that("a wave no record reached has zero counts and missing scores", {
  reached <- cbind(c(TRUE, TRUE), c(FALSE, FALSE))
  observed <- cbind(c(FALSE, FALSE), c(FALSE, FALSE))
  report <- margot_lmtp_observation_report(reached, observed, matrix(0.5, 2, 2))
  expect_identical(report$reached[[2]], 0L)
  expect_true(is.na(report$brier[[2]]) && is.na(report$log_loss[[2]]))
})

test_that("inputs are validated", {
  reached <- matrix(TRUE, 3, 2)
  observed <- matrix(TRUE, 3, 2)
  probability <- matrix(0.5, 3, 2)
  report <- function(...) {
    args <- utils::modifyList(list(reached = reached, observed = observed, probability = probability), list(...))
    do.call(margot_lmtp_observation_report, args)
  }
  expect_error(report(reached = 1 * reached), "`reached` must be a logical matrix")
  expect_error(report(reached = c(TRUE, TRUE, TRUE)), "`reached` must be a logical matrix")
  expect_error(report(observed = 1 * observed), "observed == 1")
  expect_error(report(probability = matrix("0.5", 3, 2)), "numeric matrix")
  expect_error(report(observed = matrix(TRUE, 2, 2)), "same dimensions")
  expect_error(report(probability = matrix(0.5, 3, 3)), "same dimensions")
  expect_error(report(reached = matrix(logical(), 0, 2), observed = matrix(logical(), 0, 2),
    probability = matrix(numeric(), 0, 2)), "at least one row")
  expect_error(report(reached = replace(reached, 1, NA)), "`reached` must not contain missing")
  expect_error(report(observed = replace(observed, 1, NA)), "`observed` must not be missing")
  expect_error(report(reached = replace(reached, 1, FALSE)), "must not be true where `reached` is false")
  expect_error(report(probability = replace(probability, 1, NA)), "`probability` must not be missing")
  expect_error(report(probability = replace(probability, 1, 1.01)), "between 0 and 1|\\[0, 1\\]")
  expect_error(report(probability = replace(probability, 1, -0.01)), "\\[0, 1\\]")
  expect_error(report(wave_labels = "one"), "wave_labels")
  expect_error(report(wave_labels = c("a", "a")), "wave_labels")
  # values outside reached records are ignored.
  expect_s3_class(report(reached = replace(reached, 1, FALSE), observed = replace(observed, 1, FALSE),
    probability = replace(probability, 1, NA)), "margot_lmtp_observation_report")
})
