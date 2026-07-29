# the trim-ladder gate and the shared support-status helper are defunct: the
# guide-architecture change of 29 July 2026 retired every mechanical pass/fail

make_fit <- function(dr) list(density_ratios = dr)

test_that("margot_lmtp_positivity_gate() is defunct", {
  set.seed(2026)
  tame <- matrix(stats::runif(300, 0.8, 1.25), ncol = 3)

  expect_error(
    margot_lmtp_positivity_gate(make_fit(tame), verbose = FALSE),
    class = "margot_error_defunct"
  )
})

test_that("the defunct gate error names the margot.lmtp replacement", {
  err <- tryCatch(
    margot_lmtp_positivity_gate(make_fit(matrix(1, nrow = 3, ncol = 2))),
    margot_error_defunct = function(e) e
  )
  expect_s3_class(err, "margot_error_defunct")
  expect_match(conditionMessage(err), "margot.lmtp", fixed = TRUE)
})

test_that("margot_positivity_support_status() is defunct", {
  expect_error(
    margot_positivity_support_status(0.01, margot_positivity_thresholds(NULL)),
    class = "margot_error_defunct"
  )
})

test_that("weight diagnostic reports trim_mass_share within [0, 1]", {
  set.seed(7)
  dr <- matrix(stats::rlnorm(600, meanlog = 0, sdlog = 1), ncol = 2)
  diag <- margot_lmtp_weight_diag_from_fit(make_fit(dr), trim_right = 0.98)

  expect_true("trim_mass_share" %in% names(diag$wave_table))
  share <- diag$wave_table$trim_mass_share
  expect_true(all(share >= 0 & share <= 1, na.rm = TRUE))
  expect_true(any(share > 0))
})
