test_that("margot_lmtp_weight_diag_from_fit builds mask and ESS", {
  dr <- matrix(c(2, 0, 1,
                 1.5, 0.5, 2,
                 3,  0,   4), nrow = 3, byrow = TRUE)
  fit <- list(density_ratios = dr)
  diag <- margot_lmtp_weight_diag_from_fit(fit, trim_right = 0.9, thresholds = c(2, 3))

  expect_true(all(dim(diag$mask_from_fit) == dim(dr)))
  expect_equal(diag$mask_from_fit, dr > 0)
  expect_equal(nrow(diag$wave_table), ncol(dr))
  expect_true(all(c("ess_cum_raw", "ess_cum_trim") %in% names(diag$wave_table)))
})
