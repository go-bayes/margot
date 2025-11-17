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

test_that("margot_lmtp_weight_diag_from_fit respects shift order and wave labels", {
  dr1 <- matrix(c(2, 1, 1,
                  1, 1, 0), nrow = 2, byrow = TRUE)
  dr2 <- matrix(c(1, 1, 1,
                  2, 2, 2), nrow = 2, byrow = TRUE)
  fit <- list(
    models = list(
      outcome = list(
        outcome_null = list(density_ratios = dr1),
        outcome_shift_zero = list(density_ratios = dr2)
      )
    )
  )
  lbl <- list(wave_labels = list(`1` = "Baseline", wave_2 = "Year 1"))
  diags <- margot_lmtp_weight_diag_from_fit(
    fit,
    outcome = "outcome",
    shifts = c("shift_zero", "null"),
    label_mapping = lbl
  )
  expect_true(is.list(diags))
  expect_length(diags, 2)
  expect_equal(names(diags), c("shift_zero", "null"))
  expect_true(all(c("wave", "wave_label") %in% names(diags[[1]]$wave_table)))
  expect_equal(diags[[1]]$wave_table$wave_label[1], "Baseline")
})
