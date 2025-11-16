test_that("lab palette exposes anchor colours", {
  pal <- margot_palette("lab")
  expect_true(all(c("null", "weekly", "monthly", "ipsi_02", "ipsi_05", "ipsi_10") %in% names(pal)))
})

test_that("lab palette resolver detects common shift names", {
  pal <- margot_palette("lab")
  resolve <- margot_palette_lab_resolve

  expect_equal(resolve("null", pal), unname(pal["null"]))
  expect_equal(resolve("t5_self_null", pal), unname(pal["null"]))
  expect_equal(resolve("shift_weekly", pal), unname(pal["weekly"]))
  expect_equal(resolve("MONTHLY_policy", pal), unname(pal["monthly"]))
  expect_equal(resolve("ipsi_05", pal), unname(pal["ipsi_05"]))
  expect_equal(resolve("t3_pwi_ipsi_10", pal), unname(pal["ipsi_10"]))
  expect_equal(resolve("ipsi_12", pal), unname(pal["ipsi_10"]))
  expect_equal(resolve("shift_down_extra", pal), unname(pal["shift_down"]))
  expect_equal(resolve("CONSTANT_SHIFT", pal), unname(pal["constant"]))
  expect_equal(resolve(c("null", "shift_up"), pal), unname(pal[c("null", "shift_up")]))
  expect_equal(resolve("unknown_shift", pal, default = "fallback"), "fallback")
})
