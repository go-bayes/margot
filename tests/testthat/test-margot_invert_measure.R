test_that("margot_invert_measure works with z-score method", {
  # standardized data
  x_raw <- c(1, 2, 3, 4, 5)
  x <- scale(x_raw)[, 1]  # standardize first
  x_flipped <- margot_invert_measure(x, method = "zscore")
  
  # check that it's simply negated
  expect_equal(x_flipped, -x)
  
  # check that order is reversed
  expect_true(cor(x, x_flipped) == -1)
  
  # test with non-standardized data (should warn)
  expect_message(
    margot_invert_measure(x_raw, method = "zscore"),
    "Data may not be standardized"
  )
})

test_that("margot_invert_measure works with ordinal method and known bounds", {
  # likert scale data
  likert <- c(1, 2, 3, 4, 5, 3, 2)
  likert_flipped <- margot_invert_measure(likert, method = "ordinal", scale_bounds = c(1, 5))
  
  # check the inversion formula: x_flipped = (max + min) - x
  expected <- (5 + 1) - likert
  expect_equal(likert_flipped, expected)
  
  # check specific mappings
  expect_equal(likert_flipped[likert == 1], 5)
  expect_equal(likert_flipped[likert == 5], 1)
  expect_true(all(likert_flipped[likert == 3] == 3))  # midpoint stays same
})

test_that("margot_invert_measure infers bounds correctly", {
  x <- c(2, 4, 6, 8, 10)
  x_flipped <- margot_invert_measure(x, method = "ordinal", scale_bounds = NULL)
  
  # should infer bounds as [2, 10]
  expected <- (10 + 2) - x
  expect_equal(x_flipped, expected)
})

test_that("margot_invert_measure handles NA values", {
  x <- c(1, 2, NA, 4, 5)
  
  # z-score method
  x_flipped_z <- margot_invert_measure(x, method = "zscore")
  expect_true(is.na(x_flipped_z[3]))
  expect_equal(length(x_flipped_z), length(x))
  
  # ordinal method
  x_flipped_ord <- margot_invert_measure(x, method = "ordinal", scale_bounds = c(1, 5))
  expect_true(is.na(x_flipped_ord[3]))
  expect_equal(x_flipped_ord[!is.na(x_flipped_ord)], c(5, 4, 2, 1))
})

test_that("margot_invert_measure handles edge cases", {
  # all same values - no variation
  x_const <- rep(0, 10)  # use 0 for standardized constant
  x_flipped <- margot_invert_measure(x_const, method = "zscore")
  expect_equal(x_flipped, -x_const)  # should just negate
  
  # single value
  x_single <- 5
  x_flipped <- margot_invert_measure(x_single, method = "ordinal", scale_bounds = c(1, 10))
  expect_equal(x_flipped, 6)  # (10 + 1) - 5 = 6
  
  # all NA
  x_na <- rep(NA_real_, 5)
  x_flipped <- margot_invert_measure(x_na, method = "zscore")
  expect_equal(x_flipped, x_na)
})

test_that("margot_invert_measure validates inputs correctly", {
  # non-numeric input
  expect_error(
    margot_invert_measure("not numeric"),
    "x must be numeric"
  )
  
  # invalid bounds
  expect_error(
    margot_invert_measure(1:5, method = "ordinal", scale_bounds = c(5, 1)),
    "scale_bounds\\[1\\] must be less than scale_bounds\\[2\\]"
  )
  
  # wrong length bounds
  expect_error(
    margot_invert_measure(1:5, method = "ordinal", scale_bounds = c(1)),
    "scale_bounds must be a vector of length 2"
  )
})

test_that("margot_invert_measure warns about out-of-bounds values", {
  x <- c(0, 1, 2, 3, 4, 5, 6)  # 0 and 6 are out of bounds
  
  expect_message(
    margot_invert_measure(x, method = "ordinal", scale_bounds = c(1, 5)),
    "Some values are outside specified bounds"
  )
})
