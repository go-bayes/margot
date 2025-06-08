library(testthat)

test_that("wide output has expected structure and ordering", {
  set.seed(434)
  dat <- margot_simulate(
    n              = 200,
    waves          = 3,
    p_covars       = 2,
    item_missing_rate = 0,
    y_feedback     = 0.5,
    wide           = TRUE
  )
  
  # 1. correct number of rows and columns
  expect_equal(nrow(dat), 200)
  expect_equal(
    ncol(dat),
    1 +                 # id
      3 * (2 + 1 + 1) + # 3 waves * (2 covars + y + a)
      1                 # final y
  )
  
  # 2. columns ordered id, t0_l1, t0_l2, t0_y, t0_a, t1_*, t2_*, y
  expect_identical(
    names(dat)[1:5],
    c("id", "t0_l1", "t0_l2", "t0_y", "t0_a")
  )
  expect_true("y" %in% names(dat))
  expect_equal(names(dat)[ncol(dat)], "y")
  
  # 3. no censor indicator columns
  expect_false(any(grepl("_c$", names(dat))))
  
  # 4. check attribute
  expect_equal(attr(dat, "y_feedback"), 0.5)
})

test_that("long output contains per-wave and final outcomes", {
  dat_long <- margot_simulate(
    n        = 150,
    waves    = 4,
    p_covars = 1,
    wide     = FALSE,
    seed     = 123
  )
  
  expect_true(all(c("wave", "l1", "y", "a", "y_end") %in% names(dat_long)))
  expect_equal(max(dat_long$wave), 4)
  
  # "y" should be present (possibly NA) at all waves
  expect_equal(nrow(dat_long), 150 * 4)
  
  # y_end is non-NA only at final wave
  last_wave <- subset(dat_long, wave == 4)
  expect_gt(sum(!is.na(last_wave$y_end)), 0)   # some observed
  prev_wave <- subset(dat_long, wave < 4)
  expect_equal(sum(!is.na(prev_wave$y_end)), 0)
})

test_that("positivity scenarios work correctly", {
  # good positivity
  dat_good <- margot_simulate(n = 100, positivity = "good", seed = 1)
  
  # edge positivity
  dat_edge <- margot_simulate(n = 100, positivity = "edge", seed = 1)
  
  # violated positivity (deterministic)
  dat_viol <- margot_simulate(n = 100, positivity = "violated", seed = 1, wide = TRUE)
  # check that treatment is deterministic based on covariates
  # only check for non-missing values
  non_missing <- !is.na(dat_viol$t0_a) & !is.na(dat_viol$t0_l1)
  expect_true(all(dat_viol$t0_a[non_missing] == as.integer(dat_viol$t0_l1[non_missing] > 0.5)))
})

test_that("censoring mechanisms work", {
  # high censoring rate
  dat_censor <- margot_simulate(
    n = 200, 
    waves = 5,
    censoring = list(rate = 0.5),
    seed = 42
  )
  
  # check that some individuals are censored (have NA outcomes)
  n_complete <- sum(complete.cases(dat_censor))
  expect_lt(n_complete, 200)
  
  # exposure-dependent censoring
  dat_exp_censor <- margot_simulate(
    n = 200,
    waves = 3,
    censoring = list(
      rate = 0.2,
      exposure_dependence = TRUE
    ),
    seed = 43
  )
  expect_true(nrow(dat_exp_censor) > 0)
})

test_that("outcome types work correctly", {
  # binary outcomes
  dat_binary <- margot_simulate(
    n = 100, 
    outcome_type = "binary", 
    seed = 10
  )
  expect_true(all(dat_binary$y %in% c(0, 1, NA)))
  
  # continuous outcomes
  dat_cont <- margot_simulate(
    n = 100, 
    outcome_type = "continuous", 
    seed = 11
  )
  # check that outcomes are numeric and have decimals
  non_na_y <- dat_cont$y[!is.na(dat_cont$y)]
  expect_true(is.numeric(non_na_y))
  expect_true(any(non_na_y != round(non_na_y)))
})

test_that("feedback mechanisms work", {
  # covariate feedback
  dat_covar_fb <- margot_simulate(
    n = 50,
    waves = 3,
    covar_feedback = 0.8,
    wide = TRUE,
    seed = 20
  )
  expect_equal(ncol(dat_covar_fb), 1 + 3 * 3 + 1)  # id + 3*(l,y,a) + y
  
  # outcome feedback on treatment
  dat_y_fb <- margot_simulate(
    n = 50,
    waves = 3,
    y_feedback = 1.0,
    wide = TRUE,
    seed = 21
  )
  expect_equal(attr(dat_y_fb, "y_feedback"), 1.0)
})

test_that("missing data generation works", {
  dat_miss <- margot_simulate(
    n = 100,
    waves = 3,
    p_covars = 3,
    item_missing_rate = 0.3,
    seed = 30
  )
  
  # check that some covariates have missing values
  covar_cols <- paste0("l", 1:3)
  miss_props <- sapply(covar_cols, function(col) mean(is.na(dat_miss[[col]])))
  expect_true(any(miss_props > 0))
  expect_true(all(miss_props < 1))  # not all missing
})

test_that("edge cases are handled", {
  # single wave
  dat_1wave <- margot_simulate(n = 50, waves = 1, wide = TRUE)
  expect_equal(ncol(dat_1wave), 1 + 3 + 1)  # id + (l,y,a) + y
  
  # maximum waves
  expect_error(margot_simulate(waves = 21), "waves <= 20")
  
  # invalid inputs
  expect_error(margot_simulate(n = -1), "n > 0")
  expect_error(margot_simulate(item_missing_rate = 1.5), "item_missing_rate <= 1")
  expect_error(margot_simulate(wide = "yes"), "wide must be logical")
})

test_that("reproducibility with seed", {
  dat1 <- margot_simulate(n = 30, seed = 999)
  dat2 <- margot_simulate(n = 30, seed = 999)
  expect_identical(dat1, dat2)
  
  # different seed gives different results
  dat3 <- margot_simulate(n = 30, seed = 1000)
  expect_false(identical(dat1$y, dat3$y))
})