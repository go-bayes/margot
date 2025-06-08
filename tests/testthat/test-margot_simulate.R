library(testthat)
context("margot_simulate wide/long structure")

## ---- helpers -----------------------------------------------------------
count_pattern <- function(x, pat) sum(grepl(pat, x, ignore.case = FALSE))

has_all <- function(df, vars) all(vars %in% names(df))

## ---- 1. wide format ----------------------------------------------------
test_that("wide output has expected structure and ordering", {
  n_waves <- 3
  dat <- margot_simulate(n = 200, waves = n_waves, p_covars = 2,
                         item_missing_rate = 0, y_feedback = 0.5, wide = TRUE)

  # rows
  expect_equal(nrow(dat), 200)

  # columns
  n_cov <- 2
  n_L   <- n_waves * 3
  n_A   <- n_waves
  expect_equal(
    ncol(dat),
    1 + n_cov + 1 + n_L + n_A + 1  # id + covars + t0_A1 + L + A + Y
  )

  # ordering
  expect_identical(names(dat)[1:5],
                   c("id", "B1", "B2", "t0_A1", "t1_L1"))

  # no censor column
  expect_false(any(grepl("^C$", names(dat))))
})

## ---- 2. long format ----------------------------------------------------
test_that("long output contains waves and outcomes", {
  n_waves <- 4
  dat_long <- margot_simulate(n = 150, waves = n_waves,
                              p_covars = 1, wide = FALSE, seed = 123)

  must_have <- c("wave", "t1_L1", "t1_A1", "t5_Y")
  expect_true(all(must_have %in% names(dat_long)))

  expect_equal(max(dat_long$wave), n_waves)
  expect_equal(nrow(dat_long), 150 * (n_waves + 1))  # + baseline

  last_wave <- subset(dat_long, wave == n_waves)
  prev_wave <- subset(dat_long, wave < n_waves)
  expect_gt(sum(!is.na(last_wave$t5_Y)), 0)
  expect_equal(sum(!is.na(prev_wave$t5_Y)), 0)
})

## ---- 3. attrition sanity ----------------------------------------------
test_that("attrition ≈ target rate", {
  set.seed(99)
  dat <- margot_simulate(
    n        = 5000,
    waves    = 3,
    censoring = list(rate = 0.20),
    item_missing_rate = 0,
    wide     = TRUE
  )
  alive_t3 <- sum(!is.na(dat$t3_A1))
  alive_t2 <- sum(!is.na(dat$t2_A1))
  expect_lt(abs(alive_t3 / alive_t2 - 0.8), 0.03)  # within 3 %
})

## ---- 4. metadata -------------------------------------------------------
test_that("metadata attached", {
  dat <- margot_simulate(50, 1)
  meta <- attr(dat, "margot_meta")
  expect_true(is.list(meta) && "timestamp" %in% names(meta))
})
