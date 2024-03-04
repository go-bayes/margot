# tests/testthat/test-margot_wide.R

library(testthat)
library(dplyr) # ensure dplyr and tidyr are available if used in the function
library(tidyr)
library(margot)

test_that("margot_wide transforms data correctly", {
  # ensure the df_nz data is accessible as expected
  data("df_nz", package = "margot")

  # define variables
  baseline_vars <- c(
    "male",
    "age",
    "eth_cat",
    "partner",
    "agreeableness",
    "conscientiousness",
    "extraversion",
    "honesty_humility",
    "openness",
    "neuroticism",
    "sample_weights"
  )

  exposure_var <- c("forgiveness")

  outcome_vars <- c("alcohol_frequency",
                    "alcohol_intensity",
                    "hlth_bmi",
                    "hours_exercise")

  # apply function
  wide_data <-
    margot_wide(df_nz, baseline_vars, exposure_var, outcome_vars)

  # verify structure of wide data
  expected_cols <-
    c(
      "id",
      paste0("t0_", baseline_vars),
      paste0("t0_", exposure_var),
      paste0("t0_", outcome_vars)
    )
  expect_true(all(expected_cols %in% names(wide_data)), info = "All expected columns should be present")

  # verify the number of rows matches the unique IDs in df_nz
  expect_equal(nrow(wide_data), length(unique(df_nz$id)), info = "One row per unique ID")

  # additional value checks can be added here...
})
