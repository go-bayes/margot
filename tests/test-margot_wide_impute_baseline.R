library(testthat)
library(dplyr)
library(mice) # ff mice functions are used within margot_wide_impute_baseline
library(margot)


test_that(
  "margot_wide_impute_baseline correctly imputes baseline data and handles wider format",
  {
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

    # assume df_nz is already loaded in your environment
    wide_data_2 <-
      margot_wide_impute_baseline(df_nz, baseline_vars, exposure_var, outcome_vars)

    # check function returns a data frame
    expect_true(is.data.frame(wide_data_2))

    # check baseline columns have no missing values
    for (var in baseline_vars) {
      expect_true(all(!is.na(wide_data_2[[paste0("t0_", var)]])))
    }

    # check for missing values in t2_ or greater columns for outcome variables
    time_points <-
      unique(gsub("^t(\\d+)_.*", "\\1", names(wide_data_2)))
    greater_than_t1 <- time_points[as.numeric(time_points) > 1]

    for (time in greater_than_t1) {
      for (var in outcome_vars) {
        column_name <- paste0("t", time, "_", var)
        if (column_name %in% names(wide_data_2)) {
          # Check that there is at least one NA value in each of these columns
          expect_true(any(is.na(wide_data_2[[column_name]])))
        }
      }
    }
    # check specific column names are present, for example:
    expect_true("t0_male" %in% names(wide_data_2))
    expect_true("t0_age" %in% names(wide_data_2))
    # add more checks... tba
  }
)
