# Strict All-or-Nothing Censoring for Longitudinal Data

This function processes wide-format longitudinal data with multiple time
points. It is a wrapper around the internal function
\`.strict_exposure_outcome_censoring\`. See the internal function
documentation for details.

## Usage

``` r
strict_exposure_outcome_censoring(
  df_wide,
  exposure_vars,
  ordinal_columns = NULL,
  continuous_columns_keep = NULL,
  scale_exposure = FALSE,
  not_lost_in_following_wave = "not_lost_following_wave",
  lost_in_following_wave = "lost_following_wave",
  remove_selected_columns = TRUE,
  time_point_prefixes = NULL,
  time_point_regex = NULL,
  save_observed_y = FALSE
)
```

## Arguments

- df_wide:

  A wide-format dataframe with columns like t0_X, t1_X, t2_X, etc.

- exposure_vars:

  Character vector of all exposure names (e.g. c("aaron_antagonism",
  "aaron_disinhibition", ...)).

- ordinal_columns:

  Character vector of ordinal (factor) variables to be dummy-coded.

- continuous_columns_keep:

  Numeric columns you do NOT want to scale (e.g. if they must remain in
  original units).

- scale_exposure:

  If FALSE, do not scale exposures; if TRUE, exposures are also scaled.

- not_lost_in_following_wave:

  Name for the "not lost" indicator (default "not_lost_following_wave").

- lost_in_following_wave:

  Name for the "lost" indicator (default "lost_following_wave").

- remove_selected_columns:

  If TRUE, remove original columns after dummy-coding ordinal columns.

- time_point_prefixes:

  Optional vector of wave prefixes (like c("t0","t1","t2")); if NULL, we
  auto-detect via regex.

- time_point_regex:

  Regex used to detect wave prefixes if \`time_point_prefixes\` is NULL.

- save_observed_y:

  If FALSE, set any missing final-wave outcomes to NA. If TRUE, keep
  partial final-wave outcomes.

## Value

A processed dataframe, with strict all-or-nothing censoring
