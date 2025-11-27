# Process longitudinal dyadic data in wide format with censoring by missing exposure and silent dummy-coding

this function processes longitudinal data (wide format) across multiple
waves, handling dyadic censoring and optional censoring when the
exposure is missing.

if \`censor_if_missing_exposure = TRUE\`, any record with a missing
exposure at wave t+1 is considered lost at wave t and all subsequent
wave data are set to \`NA\`. dyadic logic ensures that if any member of
a dyad is lost, the entire dyad is censored.

## Usage

``` r
margot_process_longitudinal_data_wider(
  df_wide,
  relationship_id = "NULL",
  ordinal_columns = NULL,
  continuous_columns_keep = NULL,
  exposure_vars = NULL,
  scale_exposure = FALSE,
  scale_continuous = TRUE,
  censor_if_missing_exposure = TRUE,
  not_lost_in_following_wave = "not_lost_following_wave",
  lost_in_following_wave = NULL,
  remove_selected_columns = TRUE,
  time_point_prefixes = NULL,
  time_point_regex = NULL,
  save_observed_y = FALSE
)
```

## Arguments

- df_wide:

  a wide-format \`data.frame\` containing time-prefixed columns (e.g.,
  \`t0_x\`).

- relationship_id:

  column name identifying dyads; if present, dyadic censoring is
  applied. default: \`"NULL"\`.

- ordinal_columns:

  character vector of ordinal column bases to dummy-encode after
  processing.

- continuous_columns_keep:

  character vector of continuous column names to retain without scaling.

- exposure_vars:

  character vector of exposure base names (without time prefixes).

- scale_exposure:

  logical; if \`TRUE\`, scales exposure variables. default: \`FALSE\`.

- scale_continuous:

  logical; if \`TRUE\`, scales continuous variables. default: \`TRUE\`.

- censor_if_missing_exposure:

  logical; if \`TRUE\`, missing exposures at next wave cause censoring.
  default: \`TRUE\`.

- not_lost_in_following_wave:

  suffix for the not-lost indicator. default:
  \`"not_lost_following_wave"\`.

- lost_in_following_wave:

  suffix for the lost indicator; if \`NULL\`, no lost indicator is
  added.

- remove_selected_columns:

  logical; if \`TRUE\`, removes original columns after dummy encoding.
  default: \`TRUE\`.

- time_point_prefixes:

  optional vector of time-point prefixes (e.g., \`c("t0","t1")\`). if
  \`NULL\`, inferred from data.

- time_point_regex:

  regex pattern to identify time-point prefixes. used if
  \`time_point_prefixes\` is \`NULL\`.

- save_observed_y:

  logical; if \`TRUE\`, retains observed outcomes in final wave even if
  censored. default: \`FALSE\`.

## Value

a \`data.frame\` with processed and optionally scaled and encoded
columns, ready for longitudinal analysis.
