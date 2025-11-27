# Impute Missing Values Using Carry Forward in Longitudinal Data

Imputes missing values in longitudinal data by carrying forward previous
observations up to a specified number of time points back. By default,
it never imputes data for the final wave (end-of-study). Optionally, it
can create indicator variables for imputed values.

## Usage

``` r
margot_impute_carry_forward(
  df_wide,
  columns_to_impute,
  max_carry_forward = 1,
  time_point_prefixes = NULL,
  time_point_regex = NULL,
  require_one_observed = TRUE,
  columns_no_future_required = NULL,
  create_na_indicator = TRUE,
  indicator_suffix = "_na",
  indicator_as_suffix = TRUE,
  verbose = TRUE,
  impute_final_wave = FALSE
)
```

## Arguments

- df_wide:

  a wide-format dataframe containing longitudinal data.

- columns_to_impute:

  character vector of base column names to impute (without time
  prefixes).

- max_carry_forward:

  maximum number of time points to look back for carrying forward
  values.

- time_point_prefixes:

  optional vector of time point prefixes (e.g., c("t0", "t1", "t2")).

- time_point_regex:

  optional regex pattern to identify time points. Overrides
  time_point_prefixes if provided.

- require_one_observed:

  logical. if TRUE, only impute if at least one value is observed in the
  present or a following wave.

- columns_no_future_required:

  character vector of columns that do not require future observations
  for imputation. defaults to all columns if require_one_observed =
  FALSE, or none if require_one_observed = TRUE.

- create_na_indicator:

  logical. if TRUE, creates indicator variables for imputed values.

- indicator_suffix:

  suffix to add to the original column name for the indicator variable
  (default is "\_na").

- indicator_as_suffix:

  logical. if TRUE, the indicator suffix is added as a suffix; if FALSE,
  it's added as a prefix.

- verbose:

  logical. if TRUE, prints progress information.

- impute_final_wave:

  logical. if FALSE (default), the final wave (end-of-study) is never
  imputed. if TRUE, the final wave can be imputed like other waves.

## Value

a dataframe with imputed values and optional indicator variables.
