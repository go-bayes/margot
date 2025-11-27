# process longitudinal data for three waves

this function processes longitudinal data for exactly three waves (t0,
t1, t2). it handles attrition, scales baseline variables, and optionally
encodes ordinal variables. note: this function is currently implemented
for three waves only.

## Usage

``` r
margot_process_longitudinal_data(
  df_wide,
  ordinal_columns = NULL,
  continuous_columns_keep = NULL
)
```

## Arguments

- df_wide:

  a wide-format dataframe containing longitudinal data for three waves.

- ordinal_columns:

  a character vector of column names to be treated as ordinal and
  dummy-coded.

- continuous_columns_keep:

  a character vector of continuous column names to keep without scaling.

## Value

a processed dataframe suitable for use in longitudinal analyses with
three waves.

## Details

the function performs the following steps: 1. creates na conditions for
t0 and t1 based on missingness in subsequent waves. 2. handles
non-factor and factor columns, applying attrition logic. 3. scales
numeric baseline (t0) variables. 4. selects and orders columns. 5.
optionally encodes ordinal columns.

## Note

this function is specifically designed for datasets with exactly three
waves (t0, t1, t2). it may not work correctly for datasets with fewer or
more waves.

## Examples

``` r
# assuming df_wide is your wide-format dataframe with three waves
processed_data <- margot_process_longitudinal_data(
  df_wide,
  ordinal_columns = c("education", "income_category"),
  continuous_columns_keep = c("age", "bmi")
)
#> 
#> ── Longitudinal Data Processing ────────────────────────────────────────────────
#> ℹ Starting data processing for three waves (t0, t1, t2)
#> 
#> ── Step 1: Creating NA conditions ──
#> 
#> Error: object 'df_wide' not found
```
