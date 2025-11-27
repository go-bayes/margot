# Transform longitudinal data to wide format and impute baseline (soft-deprecated)

\*\*Deprecated\*\*: Use \`margot_wide_machine(imputation_method =
"mice")\` instead.

## Usage

``` r
margot_wide_impute_baseline(.data, baseline_vars, exposure_var, outcome_vars)
```

## Arguments

- .data:

  A data frame containing the longitudinal data in long format.

- baseline_vars:

  A character vector of baseline variable names to include and impute.

- exposure_var:

  A character vector specifying the names of exposure variables.

- outcome_vars:

  A character vector specifying the names of outcome variables.

## Value

A data frame in wide format with imputed baseline variables.

## Deprecated

This function was soft-deprecated in version 1.0.38 and will be removed
in a future major release.

## Examples

``` r
# Preferred: use margot_wide_machine with mice imputation
# wide_df <- margot_wide_machine(
#   df,
#   baseline_vars = c("age", "male"),
#   exposure_var = "forgiveness",
#   outcome_vars = "kessler_latent_anxiety",
#   imputation_method = "mice"
# )
```
