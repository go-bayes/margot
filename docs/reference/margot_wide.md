# Transform longitudinal data to wide format with labels

\*\*Deprecated\*\*: Use \`margot_wide_machine()\` instead.

## Usage

``` r
margot_wide(.data, baseline_vars, exposure_var, outcome_vars)
```

## Arguments

- .data:

  A data frame containing the longitudinal data in long format.

- baseline_vars:

  A character vector of baseline variable names to include.

- exposure_var:

  A character vector of exposure variable names.

- outcome_vars:

  A character vector of outcome variable names.

## Value

A wide-format data frame with prefixed time-stamped columns.

## Deprecated

This function was soft-deprecated in version 1.0.38 and will be removed
in a future release.
