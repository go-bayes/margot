# Adjust Weights for Censoring and Sample Design with Progress Reporting

This function calculates and adjusts weights for censoring, combining
them with sample weights if provided. It also offers options for
trimming and normalising the resulting weights. Progress is reported
using the cli package.

## Usage

``` r
margot_adjust_weights(
  pscore,
  censoring_indicator,
  sample_weights = NULL,
  trim = TRUE,
  normalize = TRUE,
  lower_percentile = 0.01,
  upper_percentile = 0.99,
  na.rm = TRUE
)
```

## Arguments

- pscore:

  Numeric vector of predicted probabilities from a censoring model.
  Values must be between 0 and 1.

- censoring_indicator:

  Logical vector or 0/1 numeric vector indicating censoring status
  (TRUE/1 if censored, FALSE/0 if not).

- sample_weights:

  Optional numeric vector of sample weights.

- trim:

  Logical; whether to trim weights (default is TRUE).

- normalize:

  Logical; whether to normalise weights (default is TRUE).

- lower_percentile:

  Numeric; lower percentile for trimming (default is 0.01).

- upper_percentile:

  Numeric; upper percentile for trimming (default is 0.99).

- na.rm:

  Logical; whether to remove NA values (default is TRUE).

## Value

A list containing adjusted weights and summary statistics.
