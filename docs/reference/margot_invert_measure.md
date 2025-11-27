# Invert Measure Values for Reverse Scoring

Inverts measure values using either z-score negation or ordinal scale
inversion. This function is used internally by margot_causal_forest when
flip_outcomes is specified.

## Usage

``` r
margot_invert_measure(x, method = c("zscore", "ordinal"), scale_bounds = NULL)
```

## Arguments

- x:

  Numeric vector of values to invert

- method:

  Character string specifying inversion method: - "zscore": Simple
  negation for already standardized data (default) - "ordinal": Invert
  on ordinal scale using bounds

- scale_bounds:

  Numeric vector of length 2 specifying \[min, max\] bounds for ordinal
  scale inversion. Required when method = "ordinal". If NULL and method
  = "ordinal", bounds are inferred from data range.

## Value

Numeric vector of inverted values

## Details

Z-score method: Simply negates the values: x_flipped = -x This assumes
the input data is already standardized (z-scores). For raw data that
needs standardization, consider using scale() first.

Ordinal method: Uses the formula: x_flipped = (max + min) - x This
preserves distances between values while reversing their order.

## Examples

``` r
if (FALSE) { # \dontrun{
# z-score inversion (for already standardized data)
z_scores <- scale(c(1, 2, 3, 4, 5))[, 1] # standardize first
margot_invert_measure(z_scores, method = "zscore")

# ordinal scale inversion with known bounds
likert <- c(1, 2, 3, 4, 5, 3, 2)
margot_invert_measure(likert, method = "ordinal", scale_bounds = c(1, 5))

# ordinal scale inversion with inferred bounds
margot_invert_measure(likert, method = "ordinal")
} # }
```
