# Proportion of missing data at baseline

This function calculates the proportion of missing data at a baseline
wave. If a wave column is present, it uses the lowest number or lowest
factor level as the baseline. If no wave column is found, it issues a
warning and uses the entire dataset.

## Usage

``` r
margot_prop_missing(data, wave_col = "wave")
```

## Arguments

- data:

  A data frame containing the dataset.

- wave_col:

  A character string. Name of the column that indicates the wave. The
  default is "wave".

## Value

A numeric value representing the proportion of missing data at the
baseline wave.

## Examples

``` r
# Example using a dataset with a wave column
dat_long <- data.frame(wave = c(0, 0, 1), outcome = c(1, NA, 3))
margot_prop_missing(dat_long)
#> [1] 0.25

# Example using a dataset without a wave column
some_data <- data.frame(outcome = c(1, NA, 3))
margot_prop_missing(some_data)
#> Warning: No wave column found. Using entire dataset.
#> [1] 0.333
```
