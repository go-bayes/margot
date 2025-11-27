# Back Transform Z-Score to Original Scale

This function takes a z-score and transforms it back to its original
scale using the specified mean and standard deviation of the original
data. Often, standardization has been applied and the original scale
values are needed for interpretation.

## Usage

``` r
back_transform_zscore(z, mean, sd)
```

## Arguments

- z:

  A numeric value or vector of z-scores to be transformed back to the
  original scale.

- mean:

  The mean of the original dataset from which the z-score was
  calculated.

- sd:

  The standard deviation of the original dataset from which the z-score
  was calculated.

## Value

Returns a numeric value or vector of the original scale values
corresponding to the input z-scores.

## Examples

``` r
# Given a dataset with mean = 100 and sd = 15
original_value <- back_transform_zscore(z = 1.5, mean = 100, sd = 15)
print(original_value)
#> [1] 122.5

# Multiple z-scores can be transformed at once
z_scores <- c(-1, 0, 1, 2)
original_values <- back_transform_zscore(z = z_scores, mean = 50, sd = 10)
print(original_values)
#> [1] 40 50 60 70
```
