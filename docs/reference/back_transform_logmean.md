# Back-transform Log-transformed Mean

Back-transforms a log-transformed mean (using log(x + 1) transformation)
to its original scale. This utility function is useful for interpreting
results when the original data were transformed using log(x + 1) to
handle zero values or to normalize the distribution of the data.

## Usage

``` r
back_transform_logmean(log_mean)
```

## Arguments

- log_mean:

  The mean on the log scale, where the original data were transformed
  using log(x + 1).

## Value

A list containing the mean on the original scale (\`mean_original\`).
The standard deviation is not back-transformed by this function due to
the complexity introduced by the log(x + 1) transformation.

## Examples

``` r
log_mean <- 1.098612 # true mean is 2. We add + 1 to the log to handle zero: log(2+1) = log(3)
back_transformed_result <- back_transform_logmean(log_mean)
print(back_transformed_result)
#> $mean_original
#> [1] 1.999999
#> 
```
