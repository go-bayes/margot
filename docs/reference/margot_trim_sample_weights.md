# Standardise and (optionally) trim sample weights at both ends

This function first trims any weights below a specified lower‐quantile
threshold and/or above a specified upper‐quantile threshold, then
standardises the (possibly trimmed) weights to have a mean of 1. Missing
values are preserved.

## Usage

``` r
margot_trim_sample_weights(
  weights_vec,
  lower_quantile = NULL,
  upper_quantile = 0.99
)
```

## Arguments

- weights_vec:

  Numeric vector of sample weights. Must be positive or NA.

- lower_quantile:

  Numeric in (0,1); all weights below this quantile will be raised
  (“winsorised up”) to the lower quantile value. If `NULL` or `<= 0`,
  lower‐end trimming is skipped.

- upper_quantile:

  Numeric in (0,1); all weights above this quantile will be lowered
  (“winsorised down”) to the upper quantile value. If `NULL` or `>= 1`,
  upper‐end trimming is skipped.

## Value

A numeric vector the same length as `weights_vec`, with extremes
winsorised and then rescaled to have mean 1.

## Details

Trimming both tails of inverse-probability weights can mitigate the
influence of implausibly small or large weights, trading a bit of bias
for lower variance.

## Examples

``` r
set.seed(42)
w <- c(rlnorm(90, 0, 0.5), runif(5, 5, 20), runif(5, 0, 0.01), NA)
summary(w)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#>  0.001333  0.718449  1.071490  1.756801  1.440705 18.770846         1 

# trim both lower 1% and upper 99%, then standardise
w_both <- margot_trim_sample_weights(
  w,
  lower_quantile = 0.01,
  upper_quantile = 0.99
)
#> Raised 1 weight(s) below the 1.0% quantile (threshold = 0.00286206).
#> Lowered 1 weight(s) above the 99.0% quantile (threshold = 17.9478).
summary(w_both)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#>  0.001637  0.410874  0.612776  1.000000  0.823926 10.264162         1 

# only upper trim at 95th percentile
w_up95 <- margot_trim_sample_weights(w, lower_quantile = NULL, upper_quantile = 0.95)
#> Lowered 5 weight(s) above the 95.0% quantile (threshold = 3.42476).
summary(w_up95)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#> 0.001099 0.592212 0.883222 1.000000 1.187563 2.823008        1 

# only lower trim at 5th percentile
w_low5 <- margot_trim_sample_weights(w, lower_quantile = 0.05, upper_quantile = NULL)
#> Raised 5 weight(s) below the 5.0% quantile (threshold = 0.213166).
summary(w_low5)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.1206  0.4066  0.6063  1.0000  0.8153 10.6220       1 

# no trimming (both NULL), only standardise
w_std <- margot_trim_sample_weights(w, lower_quantile = NULL, upper_quantile = NULL)
summary(w_std)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#> 7.589e-04 4.090e-01 6.099e-01 1.000e+00 8.201e-01 1.068e+01         1 
```
