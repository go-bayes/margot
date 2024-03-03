
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

<img src="man/hex/margot_hex_sticker.jpeg" width = 240>
<!-- badges: end -->

# margot

**MARG**inal **O**bservational **T**reatment-effects

The goal of `margot` is to make causal inference more accessible. It’s
main audience are psychological scientists.

We are building helper functions to help:

- evaluate the assumptions required of their data to enable causal
  inference
- model their data (if assumptions are met)
- report results
- perform sensitivity analyses

Although `margot` focusses on computing (Marginal) Average Treatment
Effects, it also aims to help investigators obtain Conditional Average
Treatment Effects.

**This version is experimental**. It has only been tested for data
initially passed in long-data form.

## Installation

You can install the development version of `margot` like so:

``` r
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

devtools::install_github("go-bayes/margot")
```

## Example

``` r
library("margot")

# summary of agreeableness responses by wave in retained sample
df_nz |> 
  dplyr::select(id, wave, agreeableness) |> 
  dplyr::group_by(wave) |> 
  dplyr::summarise(mean = mean(agreeableness, na.rm = TRUE))
```
