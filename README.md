
<!-- README.md is generated from README.Rmd. Please edit that file -->

# margot

<!-- badges: start -->
<!-- badges: end -->

The goal of `margot` is to improve causal inference in psychological
science.

<!-- badges: start -->
<!-- badges: end -->

Currently, `margot` consists of a synthetic longitudinal dataset used
for teaching purposes.

## Installation

You can install the development version of kiwicausalR like so:

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
