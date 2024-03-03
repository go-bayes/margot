
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ```{r, include = FALSE} -->
<!-- library(here) -->
<!-- # GitHub pages -->
<!-- file.create("docs/.nojekyll", showWarnings = FALSE) -->
<!-- ``` -->
<!-- badges: start -->

<img src="man/figures/margot_hex_sticker.png" width = 240>

<!-- badges: end -->

# margot

**MARG**inal **O**bservational **T**reatment-effects.[^1]

Causal inference demands **balance** across the treatments to be
compared. In observational studies, such balance is not guaranteed. The
goal of `margot` is to enhance the accessibility of causal inference.
Its primary audience includes psychological scientists, although it may
benefit other social scientists.

The package offers functions for:

- evaluating causal assumptions
- modelling time-series data
- reporting results
- performing sensitivity analyses

`margot` focuses on streamlining the estimation of (Marginal) Average
Treatment Effects, but it also supports workflows for Conditional
Average Treatment Effects, Heterogeneous Treatment Effects, and Modified
Treatment Policies.

This version of the package is a work in progress and experimental.

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

[^1]: The name `margot` reflects the contents of this package; it is
    also the name of my daughter, Margot.
