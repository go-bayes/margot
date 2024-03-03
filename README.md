
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

**MARG**inal **O**bservational **T**reatment-effects

Causal inference demands balance among the treatments to be compared. In
observational studies, such balance is not guaranteed. The goal of
`margot` is to enhance the accessibility of causal inference. Its
primary audience includes psychological scientists, though it may
benefit others.

The package offers helper functions for:

- evaluating causal assumptions
- modelling time-series data
- reporting results
- performing sensitivity analyses

`margot` focuses on computing Marginal Average Treatment Effects, but it
also supports Conditional Average Treatment Effects.

Please note this version is a work in progress and experimental. All
functions have been tested, albeit only with data resembling synthetic
teaching data (a three-wave panel design). If your interests or data
differ, proceed with caution.

The name `margot` aptly reflects the essence of this package, which I
have also chosen to name in honour of my daughter, Margot.

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
