
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

Causal inference requires **balance** across the treatments to be
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

# create transition table to evaluate the positivity assumption
transition_matrix <- create_transition_matrix(df_nz, "religion_believe_god", "id")

# create table and table explanation
table_change_belief <- transition_table(transition_matrix)
table_change_belief
```

## Code

Go to:<https://github.com/go-bayes/margot>

## Doi

[![DOI](https://zenodo.org/badge/766117235.svg)](https://zenodo.org/doi/10.5281/zenodo.10907723)

[^1]: The name `margot` reflects the contents of this package; it is
    also the name of my daughter, Margot.
