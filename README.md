
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ```{r, include = FALSE} -->
<!-- library(here) -->
<!-- # GitHub pages -->
<!-- file.create("docs/.nojekyll", showWarnings = FALSE) -->
<!-- ``` -->
<!-- badges: start -->

<img src="man/figures/margot.png" width = 600>

<!-- badges: end -->

# margot

**MARG**inal **O**bservational **T**reatment-effects.[^1]

Causal inference requires **balance** across the treatments to be
compared. In observational studies, such balance is not guaranteed;
quantifying causality therefore requires careful, multi-step workflows.

The goal of `margot` is to enhance the accessibility of these workflows
for causal inference. Its primary audience includes psychological
scientists, although it may benefit other social scientists.

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

### Code

The code in this package is licensed under the GNU General Public
License (GPL) v3.0. You can redistribute it and/or modify it under the
terms of the GPL as published by the Free Software Foundation. For more
information, see [GPL v3.0](http://www.gnu.org/licenses/).

The `margot` package is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See th GNU General
Public License for more details.

## Doi

[![DOI](https://zenodo.org/badge/766117235.svg)](https://zenodo.org/doi/10.5281/zenodo.10907723)

[^1]: The logo is a *Single World Intervention Template* (SWIT). We use
    a SWIT to generate *Single World Intervention Graphs* (SWIGs) –
    causal diagrams for which identification assumptions can be read
    separately for each treatment (regime) to be compared. The name
    `margot` reflects the contents and aims of this package; it is also
    the name of my daughter, Margot.
