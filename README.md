
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- ```{r, include = FALSE} -->

<!-- library(here) -->

<!-- # GitHub pages -->

<!-- file.create("docs/.nojekyll", showWarnings = FALSE) -->

<!-- ``` -->

<!-- badges: start -->

<img src="man/figures/margot.png" width = 600 alt="Margot package logo showing the package name">

<!-- badges: end -->

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
Average Treatment Effects and exploring Heterogeneous Treatment Effects,
as well as Modified Treatment Policies.

[^1]: The logo is a *Single World Intervention Template* (SWIT). We use
    a SWIT to generate *Single World Intervention Graphs* (SWIGs) –
    causal diagrams for which identification assumptions can be read
    separately for each treatment (regime) to be compared. The name
    `margot` reflects the contents and aims of this package; it is also
    the name of my daughter, Margot.
