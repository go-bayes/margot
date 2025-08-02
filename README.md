
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

> **⚠️ IMPORTANT NOTICE**: This development version of the margot
> package is undergoing significant refactoring as we transition to the
> **margotverse** suite of packages. This package is currently for the
> author’s lab use only. The package will be split into focused,
> single-responsibility packages including margot.core, margot.lmtp,
> margot.grf, margot.viz, and others. Please expect breaking changes in
> upcoming releases.

**MARG**inal **O**bservational **T**reatment-effects.[^1]

Causal inference requires **balance** across the treatments to be
compared. In observational studies, such balance is not guaranteed;
quantifying causality therefore requires careful, multi-step workflows.

The goal of `margot` is to enhance understanding of causality in
observational research.

The package offers functions for:

- evaluating causal assumptions
- modelling time-series data
- reporting results
- performing sensitivity analyses

`margot` focuses on streamlining the estimation of (Marginal) Average
Treatment Effects (ATT, ATE), but it also supports workflows for
Heterogeneous Treatment Effects (CATE) (estimated via `grf`), as well as
Longitudinal Modified Treatment Policies (estimated via `lmtp`). It has
extensive graphical and reporting functions to ease burdens for
understanding.

[^1]: The logo is a *Single World Intervention Template* (SWIT). We use
    a SWIT to generate *Single World Intervention Graphs* (SWIGs) –
    causal diagrams for which identification assumptions can be read
    separately for each treatment (regime) to be compared. The name
    `margot` reflects the contents and aims of this package; it is also
    the name of my daughter, Margot.
