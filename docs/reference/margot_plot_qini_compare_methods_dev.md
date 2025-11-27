# Compare QINI Curves Across Different Methods

Convenience function for comparing QINI curves from different baseline
methods

## Usage

``` r
margot_plot_qini_compare_methods_dev(
  cf_results,
  outcome,
  baseline_methods = c("maq_no_covariates", "simple", "none"),
  ...
)
```

## Arguments

- cf_results:

  Results from margot_causal_forest_dev()

- outcome:

  Single outcome to analyze

- baseline_methods:

  Vector of baseline methods to compare

- ...:

  Additional arguments passed to margot_plot_qini_dev()

## Value

A ggplot2 object comparing methods
