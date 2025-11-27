# Create Summary Plot of Optimal Treatment Fractions Across Costs

Creates a line plot showing how the optimal treatment fraction changes
with treatment cost for one or more models.

## Usage

``` r
margot_plot_qini_cost_summary(
  cost_sensitivity_result,
  model_names = NULL,
  spend_level = 0.1,
  metric = "comparison_gain",
  ...
)
```

## Arguments

- cost_sensitivity_result:

  Output from margot_qini_cost_sensitivity()

- model_names:

  Optional character vector of models to include. Default NULL includes
  all models.

- spend_level:

  Numeric; which spend level to plot. Default 0.1.

- metric:

  Character; which metric to plot: "comparison_gain" (default),
  "difference_gain", or "reference_gain".

- ...:

  Additional arguments passed to ggplot2

## Value

A ggplot object
