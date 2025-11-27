# Plot QINI Curves Across Treatment Cost Scenarios

Visualizes how QINI curves change with different treatment costs,
showing the impact of budget constraints on optimal treatment
allocation.

## Usage

``` r
margot_plot_qini_cost_sensitivity(
  cost_sensitivity_result,
  model_name,
  plot_type = c("overlay", "facet"),
  show_baseline = TRUE,
  colors = NULL,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- cost_sensitivity_result:

  Output from margot_qini_cost_sensitivity()

- model_name:

  Character string specifying which model to plot

- plot_type:

  Character; either "overlay" (all costs on one plot) or "facet"
  (separate panel for each cost). Default is "overlay".

- show_baseline:

  Logical; whether to show baseline curves. Default is TRUE.

- colors:

  Optional vector of colors for different cost scenarios. If NULL, uses
  a gradient from blue (low cost) to red (high cost).

- title:

  Optional plot title. If NULL, auto-generated based on model name.

- subtitle:

  Optional plot subtitle. If NULL, describes the cost scenarios.

- ...:

  Additional arguments passed to ggplot2 functions

## Value

A ggplot object

## Details

This function creates visualizations showing how QINI curves change with
treatment cost. Lower costs result in steeper curves (more people can be
treated cost-effectively), while higher costs result in shallower curves
(only highest-effect individuals justify treatment).

The "overlay" plot type shows all cost scenarios on one plot with
different colors, making it easy to compare curve shapes. The "facet"
plot type creates separate panels for each cost, useful when curves
overlap significantly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run cost sensitivity analysis
cost_sens <- margot_qini_cost_sensitivity(
  causal_forest_results,
  costs = c(0.2, 0.5, 1, 2, 5)
)

# Overlay plot (default)
margot_plot_qini_cost_sensitivity(cost_sens, "model_anxiety")

# Faceted plot
margot_plot_qini_cost_sensitivity(
  cost_sens,
  "model_anxiety",
  plot_type = "facet"
)

# Custom styling
margot_plot_qini_cost_sensitivity(
  cost_sens,
  "model_anxiety",
  colors = c("darkgreen", "gold", "orange", "red", "darkred"),
  title = "Treatment Cost Impact on Anxiety Intervention",
  subtitle = "Lower costs enable treating more patients"
)
} # }
```
