# Batch Plot QINI Curves Across Treatment Costs for Multiple Models

Creates visualizations showing how QINI curves change with treatment
cost for multiple models simultaneously. This function combines the
functionality of margot_qini_cost_sensitivity() and
margot_plot_qini_batch() to create comprehensive cost sensitivity
visualizations.

## Usage

``` r
margot_plot_qini_batch_cost_sensitivity(
  models,
  costs = c(0.2, 0.5, 1, 2, 5),
  model_names = NULL,
  plot_type = c("grid", "list", "combined"),
  spend_levels = 0.1,
  baseline_method = "maq_no_covariates",
  label_mapping = NULL,
  ncol = NULL,
  heights = NULL,
  widths = NULL,
  verbose = TRUE,
  x_axis = "budget",
  ...
)
```

## Arguments

- models:

  List returned by margot_causal_forest(), containing results and
  optionally full_models.

- costs:

  Numeric vector of treatment costs to evaluate. Default is c(0.2, 0.5,
  1, 2, 5) representing a range from cheap to expensive treatments.

- model_names:

  Optional character vector specifying which models to process. Default
  NULL (all models).

- plot_type:

  Character; how to arrange the plots: - "grid": Arrange in a grid with
  models as rows and costs as columns - "list": Return a nested list of
  individual plots - "combined": Create a single combined plot with
  facets Default is "grid".

- spend_levels:

  Numeric vector of spend levels for annotations. Default is 0.1.

- baseline_method:

  Method for generating baseline. See margot_qini() for details. Default
  is "maq_no_covariates".

- label_mapping:

  Named character vector for converting variable names to readable
  labels.

- ncol:

  For plot_type = "grid", number of columns in the grid. Default NULL
  uses the number of costs.

- heights:

  For plot_type = "grid", relative heights of rows. Default NULL gives
  equal heights.

- widths:

  For plot_type = "grid", relative widths of columns. Default NULL gives
  equal widths.

- verbose:

  Logical; print progress messages (default TRUE).

- x_axis:

  Type of x-axis for QINI curves: "proportion" or "budget" (default).
  "budget" shows budget per unit (matching maq visualization) which
  better illustrates the effect of different treatment costs.

- ...:

  Additional arguments passed to margot_plot_qini().

## Value

Depending on plot_type: - "grid": A combined plot object (requires
patchwork package) - "list": A nested list where first level is model
names, second level is costs - "combined": A single ggplot object with
facets

## Details

This function efficiently generates QINI curves for multiple models at
different treatment costs, enabling comprehensive cost sensitivity
analysis. The plots help identify: - Which models are most sensitive to
treatment cost - Cost thresholds where targeting becomes ineffective -
Optimal cost-benefit trade-offs for different outcomes

## Examples

``` r
if (FALSE) { # \dontrun{
# Create grid of plots for all models
grid_plots <- margot_plot_qini_batch_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2),
  plot_type = "grid"
)
print(grid_plots)

# Get individual plots as a list
plot_list <- margot_plot_qini_batch_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2),
  model_names = c("anxiety", "depression"),
  plot_type = "list"
)
# Access specific plot: plot_list$model_anxiety$cost_0.5

# Create combined faceted plot
combined_plot <- margot_plot_qini_batch_cost_sensitivity(
  cf_results,
  costs = c(0.5, 1, 2),
  model_names = c("anxiety", "depression"),
  plot_type = "combined"
)
} # }
```
