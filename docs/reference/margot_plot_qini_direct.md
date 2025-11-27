# Plot QINI Curves from Pre-computed Data

Internal function to create QINI plots directly from pre-computed
qini_data without requiring the full causal forest results structure.

## Usage

``` r
margot_plot_qini_direct(
  qini_data,
  qini_objects = NULL,
  outcome_var = "Outcome",
  label_mapping = NULL,
  spend_levels = 0.1,
  show_spend_lines = TRUE,
  spend_line_color = "red",
  spend_line_alpha = 0.5,
  theme = "classic",
  show_ci = FALSE,
  ci_alpha = 0.05,
  ci_n_points = 20,
  ci_ribbon_alpha = 0.3,
  ci_ribbon_color = NULL,
  horizontal_line = TRUE,
  ylim = NULL,
  fixed_ylim = FALSE,
  cate_color = "#d8a739",
  ate_color = "#4d4d4d",
  treatment_cost = 1,
  x_axis = NULL,
  ...
)
```

## Arguments

- qini_data:

  Data frame with columns: proportion, gain, curve

- qini_objects:

  List of maq objects (optional, for confidence intervals)

- outcome_var:

  Character string for the outcome variable name

- label_mapping:

  Optional named list for label transformations

- spend_levels:

  Numeric vector of spend levels for vertical lines

- show_spend_lines:

  Logical; show vertical spend lines

- spend_line_color:

  Color for spend lines

- spend_line_alpha:

  Alpha for spend lines

- theme:

  ggplot2 theme name

- show_ci:

  Logical or character for confidence intervals

- ci_alpha:

  Significance level for CI

- ci_n_points:

  Number of points for CI computation

- ci_ribbon_alpha:

  Alpha for CI ribbons

- ci_ribbon_color:

  Color for CI ribbons

- horizontal_line:

  Logical; draw horizontal lines for complete paths

- ylim:

  Y-axis limits

- fixed_ylim:

  Logical; use fixed y-axis scaling

- cate_color:

  Color for CATE curve

- ate_color:

  Color for ATE curve

- treatment_cost:

  Treatment cost value

- x_axis:

  Type of x-axis: "proportion" or "budget". If not specified, will be
  inferred from data.

- ...:

  Additional arguments (ignored)

## Value

A ggplot object
