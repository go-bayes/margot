# Batch Process and Plot QINI Curves for Multiple Models

This function processes a subset of models (or all models by default),
creates QINI (Qini coefficient) plots for each model using the margot
package.

## Usage

``` r
margot_plot_qini_batch(
  mc_result,
  model_names = NULL,
  label_mapping = NULL,
  spend_levels = c(0.1, 0.4),
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
  grid_step = NULL,
  ylim = NULL,
  baseline_method = "auto",
  cate_color = "#d8a739",
  ate_color = "#4d4d4d",
  scale = "average",
  treatment_cost = NULL,
  seed = 12345,
  fixed_ylim = FALSE
)
```

## Arguments

- mc_result:

  A list containing the results from margot_causal_forest().

- model_names:

  Optional character vector of model names to process. Can be specified
  with or without the "model\_" prefix. Default NULL (all models).

- label_mapping:

  Optional named list for custom label mappings. Keys should be original
  variable names (with or without "model\_" prefix), and values should
  be the desired display labels. Default is NULL.

- spend_levels:

  Numeric vector of spend levels to show with vertical lines. Default is
  0.1.

- show_spend_lines:

  Logical indicating whether to show vertical lines at spend levels.
  Default is TRUE.

- spend_line_color:

  Color for spend level lines. Default is "red".

- spend_line_alpha:

  Alpha transparency for spend lines. Default is 0.5.

- theme:

  Character string specifying the ggplot2 theme. Default is "classic".

- show_ci:

  Logical or character indicating which confidence intervals to show.
  Options: FALSE (none), TRUE or "both" (both curves), "cate" (CATE
  only), "ate" (ATE only). Default is FALSE.

- ci_alpha:

  Significance level for confidence intervals. Default is 0.05.

- ci_n_points:

  Number of points at which to compute confidence intervals. Default is
  20.

- ci_ribbon_alpha:

  Alpha transparency for confidence interval ribbons. Default is 0.3.

- ci_ribbon_color:

  Color for confidence interval ribbons. If NULL (default), uses the
  curve color.

- horizontal_line:

  Logical indicating whether to draw horizontal lines where Qini curves
  plateau when the path is complete. Default is TRUE.

- grid_step:

  Integer specifying the step size for subsampling the curve data. If
  NULL (default), uses max(floor(nrow(qini_data) / 1000), 1). Set to 1
  to plot all points.

- ylim:

  Numeric vector of length 2 specifying the y-axis limits c(min, max).
  Default is NULL (automatic scaling).

- baseline_method:

  Method for generating baseline: "auto" (default), "maq_no_covariates",
  "simple", "maq_only", or "none". See details in
  margot_generate_qini_data().

- cate_color:

  Color for the CATE (targeted treatment) curve. Default is "#d8a739"
  (gold).

- ate_color:

  Color for the ATE (no-priority/uniform assignment) curve. Default is
  "#4d4d4d" (dark gray).

- scale:

  Character string specifying the scale for gains: "average" (default),
  "cumulative", or "population". "average" shows expected policy effect
  per unit (maq default), "cumulative" shows traditional cumulative
  gains, "population" shows total population impact.

- treatment_cost:

  Numeric scalar; the treatment cost used in QINI calculations. Default
  is NULL, which attempts to extract the cost from model metadata. If
  not found, assumes cost = 1. When cost differs from stored cost, QINI
  curves are automatically regenerated. When cost differs from 1, it
  will be shown in the plot subtitle.

- seed:

  Integer; seed for reproducible QINI generation when treatment_cost
  differs from stored cost. Default is 12345.

- fixed_ylim:

  Logical; if TRUE and ylim is NULL, calculates y-axis limits that would
  accommodate a cost=1 scenario for consistent scaling across different
  cost comparisons. Default is FALSE.

## Value

A list containing the generated ggplot objects for each processed model.

## Examples

``` r
if (FALSE) { # \dontrun{
# Process all models
qini_plots <- margot_plot_qini_batch(mc_result)

# Process specific models with confidence intervals
qini_plots <- margot_plot_qini_batch(
  mc_result,
  model_names = c("t2_belong_z", "t2_meaning_z"),
  show_ci = TRUE, # shows CI for both curves
  ci_n_points = 50
)

# Show CI only for CATE curves
qini_plots <- margot_plot_qini_batch(
  mc_result,
  model_names = c("anxiety", "depression"),
  show_ci = "cate"
)

# Custom label mapping
label_mapping <- list(
  "t2_belong_z" = "Belonging",
  "t2_meaning_z" = "Meaning in Life"
)
qini_plots <- margot_plot_qini_batch(
  mc_result,
  label_mapping = label_mapping
)
} # }
```
