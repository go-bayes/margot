# Development Version of QINI Curve Plotting

Creates QINI curve plots from margot_qini_dev() results. Can handle
single or multiple outcomes/models in one plot without needing a
separate batch function.

## Usage

``` r
margot_plot_qini_dev(
  qini_results,
  outcomes = NULL,
  models = NULL,
  scale = "average",
  show_confidence = TRUE,
  show_baseline = TRUE,
  spend_markers = NULL,
  colors = NULL,
  theme_fn = ggplot2::theme_minimal,
  title = NULL,
  subtitle = NULL,
  facet_outcomes = TRUE,
  facet_scales = "fixed",
  label_mapping = NULL,
  verbose = TRUE
)
```

## Arguments

- qini_results:

  Results from margot_qini_dev() or a named list of such results

- outcomes:

  Character vector of outcomes to plot. NULL = all.

- models:

  Character vector of model names when qini_results is a list. NULL =
  all.

- scale:

  Character. Scale for gains: "average" (default), "cumulative", or
  "population"

- show_confidence:

  Logical. Show confidence intervals (default: TRUE)

- show_baseline:

  Logical. Show baseline curve (default: TRUE)

- spend_markers:

  Numeric vector. Budget levels to mark with vertical lines

- colors:

  Character vector of colors for curves. Auto-generated if NULL.

- theme_fn:

  ggplot2 theme function (default: theme_minimal())

- title:

  Character. Plot title. Auto-generated if NULL.

- subtitle:

  Character. Plot subtitle. Auto-generated if NULL.

- facet_outcomes:

  Logical. Create facets by outcome when multiple (default: TRUE)

- facet_scales:

  Character. Facet scales: "fixed", "free", "free_x", "free_y"

- label_mapping:

  Named list for variable label translation

- verbose:

  Logical. Print progress (default: TRUE)

## Value

A ggplot2 object

## Details

This function replaces both margot_plot_qini() and
margot_plot_qini_batch() by intelligently handling both single and
multiple model inputs.

Scale options: - "average": Average gain per unit treated (maq
default) - "cumulative": Cumulative gain (gain \* proportion) -
"population": Total population gain (gain \* proportion \* n)

When multiple models are provided (via a named list), the function can
either: - Create separate facets for each outcome (facet_outcomes =
TRUE) - Overlay all curves on a single plot (facet_outcomes = FALSE)

## Examples

``` r
if (FALSE) { # \dontrun{
# Single model plot
test_data <- margot_simulate_test_data()
cf_results <- margot_causal_forest_dev(test_data$data, c("Y1", "Y2"), "A")
qini_results <- margot_qini_dev(cf_results)

plot1 <- margot_plot_qini_dev(qini_results)

# Multiple models comparison
cf_results2 <- margot_causal_forest_dev(test_data$data, c("Y3", "Y4"), "A")
qini_results2 <- margot_qini_dev(cf_results2)

plot2 <- margot_plot_qini_dev(
  list(model1 = qini_results, model2 = qini_results2),
  scale = "cumulative",
  spend_markers = c(0.2, 0.5)
)
} # }
```
