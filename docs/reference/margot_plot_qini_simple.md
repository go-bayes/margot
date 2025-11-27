# Plot QINI Curves (Simplified Version)

Creates a ggplot2 visualization of QINI curves computed by
margot_qini(). Shows CATE-based targeting vs no-priority baseline.

## Usage

``` r
margot_plot_qini_simple(
  margot_result,
  model_name,
  show_ci = FALSE,
  ci_alpha = 0.1,
  colors = c(cate = "#1f77b4", baseline = "#ff7f0e"),
  spend_levels = c(0.1, 0.4),
  title = NULL,
  subtitle = NULL,
  theme = theme_minimal(),
  return_data = FALSE
)
```

## Arguments

- margot_result:

  Output from margot_qini()

- model_name:

  Character string specifying which model to plot

- show_ci:

  Logical or character. Show confidence intervals: FALSE (none),
  TRUE/"both" (both curves), "cate" (CATE only), "baseline" (baseline
  only)

- ci_alpha:

  Significance level for confidence intervals (default 0.1)

- colors:

  Named vector of colors. Default: c(cate = "#1f77b4", baseline =
  "#ff7f0e")

- spend_levels:

  Numeric vector of spend levels to mark with vertical lines

- title:

  Character string for plot title. If NULL, auto-generated

- subtitle:

  Character string for plot subtitle. If NULL, auto-generated

- theme:

  ggplot2 theme. Default is theme_minimal()

- return_data:

  Logical. Return data frame instead of plot (default FALSE)

## Value

A ggplot2 object or data frame (if return_data = TRUE)
