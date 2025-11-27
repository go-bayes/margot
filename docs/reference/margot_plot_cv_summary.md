# Create summary plot for cross-validation heterogeneity results

Creates a summary bar plot showing the proportion of significant models
and their distribution across positive/negative effects.

## Usage

``` r
margot_plot_cv_summary(
  cv_results,
  title = NULL,
  positive_color = "#4CAF50",
  negative_color = "#F44336",
  neutral_color = "#9E9E9E"
)
```

## Arguments

- cv_results:

  A margot_cv_results object from margot_rate_cv()

- title:

  Character string for the plot title. Default generates based on
  results.

- positive_color:

  Color for positive effects. Default "#4CAF50" (green).

- negative_color:

  Color for negative effects. Default "#F44336" (red).

- neutral_color:

  Color for non-significant effects. Default "#9E9E9E" (gray).

## Value

A ggplot object
