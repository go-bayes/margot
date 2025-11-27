# Create a Colored Histogram with Quantile or Custom Breaks

This function creates a histogram with colored regions based on quantile
breaks or custom breaks. It uses the \`create_ordered_variable\`
function to categorize the data and then plots the histogram with
different colors for each category.

## Usage

``` r
margot_plot_histogram_intervals(
  df,
  col_name,
  n_divisions = NULL,
  custom_breaks = NULL,
  cutpoint_inclusive = "upper",
  ties.method = NULL,
  colour_palette = NULL,
  hist_colour = "black",
  line_type = "solid",
  line_width = 0.75,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = "Count",
  theme_choice = theme_classic(),
  text_size = 12,
  axis_text_angle = 45,
  add_density = FALSE,
  add_rug = FALSE,
  facet_var = NULL,
  x_scale_transform = NULL,
  y_scale_transform = NULL,
  additional_layers = NULL,
  binwidth = NULL
)
```

## Arguments

- df:

  A data frame containing the variable to be plotted.

- col_name:

  The name of the column in the data frame to be plotted.

- n_divisions:

  The number of divisions for quantile breaks. Ignored if custom_breaks
  is provided.

- custom_breaks:

  A numeric vector of custom break points.

- cutpoint_inclusive:

  Character. Either "lower" or "upper", specifying whether the cutpoint
  should be included in the lower or upper interval.

- ties.method:

  A character string specifying how ties should be handled. See
  ?quantile for details.

- colour_palette:

  A vector of colors to use for the intervals. If NULL, uses the
  Okabe-Ito palette.

- hist_colour:

  The color of the histogram borders.

- line_type:

  The type of line to use for the histogram borders.

- line_width:

  The width of the lines for the histogram borders.

- title:

  The title of the plot. If NULL, a default title is used.

- subtitle:

  The subtitle of the plot. If NULL, a default subtitle is used.

- x_lab:

  The label for the x-axis. If NULL, the column name is used.

- y_lab:

  The label for the y-axis. Default is "Count".

- theme_choice:

  The ggplot2 theme to use. Default is theme_classic().

- text_size:

  The base text size for the plot.

- axis_text_angle:

  The angle of the x-axis text.

- add_density:

  Logical. If TRUE, adds a density curve to the plot.

- add_rug:

  Logical. If TRUE, adds a rug plot to the x-axis.

- facet_var:

  Optional. The name of a variable to use for faceting.

- x_scale_transform:

  Optional. A transformation for the x-axis (e.g., "log10").

- y_scale_transform:

  Optional. A transformation for the y-axis (e.g., "log10").

- additional_layers:

  A list of additional ggplot2 layers to add to the plot.

- binwidth:

  The width of the bins for the histogram. If NULL, calculated
  automatically.

## Value

A ggplot2 object representing the colored histogram.

## Examples

``` r
df <- data.frame(value = rnorm(1000))
coloured_histogram_quantiles(df, "value", n_divisions = 4)
#> Warning: `coloured_histogram_quantiles()` was deprecated in margot 1.0.0.
#> ℹ Please use `margot_plot_hist()` instead.
#> Error in ggplot(df, aes(x = !!rlang::sym(col_name))): could not find function "ggplot"

# With custom breaks
coloured_histogram_quantiles(df, "value", custom_breaks = c(-2, -1, 0, 1, 2))
#> Error in coloured_histogram_quantiles(df, "value", custom_breaks = c(-2,     -1, 0, 1, 2)): unused argument (custom_breaks = c(-2, -1, 0, 1, 2))
```
