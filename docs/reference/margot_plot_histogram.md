# Create a Histogram with Mean and Standard Deviation Highlights for Each Wave and Variable

This function generates a histogram plot for specified variables across
different waves of data. It highlights the mean and standard deviation
for each variable in each wave.

## Usage

``` r
margot_plot_histogram(
  data,
  col_names,
  id_col = "id",
  wave_col = "wave",
  waves = NULL,
  binwidth = 0.5,
  title = NULL,
  x_label = NULL,
  y_label = "Count",
  save_path = NULL,
  width = 12,
  height = 8,
  facet_scales = "free",
  color_palette = NULL,
  add_timestamp = FALSE,
  file_prefix = "",
  mean_line_color = "black",
  sd_line_color = "black",
  vertical_facets = FALSE
)
```

## Arguments

- data:

  A data frame containing the variables for the plot.

- col_names:

  Vector of names of the columns to create histograms for.

- id_col:

  Name of the column containing unique identifiers. Default is "id".

- wave_col:

  Name of the column containing wave information. Default is "wave".

- waves:

  Vector of waves to include in the plot. If NULL, all waves are
  included.

- binwidth:

  Width of the bins for the histogram. Default is 0.5.

- title:

  An optional title for the plot. If NULL, an automatic title will be
  generated.

- x_label:

  An optional label for the x-axis. If NULL, "Value" will be used.

- y_label:

  An optional label for the y-axis. Default is "Count".

- save_path:

  An optional path to save the plot. If NULL, the plot will not be
  saved.

- width:

  The width of the saved plot in inches. Default is 12.

- height:

  The height of the saved plot in inches. Default is 8.

- facet_scales:

  Scales for facet. Either "fixed", "free_x", "free_y", or "free".
  Default is "free".

- color_palette:

  An optional custom color palette for the plot.

- add_timestamp:

  Logical. If TRUE, adds a timestamp to the saved filename. Default is
  FALSE.

- file_prefix:

  An optional prefix to add to the beginning of the saved filename.
  Default is an empty string.

- mean_line_color:

  Color of the vertical line representing the mean. Default is "black".

- sd_line_color:

  Color of the dashed lines representing the standard deviation. Default
  is "black".

- vertical_facets:

  Logical. If TRUE, facets are arranged vertically. If FALSE (default),
  facets are arranged horizontally.

## Value

A ggplot2 object representing the histogram with highlights.

## Examples

``` r
# basic usage with default settings
margot_plot_histogram(
  data = your_data,
  col_names = c("variable1", "variable2"),
  id_col = "participant_id",
  wave_col = "survey_wave"
)
#> 
#> ── Margot Plot Histogram ───────────────────────────────────────────────────────
#> ✖ An error occurred: object 'your_data' not found
#> <simpleError in eval(expr, envir): object 'your_data' not found>
#> NULL

# specify waves and custom binwidth
margot_plot_histogram(
  data = your_data,
  col_names = c("score1", "score2"),
  waves = c(2018, 2020),
  binwidth = 1
)
#> 
#> ── Margot Plot Histogram ───────────────────────────────────────────────────────
#> ✖ An error occurred: object 'your_data' not found
#> <simpleError in eval(expr, envir): object 'your_data' not found>
#> NULL

# use custom labels and saving the plot with timestamp and prefix
margot_plot_histogram(
  data = your_data,
  col_names = c("attitude_measure"),
  title = "Distribution of Attitudes Over Time",
  x_label = "Attitude Score",
  save_path = "path/to/save/plot",
  add_timestamp = TRUE,
  file_prefix = "study1"
)
#> 
#> ── Margot Plot Histogram ───────────────────────────────────────────────────────
#> ✖ An error occurred: object 'your_data' not found
#> <simpleError in eval(expr, envir): object 'your_data' not found>
#> NULL

# use a custom color palette and custom line colors
custom_colors <- c("#FF9999", "#66B2FF")
margot_plot_histogram(
  data = your_data,
  col_names = c("var1", "var2"),
  color_palette = custom_colors,
  mean_line_color = "red",
  sd_line_color = "blue"
)
#> 
#> ── Margot Plot Histogram ───────────────────────────────────────────────────────
#> ✖ An error occurred: object 'your_data' not found
#> <simpleError in eval(expr, envir): object 'your_data' not found>
#> NULL

# use vertical faceting
margot_plot_histogram(
  data = your_data,
  col_names = c("var1", "var2"),
  vertical_facets = TRUE
)
#> 
#> ── Margot Plot Histogram ───────────────────────────────────────────────────────
#> ✖ An error occurred: object 'your_data' not found
#> <simpleError in eval(expr, envir): object 'your_data' not found>
#> NULL
```
