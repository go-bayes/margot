# Create Individual Longitudinal Response Plots

This function creates a ggplot2 visualization of individual responses
over time for one or more variables. It allows for flexible data
filtering, sampling, and customization of the plot appearance. The
function automatically handles missing data by removing rows with NA
values in the specified variables.

## Usage

``` r
margot_plot_individual_responses(
  data,
  y_vars,
  id_col = "id",
  wave_col = "wave",
  waves = NULL,
  data_fraction = 1,
  random_draws = 100,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  theme = ggplot2::theme_classic(),
  include_timestamp = FALSE,
  save_path = NULL,
  width = 16,
  height = 8,
  seed = 12345,
  wave_label_angle = 45,
  full_response_scale = TRUE,
  scale_range = NULL,
  prefix = NULL,
  jitter_amount = 0.05,
  legend_position = "top"
)
```

## Arguments

- data:

  A data frame containing the variables to be plotted.

- y_vars:

  A character vector of column names in \`data\` to be plotted on the
  y-axis.

- id_col:

  The name of the column in \`data\` that contains unique identifiers
  for individuals. Default is "id".

- wave_col:

  The name of the column in \`data\` that contains the wave or time
  information. Default is "wave".

- waves:

  An optional vector of wave values to include in the plot. If NULL, all
  waves are included.

- data_fraction:

  The fraction of data to use (between 0 and 1). Default is 1 (use all
  data).

- random_draws:

  The number of random individuals to plot. If specified, overrides
  \`data_fraction\`.

- title:

  An optional title for the plot.

- y_label:

  An optional label for the y-axis.

- x_label:

  An optional label for the x-axis.

- color_palette:

  An optional vector of colors to use for the variables.

- theme:

  A ggplot2 theme to use for the plot. Default is theme_classic().

- include_timestamp:

  Logical, whether to include a timestamp in the saved file name.
  Default is FALSE.

- save_path:

  An optional file path to save the plot.

- width:

  The width of the saved plot in inches. Default is 16.

- height:

  The height of the saved plot in inches. Default is 8.

- seed:

  An optional seed for reproducibility when sampling data.

- wave_label_angle:

  The angle of the x-axis labels in degrees. Default is 45.

- full_response_scale:

  Logical, whether to use the full response scale for the y-axis.
  Default is TRUE.

- scale_range:

  An optional numeric vector of length 2 specifying the range for the
  y-axis. If NULL, the range is determined from the data.

- prefix:

  An optional prefix for the saved file name.

- jitter_amount:

  Numeric, the amount of vertical jitter to apply to points and lines.
  Default is 0.05.

- legend_position:

  The position of the legend. Default is "top".

## Value

A ggplot2 object representing the individual response plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic usage with default settings
plot1 <- margot_plot_individual_responses(
  data = your_data,
  y_vars = c("variable1", "variable2"),
  id_col = "participant_id",
  wave_col = "year"
)

# Example 2: Plotting specific waves and using random draws
plot2 <- margot_plot_individual_responses(
  data = your_data,
  y_vars = c("score1", "score2", "score3"),
  waves = c(2020, 2021, 2022),
  random_draws = 50,
  title = "Individual Scores Over Time",
  y_label = "Score",
  x_label = "Year",
  seed = 123
)

# Example 3: Customizing plot appearance and saving
plot3 <- margot_plot_individual_responses(
  data = your_data,
  y_vars = c("measure1", "measure2"),
  full_response_scale = TRUE,
  scale_range = c(0, 10),
  theme = theme_minimal(),
  wave_label_angle = 90,
  jitter_amount = 0.03,
  legend_position = "bottom",
  save_path = "path/to/save",
  prefix = "custom_plot"
)
} # }
```
