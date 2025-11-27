# Visualise Shifts in Data Distributions with Highlighted Ranges

This function creates a histogram that highlights a specified range of
values to visualise shifts in data distributions. The highlighted range
can indicate areas of interest, such as shifts up or down in the
distribution. This visualisation is useful for understanding the
implications of causal contrasts, such as modified treatment policies.
The fill colour of the histogram is dynamically adjusted based on the
specified direction of the shift.

## Usage

``` r
margot_plot_shift(
  df,
  col_name,
  binwidth = 1,
  range_highlight = NULL,
  shift = "up",
  show_avg_line = TRUE,
  print_avg_value = TRUE,
  show_sd_line = TRUE,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = "Count",
  save_path = NULL,
  width = 12,
  height = 8,
  include_timestamp = FALSE
)
```

## Arguments

- df:

  A dataframe containing the variable of interest.

- col_name:

  The name of the column in `df` to be visualised in the histogram. This
  should be a numeric variable.

- binwidth:

  The width of the bins for the histogram. Default is 1. Adjust this
  based on the distribution and scale of your data.

- range_highlight:

  A numeric vector of length 2 specifying the start and end of the range
  to highlight. If `NULL`, no range is highlighted.

- shift:

  A character string indicating the direction of the shift, either "up"
  or "down". Default is "up".

- show_avg_line:

  A logical value indicating whether to display a vertical line
  representing the average value. Default is `TRUE`.

- print_avg_value:

  A logical value indicating whether to print the numerical value of the
  average on the plot. When `TRUE`, the value is printed horizontally to
  the left of the mean line at the bottom of the graph. Default is
  `TRUE`.

- show_sd_line:

  A logical value indicating whether to display vertical dashed lines
  (in grey) representing one standard deviation on either side of the
  average. Default is `TRUE`.

- title:

  An optional custom title for the plot. If `NULL`, a default title will
  be generated.

- subtitle:

  An optional custom subtitle for the plot. If `NULL`, a default
  subtitle will be generated.

- x_lab:

  An optional label for the x-axis. If `NULL`, the formatted column name
  is used.

- y_lab:

  The label for the y-axis. Default is "Count".

- save_path:

  An optional path to save the plot. If `NULL`, the plot will not be
  saved.

- width:

  The width of the saved plot in inches. Default is 12.

- height:

  The height of the saved plot in inches. Default is 8.

- include_timestamp:

  A logical value indicating whether to include a timestamp in the saved
  filename. Default is `FALSE`.

## Value

A ggplot object representing the histogram with specified highlights.
