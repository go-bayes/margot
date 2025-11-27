# Create a Discontinuity Plot for Multiple Events

This function creates a ggplot2 visualisation to show discontinuities in
data across multiple events. It's particularly useful for visualising
changes in trends before and after significant events.

## Usage

``` r
margot_plot_discontinuity(
  data,
  y_var,
  event_dates,
  event_names = NULL,
  start_date = NULL,
  end_date = NULL,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = 1,
  seed = 12345,
  point_alpha = 0.03,
  jitter_width = 1,
  base_date = as.Date("2009-06-30"),
  save_path = NULL,
  width = 12,
  height = 8,
  event_line_color = "darkred",
  event_line_alpha = 0.7,
  event_line_type = "dashed",
  event_line_width = 0.5,
  event_label_size = 3,
  event_label_color = "darkred",
  legend_position = "bottom",
  use_title_case = TRUE,
  remove_underscores = TRUE
)
```

## Arguments

- data:

  A data frame containing the variables to be plotted.

- y_var:

  The name of the y-axis variable in the data frame.

- event_dates:

  A vector of dates representing the events.

- event_names:

  An optional vector of names for the events. If NULL, events will be
  labeled "Event 1", "Event 2", etc.

- start_date:

  An optional start date for the x-axis.

- end_date:

  An optional end date for the x-axis.

- title:

  An optional title for the plot.

- y_label:

  An optional label for the y-axis.

- x_label:

  An optional label for the x-axis.

- smoothing_method:

  The method used for smoothing. Default is "gam".

- gam_k:

  The number of knots to use if smoothing_method is "gam". Default is 4.

- data_fraction:

  The fraction of data to use. Default is 1 (use all data).

- seed:

  An optional seed for reproducibility when sampling data.

- point_alpha:

  The alpha (transparency) of the data points. Default is 0.03.

- jitter_width:

  The width of the jitter for the data points. Default is 1.

- base_date:

  The base date for the timeline. Default is "2009-06-30".

- save_path:

  An optional path to save the plot.

- width:

  The width of the saved plot in inches. Default is 12.

- height:

  The height of the saved plot in inches. Default is 8.

- event_line_color:

  The color of the event lines. Default is "darkred".

- event_line_alpha:

  The alpha of the event lines. Default is 0.7.

- event_line_type:

  The type of the event lines. Default is "dashed".

- event_line_width:

  The width of the event lines. Default is 0.5.

- event_label_size:

  The size of the event labels. Default is 3.

- event_label_color:

  The color of the event labels. Default is "darkred".

- legend_position:

  The position of the legend. Default is "bottom".

- use_title_case:

  Logical, whether to use title case for labels. Default is TRUE.

- remove_underscores:

  Logical, whether to remove underscores from labels. Default is TRUE.

## Value

A ggplot2 object representing the discontinuity plot.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(ggplot2)
library(margot)

# Assume that 'dat' is your dataset and that 'path_talk' is defined
muslim_discontinuity_warmth_plot <- margot_plot_discontinuity(
  data = dat,
  y_var = "warm_muslims",
  event_dates = c("2019-03-15", "2020-03-26"),
  event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
  start_date = "2012-06-06",
  title = "Discontinuity at multiple events (GAM)",
  y_label = "Muslim Warmth",
  x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
  point_alpha = 0.05,
  smoothing_method = "gam",
  gam_k = 4,
  data_fraction = .1,
  seed = 123,
  save_path = here::here(path_talk)
)

# Display the plot
print(muslim_discontinuity_warmth_plot)
} # }
```
