# Plot Panel Study Response Timeline

This function creates a ggplot2 visualization of a panel study response
timeline.

## Usage

``` r
margot_plot_response_timeline(
  df_timeline,
  n_total_participants = NULL,
  save = FALSE,
  save_path = here::here("output"),
  width = 12,
  height = 8,
  base_filename = "timeline_histogram",
  title = "Panel Study Timeline",
  x_label = "Date",
  y_label = "Count of Responses",
  color_palette = NULL,
  save_png = FALSE,
  use_timestamp = FALSE
)
```

## Arguments

- df_timeline:

  A data frame containing the processed timeline data, typically output
  from \`prepare_panel_data()\`.

- n_total_participants:

  The total number of unique participants. If NULL, it will be extracted
  from df_timeline if available.

- save:

  Logical. If TRUE, saves the plot as a qs file. Default is FALSE.

- save_path:

  The directory path to save the plot. Default is "output" in the
  current working directory.

- width:

  The width of the saved plot in inches. Default is 12.

- height:

  The height of the saved plot in inches. Default is 8.

- base_filename:

  The base filename for saving the plot. Default is
  "timeline_histogram".

- title:

  The main title for the plot. Default is "Panel Study Timeline".

- x_label:

  The label for the x-axis. Default is "Date".

- y_label:

  The label for the y-axis. Default is "Count of Responses".

- color_palette:

  A vector of colors to use for the waves. If NULL, uses a default
  color-blind friendly palette.

- save_png:

  Logical. If TRUE, saves the plot as a PNG file. Default is FALSE.

- use_timestamp:

  Logical. If TRUE, includes a timestamp in the PNG filename. Default is
  FALSE.

## Value

A ggplot2 object representing the panel study response timeline.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)

# Assume we have a data frame 'nzavs_data' with columns: id, wave, tscore

# Step 1: Define NZAVS-specific wave breaks
nzavs_wave_breaks <- list(
  "time 1" = c(as.Date("2009-08-30"), as.Date("2010-10-15")),
  "time 2" = c(as.Date("2010-10-15"), as.Date("2011-10-15")),
  "time 3" = c(as.Date("2011-10-15"), as.Date("2012-10-15")),
  "time 4" = c(as.Date("2012-10-15"), as.Date("2013-10-15")),
  "time 5" = c(as.Date("2013-10-15"), as.Date("2014-10-15")),
  "time 6" = c(as.Date("2014-10-15"), as.Date("2015-10-15")),
  "time 7" = c(as.Date("2015-10-15"), as.Date("2016-10-15")),
  "time 8" = c(as.Date("2016-10-15"), as.Date("2017-10-15")),
  "time 9" = c(as.Date("2017-10-15"), as.Date("2018-10-15")),
  "time 10" = c(as.Date("2018-10-15"), as.Date("2019-10-15")),
  "time 11" = c(as.Date("2019-10-15"), as.Date("2020-10-15")),
  "time 12" = c(as.Date("2020-10-15"), as.Date("2021-10-15")),
  "time 13" = c(as.Date("2021-10-15"), as.Date("2022-10-15")),
  "time 14" = c(as.Date("2022-10-15"), as.Date("2023-10-15"))
)

# Step 2: Prepare the NZAVS data
prepared_data <- prepare_panel_data(
  dat = nzavs_data,
  wave_col = "wave",
  tscore_col = "tscore",
  id_col = "id",
  base_date = as.Date("2009-06-30"),
  wave_breaks = nzavs_wave_breaks
)

# Step 3: Create the NZAVS timeline plot
nzavs_timeline <- margot_plot_response_timeline(
  df_timeline = prepared_data$df_timeline,
  n_total_participants = prepared_data$n_total_participants,
  save = TRUE,
  save_png = TRUE,
  use_timestamp = TRUE,
  save_path = here::here("output", "plots"),
  title = "New Zealand Attitudes and Values Study (panel)",
  x_label = paste(
    "NZAVS years", min(prepared_data$df_timeline$day, na.rm = TRUE),
    "-", max(prepared_data$df_timeline$day, na.rm = TRUE),
    "cohort: daily counts by condition"
  ),
  y_label = "Count of Responses"
)

# Display the plot
print(nzavs_timeline)
} # }
```
