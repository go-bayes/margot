# Prepare Panel Data for Timeline Visualization

This function prepares panel data for timeline visualization across
multiple waves.

## Usage

``` r
prepare_panel_data(
  dat,
  wave_col = "wave",
  tscore_col = "tscore",
  id_col = "id",
  base_date = as.Date("1970-01-01"),
  wave_breaks = NULL
)
```

## Arguments

- dat:

  A data frame containing the panel data. Must include columns for wave,
  time score, and participant ID.

- wave_col:

  The name of the column containing wave information. Default is "wave".

- tscore_col:

  The name of the column containing time score information. Default is
  "tscore".

- id_col:

  The name of the column containing participant IDs. Default is "id".

- base_date:

  The base date for calculating the timeline. Default is "1970-01-01".

- wave_breaks:

  A named list of date ranges for each wave. If NULL, waves will not be
  categorized.

## Value

A list containing two elements:

- df_timeline:

  A data frame with the processed timeline data

- n_total_participants:

  The total number of unique participants in the dataset

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- read.csv("panel_data.csv")
wave_breaks <- list(
  "wave 1" = c(as.Date("2010-01-01"), as.Date("2010-12-31")),
  "wave 2" = c(as.Date("2011-01-01"), as.Date("2011-12-31"))
)
prepared_data <- prepare_panel_data(dat,
  wave_col = "Wave", tscore_col = "TimeScore",
  id_col = "ParticipantID", base_date = "2010-01-01",
  wave_breaks = wave_breaks
)
} # }
```
