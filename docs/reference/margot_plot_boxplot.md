# Create panel data Boxplots using ggplot2

This function creates boxplots for one or more variables across
specified panel waves. It offers various customisation options for the
plot appearance and layout.

## Usage

``` r
margot_plot_boxplot(
  data,
  y_vars,
  waves = NULL,
  id_col = "id",
  title = NULL,
  y_label = NULL,
  x_label = "Wave",
  show_points = FALSE,
  point_alpha = 0.05,
  point_size = 0.5,
  include_timestamp = FALSE,
  save_path = NULL,
  prefix = NULL,
  width = 16,
  height = 8,
  legend_position = "bottom",
  y_limits = NULL,
  facet_scales = "free_y",
  facet_ncol = NULL,
  facet_nrow = NULL,
  coord_flip = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing the variables to be plotted.

- y_vars:

  A list of variable names to be plotted on the y-axis.

- waves:

  A vector of wave values to include in the plot (default is NULL, which
  includes all waves).

- id_col:

  Name of the column containing unique identifiers (default is "id").

- title:

  The title of the plot (optional, auto-generated if NULL).

- y_label:

  The label for the y-axis (optional).

- x_label:

  The label for the x-axis (optional, defaults to "Wave").

- show_points:

  Logical, whether to show individual data points (default is FALSE).

- point_alpha:

  Alpha value for data points if shown (default is 0.3).

- point_size:

  Size of data points if shown (default is 0.5).

- include_timestamp:

  Logical, whether to include timestamp in plot title and filename
  (default is FALSE).

- save_path:

  Path to save the plot (optional).

- prefix:

  Optional prefix for the saved file name (default is NULL).

- width:

  Width of the saved plot in inches (default is 16).

- height:

  Height of the saved plot in inches (default is 8).

- legend_position:

  Position of the legend (default is "bottom").

- y_limits:

  Y-axis limits (optional).

- facet_scales:

  Scales for facet panels (default is "free_y").

- facet_ncol:

  Number of columns for facet_wrap (optional).

- facet_nrow:

  Number of rows for facet_wrap (optional).

- coord_flip:

  Logical, whether to flip the coordinates of the plot (default is
  FALSE).

- ...:

  Additional arguments passed to geom_boxplot().

## Value

A ggplot object representing the boxplot.

## Examples

``` r
if (FALSE) { # \dontrun{
# define outcome variables
outcome_vars <- c(
  "env_climate_chg_concern",
  "env_climate_chg_cause",
  "env_climate_chg_real",
  "env_sat_nz_environment",
  "envefficacy"
)

# basic usage with all waves
p1 <- margot_plot_boxplot(
  data = your_data,
  y_vars = outcome_vars,
  id_col = "id"
)

# plotting specific waves with points shown and coordinates flipped
p2 <- margot_plot_boxplot(
  data = your_data,
  y_vars = outcome_vars,
  waves = c(2021, 2022),
  show_points = TRUE,
  coord_flip = TRUE,
  id_col = "id"
)

# saving the plot with a custom prefix
margot_plot_boxplot(
  data = your_data,
  y_vars = outcome_vars,
  waves = c(2021, 2022, 2023),
  save_path = "path/to/save",
  prefix = "climate_change",
  include_timestamp = TRUE,
  id_col = "id"
)

# customizing the plot appearance with flipped coordinates
p3 <- margot_plot_boxplot(
  data = your_data,
  y_vars = c("env_climate_chg_concern", "envefficacy"),
  waves = c(2021, 2022),
  title = "Climate Change Concern and Efficacy",
  y_label = "Score",
  legend_position = "right",
  facet_scales = "free",
  coord_flip = TRUE,
  id_col = "id"
)
} # }
```
