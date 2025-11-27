# create boxplots with covariates using ggplot2

this function creates boxplots for one outcome variable across specified
panel waves, allowing for different groups as covariates. it combines
features from margot_plot_boxplot() and margot_plot_slope_covariate().

## Usage

``` r
margot_plot_boxplot_covariate(
  data,
  outcome,
  covariate,
  waves = NULL,
  id_col = "id",
  title = NULL,
  y_label = NULL,
  x_label = "Wave",
  color_label = NULL,
  show_points = FALSE,
  point_alpha = 0.05,
  point_size = 0.5,
  include_timestamp = FALSE,
  save_path = NULL,
  prefix = NULL,
  width = 16,
  height = 8,
  legend_position = "right",
  y_limits = NULL,
  coord_flip = FALSE,
  ...
)
```

## Arguments

- data:

  a data frame containing the variables to be plotted.

- outcome:

  the name of the outcome variable to be plotted.

- covariate:

  the name of the covariate variable for grouping.

- waves:

  a vector of wave values to include in the plot (default is NULL, which
  includes all waves).

- id_col:

  name of the column containing unique identifiers (default is "id").

- title:

  the title of the plot (optional, auto-generated if NULL).

- y_label:

  the label for the y-axis (optional).

- x_label:

  the label for the x-axis (optional, defaults to "Wave").

- color_label:

  the label for the color legend (optional, defaults to the covariate
  name).

- show_points:

  logical, whether to show individual data points (default is FALSE).

- point_alpha:

  alpha value for data points if shown (default is 0.05).

- point_size:

  size of data points if shown (default is 0.5).

- include_timestamp:

  logical, whether to include timestamp in plot title and filename
  (default is FALSE).

- save_path:

  path to save the plot (optional).

- prefix:

  optional prefix for the saved file name (default is NULL).

- width:

  width of the saved plot in inches (default is 12).

- height:

  height of the saved plot in inches (default is 8).

- legend_position:

  position of the legend (default is "right").

- y_limits:

  y-axis limits (optional).

- coord_flip:

  logical, whether to flip the coordinates of the plot (default is
  FALSE).

- ...:

  additional arguments passed to geom_boxplot().

## Value

a ggplot object representing the boxplot with covariates.

## Examples

``` r
if (FALSE) { # \dontrun{
# example 1: basic usage with all waves
p1 <- margot_plot_boxplot_covariate(
  data = your_data,
  outcome = "env_climate_chg_concern",
  covariate = "education",
  id_col = "id"
)

# example 2: plotting specific waves with custom labels
p2 <- margot_plot_boxplot_covariate(
  data = your_data,
  outcome = "political_orientation",
  covariate = "age_group",
  waves = c(2021, 2022, 2023),
  y_label = "Political Orientation",
  color_label = "Age Group",
  id_col = "id"
)

# example 3: showing individual points and flipping coordinates
p3 <- margot_plot_boxplot_covariate(
  data = your_data,
  outcome = "env_sat_nz_environment",
  covariate = "income_bracket",
  show_points = TRUE,
  coord_flip = TRUE,
  y_label = "Satisfaction with NZ Environment",
  color_label = "Income Bracket",
  id_col = "id"
)

# example 4: customizing plot appearance and saving
p4 <- margot_plot_boxplot_covariate(
  data = your_data,
  outcome = "envefficacy",
  covariate = "gender",
  y_label = "Environmental Efficacy",
  color_label = "Gender",
  legend_position = "bottom",
  y_limits = c(1, 7),
  save_path = "path/to/save",
  prefix = "env_efficacy",
  width = 10,
  height = 6,
  id_col = "id"
)

# example 5: using with categorical outcome and including timestamp
p5 <- margot_plot_boxplot_covariate(
  data = your_data,
  outcome = "env_climate_chg_cause",
  covariate = "political_party",
  y_label = "Perceived Cause of Climate Change",
  color_label = "Political Party",
  include_timestamp = TRUE,
  id_col = "id"
)
} # }
```
