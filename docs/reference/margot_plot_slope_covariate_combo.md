# Create a Combined Slope Plot using ggeffects and patchwork

This function creates multiple ggplot2 visualizations using ggeffects to
calculate predicted responses from models for multiple outcome variables
and combines them into a single plot using the patchwork package. It
allows flexible specification of the models and plotting options,
including layout and annotations.

## Usage

``` r
margot_plot_slope_covariate_combo(
  data,
  outcome_vars,
  exposure_formula,
  terms,
  label_mapping = NULL,
  x_label = NULL,
  color_label = NULL,
  save_path = NULL,
  file_prefix = "plot_slope_covariate_batch",
  ncol = NULL,
  nrow = NULL,
  guides = "collect",
  patchwork_params = list(),
  plot_annotation_params = list(),
  caption_size = 10,
  include_individual_titles = FALSE,
  width = 12,
  height = 8,
  dpi = 400,
  ...
)
```

## Arguments

- data:

  A data frame containing the variables for the models.

- outcome_vars:

  A character vector specifying the outcome variables to be modeled.

- exposure_formula:

  A formula specifying the exposure variables (right-hand side of the
  model).

- terms:

  A character vector specifying the terms to be used in
  predict_response.

- label_mapping:

  An optional named list mapping outcome variables to custom y-axis
  labels.

- x_label:

  An optional label for the x-axis. If NULL, the first term will be
  used.

- color_label:

  An optional label for the color legend. If NULL, the second term will
  be used.

- save_path:

  An optional path to save the combined plot. If NULL, the plot will not
  be saved.

- file_prefix:

  Optional prefix for the saved file name (default is
  "plot_slope_covariate_batch").

- ncol:

  Number of columns in the combined plot layout.

- nrow:

  Number of rows in the combined plot layout.

- guides:

  How to combine legends. Default is "collect".

- patchwork_params:

  A list of additional parameters to be passed to
  patchwork::plot_layout().

- plot_annotation_params:

  A list of parameters to be passed to patchwork::plot_annotation().

- include_individual_titles:

  Logical, whether to include titles in individual plots (default is
  FALSE).

- width:

  Width of the combined plot in inches. Default is 12.

- height:

  Height of the combined plot in inches. Default is 8.

- dpi:

  Resolution of the saved plot. Default is 400.

- ...:

  Additional arguments to be passed to margot_plot_slope_covariate().

## Value

A ggplot2 object representing the combined plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define outcome variables and label mapping
outcome_vars <- c("var1", "var2", "var3")
label_mapping <- list("var1" = "Variable 1", "var2" = "Variable 2", "var3" = "Variable 3")

# Create combined plot
combined_plot <- margot_plot_slope_covariate_combo(
  data = dat,
  outcome_vars = outcome_vars,
  exposure_formula = "~ wave:covariate",
  terms = c("wave", "covariate"),
  label_mapping = label_mapping,
  x_label = "Time",
  color_label = "Covariate",
  ncol = 2,
  plot_annotation_params = list(
    title = "Combined Slope Plots",
    subtitle = "Subtitle for the combined plot"
  ),
  save_path = "path/to/save/directory",
  width = 14,
  height = 10
)
} # }
```
