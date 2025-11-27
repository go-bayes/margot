# Create a Slope Plot using ggeffects

This function creates a ggplot2 visualization using ggeffects to
calculate predicted responses from a model. It allows flexible
specification of the model and plotting options. The function
automatically handles NA and infinite values, and reports the number of
unique participants and observations used in the analysis.

## Usage

``` r
margot_plot_slope_covariate(
  data,
  formula,
  terms,
  id_col = "id",
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  y_limits = c(1, 7),
  color_label = NULL,
  include_title = TRUE,
  include_timestamp = FALSE,
  save_path = NULL,
  prefix = NULL,
  width = 12,
  height = 8,
  seed = 12345,
  ...
)
```

## Arguments

- data:

  A data frame containing the variables for the model.

- formula:

  A formula specifying the model to be fit.

- terms:

  A character vector specifying the terms to be used in
  predict_response.

- id_col:

  Name of the column containing unique identifiers (default is "id").

- title:

  An optional title for the plot. If NULL, an automatic title will be
  generated.

- y_label:

  An optional label for the y-axis. If NULL, the response variable name
  will be used.

- x_label:

  An optional label for the x-axis. If NULL, the first term will be
  used.

- y_limits:

  An optional vector of two numbers specifying the y-axis limits.
  Default is c(1, 7).

- color_label:

  An optional label for the color legend. If NULL, the second term will
  be used.

- include_title:

  Logical, whether to include the plot title (default is TRUE).

- include_timestamp:

  Logical, whether to include timestamp in plot title and filename
  (default is FALSE).

- save_path:

  An optional path to save the plot. If NULL, the plot will not be
  saved.

- prefix:

  Optional prefix for the saved file name (default is NULL).

- width:

  The width of the saved plot in inches. Default is 12.

- height:

  The height of the saved plot in inches. Default is 8.

- seed:

  An optional seed for reproducibility.

- ...:

  Additional arguments to be passed to ggeffects::predict_response.

## Value

A ggplot2 object representing the plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage remains the same
} # }
```
