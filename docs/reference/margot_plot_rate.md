# Plot Rank Average Treatment Effect

This function creates a ggplot visualisation of the Rank Average
Treatment Effect. It displays the estimate with a confidence interval,
using a simple black line and light gray shading by default. Can either
plot an existing RATE object or compute RATE on-demand from a causal
forest.

## Usage

``` r
margot_plot_rate(
  x,
  outcome_var,
  title = NULL,
  subtitle = "(95% confidence interval shown as shaded area)",
  x_lab = "Treated fraction (q)",
  y_lab = "Estimate",
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  remove_underscores = TRUE,
  label_mapping = NULL,
  target = "AUTOC",
  tau_hat = NULL,
  q = seq(0.1, 1, by = 0.1),
  policy = "treat_best",
  subset = NULL,
  use_oob_predictions = TRUE,
  seed = 12345,
  ...
)
```

## Arguments

- x:

  Either an object of class "rank_average_treatment_effect" or a
  causal_forest object from grf. If a causal_forest is provided, RATE
  will be computed on-demand.

- outcome_var:

  A character string specifying the name of the outcome variable to
  plot. This is used for the plot title with proper label
  transformation.

- title:

  Character string for the plot title. If NULL (default), a title is
  generated using the transformed outcome variable name.

- subtitle:

  Character string for the plot subtitle. Default explains the
  confidence interval.

- x_lab:

  Character string for the x-axis label. Default is "Treated fraction
  (q)".

- y_lab:

  Character string for the y-axis label. Default is "Estimate".

- remove_tx_prefix:

  Logical value indicating whether to remove the "tx\_" prefix from
  labels. Default is TRUE.

- remove_z_suffix:

  Logical value indicating whether to remove the "\_z" suffix from
  labels. Default is TRUE.

- use_title_case:

  Logical value indicating whether to convert labels to title case.
  Default is TRUE.

- remove_underscores:

  Logical value indicating whether to remove underscores from labels.
  Default is TRUE.

- label_mapping:

  Optional named list for custom label mappings. Keys should be original
  variable names (with or without "model\_" prefix), and values should
  be the desired display labels. Default is NULL.

- target:

  Character; either "AUTOC" or "QINI". Only used when x is a
  causal_forest. Default is "AUTOC".

- tau_hat:

  Optional vector of treatment effect estimates. Only used when x is a
  causal_forest. If NULL, will be computed using out-of-bag predictions.

- q:

  Numeric vector of quantiles at which to evaluate. Default is seq(0.1,
  1, by = 0.1). Only used when x is a causal_forest.

- policy:

  Character; either "treat_best" or "withhold_best". Only used when x is
  a causal_forest. Default is "treat_best".

- subset:

  Optional indices for subsetting the evaluation data. Only used when x
  is a causal_forest.

- use_oob_predictions:

  Logical; if TRUE and tau_hat is NULL, use out-of-bag predictions. Only
  used when x is a causal_forest. Default is TRUE.

- seed:

  Optional random seed for reproducibility. Only used when x is a
  causal_forest.

- ...:

  Additional arguments passed to ggplot.

## Value

A ggplot object that can be further customised or printed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming rate_eval is your rank_average_treatment_effect object
p <- margot_plot_rate(rate_eval, "model_t2_belong_z")
print(p)

# Using custom label mapping
label_mapping <- list(
  "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
  "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
)
p <- margot_plot_rate(rate_eval, "model_t2_env_not_env_efficacy_z",
  label_mapping = label_mapping
)
print(p)
} # }
```
