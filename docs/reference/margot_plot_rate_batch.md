# Batch Process and Plot RATE Curves for Multiple Models

This function processes a subset of models (or all models by default),
creates RATE (Rank Average Treatment Effect) plots for each model using
the margot package. Can plot either AUTOC or QINI RATE results.

## Usage

``` r
margot_plot_rate_batch(
  models_binary,
  model_names = NULL,
  target = c("AUTOC", "QINI"),
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  remove_underscores = TRUE,
  label_mapping = NULL,
  compute_on_demand = TRUE,
  q = seq(0.1, 1, by = 0.1),
  policy = "treat_best",
  use_oob_predictions = TRUE,
  seed = 12345
)
```

## Arguments

- models_binary:

  A list of model results, where each element contains a 'rate_result'
  and/or 'rate_qini' component.

- model_names:

  Optional character vector of model names to process. Default NULL (all
  models).

- target:

  Character; either "AUTOC" (default) or "QINI". Determines which RATE
  result to plot.

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

- compute_on_demand:

  Logical; if TRUE and RATE results are not pre-computed, will compute
  them on-demand from the causal forest (requires full_models to be
  saved). Default is FALSE.

- q:

  Numeric vector of quantiles at which to evaluate when computing
  on-demand. Default is seq(0.1, 1, by = 0.1).

- policy:

  Character; either "treat_best" or "withhold_best" when computing
  on-demand. Default is "treat_best".

- use_oob_predictions:

  Logical; if TRUE, use out-of-bag predictions when computing on-demand.
  Default is TRUE.

- seed:

  Optional random seed for reproducibility when computing on-demand.

## Value

A list containing the generated ggplot objects for each processed model.
