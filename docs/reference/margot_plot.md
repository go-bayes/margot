# Create a Margot Plot with Proper Multiplicity Correction

Create coordinated plots, tables and text from supplied effect estimates
and confidence intervals, with the requested multiplicity adjustment.
Model-scale estimates and sensitivity quantities remain separate from
reported unit conversions.

## Usage

``` r
margot_plot(
  .data,
  type = c("RD", "RR"),
  order = c("alphabetical", "magnitude_desc", "magnitude_asc", "evaluebound_desc",
    "evaluebound_asc", "custom", "default"),
  custom_order = NULL,
  title_binary = NULL,
  include_coefficients = TRUE,
  standardize_label = c("NZ", "US", "none"),
  e_val_bound_threshold = 1.2,
  adjust = c("none", "bonferroni", "holm", "BH"),
  alpha = 0.05,
  ...,
  options = list(),
  label_mapping = NULL,
  save_output = FALSE,
  use_timestamp = FALSE,
  base_filename = "margot_plot_output",
  prefix = NULL,
  save_path = here::here("push_mods"),
  original_df = NULL,
  bold_rows = FALSE,
  rename_cols = FALSE,
  col_renames = list(`E-Value` = "E_Value", `E-Value bound` = "E_Val_bound"),
  rename_ate = FALSE,
  rename_evalue = FALSE,
  scale_info = NULL
)
```

## Arguments

- .data:

  data frame containing causal effect estimates with columns for effect
  sizes, confidence intervals, E-values and E-value bounds

- type:

  character. type of effect estimate: "RD" (risk difference) or "RR"
  (risk ratio)

- order:

  Outcome ordering rule; the table and text follow the graph from top to
  bottom.

- custom_order:

  Outcome labels in the requested custom factor order.

- title_binary:

  Retained compatibility argument.

- include_coefficients:

  Whether to print numerical coefficients on the plot.

- standardize_label:

  Axis-label convention: New Zealand, US, or no standardisation label.

- e_val_bound_threshold:

  E-value lower-bound threshold for colouring and prose selection.

- adjust:

  character. multiplicity correction method: "none", "bonferroni"

- alpha:

  numeric. significance level for corrections

- ...:

  other parameters as in original function

- options:

  Plotting options; explicit arguments override corresponding option
  entries.

- label_mapping:

  Optional mapping from source outcome names to display labels.

- save_output:

  Whether to save the complete reporting list.

- use_timestamp:

  Whether to append a timestamp to saved filenames.

- base_filename:

  Base name for the saved reporting object.

- prefix:

  Optional saved-filename prefix.

- save_path:

  Directory for saved output.

- original_df:

  Legacy unstandardised source data for inferred scale metadata.
  Explicit saved metadata is preferred.

- bold_rows:

  Whether table row labels above the reporting threshold receive
  Markdown emphasis.

- rename_cols:

  Whether to apply the requested table column-name mapping.

- col_renames:

  Named mapping from new table column names to existing names.

- rename_ate:

  Whether to rename the estimate column, or a supplied replacement name.

- rename_evalue:

  Whether to use display names for E-value columns.

- scale_info:

  Optional data frame keyed by the original outcome name, with
  transformation (\`identity\`, \`log\`, or \`log1p\`), saved \`center\`
  and positive \`scale\`, \`orientation\` (1 or -1), \`unit\`, and
  positive \`unit_multiplier\`. Constants describe the model outcome as
  orientation \* (g(Y) - center) / scale. Explicit metadata overrides
  inference from \`original_df\`.

## Value

An invisible list with \`plot\`, \`interpretation\`, and
\`transformed_table\`.
