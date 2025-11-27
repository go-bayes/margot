# Batch process policy tree interpretations

This function now accepts a vector of model names to process and
produces a single combined output. The common description is printed
once at the top, followed by each model's specific findings. You can now
control whether to interpret depth-1 or depth-2 trees via the
\`max_depth\` argument, or supply per-model depth choices via
\`depths_by_model\`.

## Usage

``` r
margot_interpret_policy_batch(
  models,
  model_names = NULL,
  max_depth = 2L,
  depths_by_model = NULL,
  save_path = NULL,
  prefix = NULL,
  include_timestamp = FALSE,
  report_policy_value = c("none", "treat_all", "control_all", "both", "treated_only"),
  policy_value_R = 499L,
  policy_value_seed = 42L,
  policy_value_ci_level = 0.95,
  brief = FALSE,
  brief_save_to = NULL,
  return_as_list = FALSE,
  policy_value_source = c("compute", "use_coherent"),
  coherent_values = NULL,
  ...
)
```

## Arguments

- models:

  A list containing the results from multi-arm causal forest models.

- model_names:

  A character vector of model names to interpret. If named, the values
  should be 1 or 2 (depth assignments) and take precedence over
  \`max_depth\` and \`depths_by_model\`. If NULL, all models are
  processed.

- max_depth:

  Integer, 1 or 2; fallback depth used when no per-model depth mapping
  is provided via \`model_names\` or \`depths_by_model\` (default 2).

- depths_by_model:

  Optional named vector/list mapping models (with or without \`model\_\`
  prefix) to depth 1 or 2; combined with any depth hints supplied
  through \`model_names\`.

- save_path:

  The path where the combined interpretation will be saved. If NULL,
  nothing is saved.

- prefix:

  An optional prefix for the filename.

- include_timestamp:

  Logical; whether to include a timestamp in the filename (if desired).

- report_policy_value:

  Character: one of "none" (default), "treat_all", "control_all", or
  "both". If not "none", each model interpretation will include a
  one-line policy value summary with 95% CIs based on bootstrap SEs.

- policy_value_R:

  Integer \>= 199; bootstrap replicates (default 499).

- policy_value_seed:

  Integer or NULL; RNG seed (default 42).

- policy_value_ci_level:

  Numeric confidence level (default 0.95).

- brief:

  Logical; if TRUE, prepend a compact treated-only summary for each
  model (coverage treated and average uplift among treated) and
  optionally save it.

- brief_save_to:

  Optional path to save the brief treated-only summary as text.

- ...:

  Additional arguments to pass to margot_interpret_policy_tree(),
  including include_conditional_means (default TRUE), use_math_notation
  (default FALSE), output_format ("bullet" or "prose"), original_df,
  label_mapping, and policy value options.

## Value

A single character string containing the combined markdown output. When
\`return_as_list = TRUE\`, returns a list with: - \`report_detail\`: The
full detailed interpretation text - \`report_brief\`: Brief treated-only
summary lines (if \`brief = TRUE\`) - \`by_model\`: List of per-model
interpretation sections - \`policy_value_explanation\`: Short
explanation of policy value terms - \`model_depths\` / \`depth_map\`:
Named vector of depths used per model
