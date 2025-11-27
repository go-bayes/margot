# Interpret Policy Tree Results

This function creates an interpretation of policy tree results from a
causal forest or multi-arm causal forest model. It generates a formatted
description of the policy tree, including the main splits and
recommended actions.

## Usage

``` r
margot_interpret_policy_tree(
  model,
  model_name,
  max_depth = 2L,
  train_proportion = 0.5,
  custom_action_names = NULL,
  label_mapping = NULL,
  original_df = NULL,
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  include_conditional_means = TRUE,
  use_math_notation = FALSE,
  output_format = c("prose", "bullet"),
  report_policy_value = c("none", "treat_all", "control_all", "both", "treated_only"),
  policy_value_R = 499L,
  policy_value_seed = 42L,
  policy_value_ci_level = 0.95
)
```

## Arguments

- model:

  A list containing the results from a multi-arm causal forest model.

- model_name:

  A string specifying which model's results to interpret.

- max_depth:

  Integer, 1 or 2; which stored tree to interpret.

- train_proportion:

  Numeric value between 0 and 1 for the proportion of data used for
  training. Default is 0.5.

- custom_action_names:

  Optional vector of custom names for the actions. Must match the number
  of actions in the policy tree.

- label_mapping:

  Optional list that maps variable names to custom labels.

- original_df:

  Optional dataframe with untransformed variables, used to display split
  values on the data scale.

- remove_tx_prefix:

  Logical indicating whether to remove prefixes like t0\_ from variable
  names. Default is TRUE.

- remove_z_suffix:

  Logical indicating whether to remove the \_z suffix from variable
  names. Default is TRUE.

- use_title_case:

  Logical indicating whether to convert variable names to title case.
  Default is TRUE.

- include_conditional_means:

  Logical indicating whether to include conditional means information if
  available. Default is TRUE.

- use_math_notation:

  Logical indicating whether to use mathematical notation (E\[Y(a)\|X\])
  or plain language. Default is FALSE for clarity.

- output_format:

  Character string specifying output format: "prose" (default) for
  flowing narrative text, or "bullet" for structured bullet points.

- report_policy_value:

  Character: one of "none" (default), "treat_all", "control_all", or
  "both". If not "none", appends policy value summary with 95 stored in
  \`plot_data\` and DR scores from the model.

- policy_value_R:

  Integer ≥ 199; number of bootstrap replicates (default 499).

- policy_value_seed:

  Integer or NULL; RNG seed (default 42).

- policy_value_ci_level:

  Numeric confidence level (default 0.95).

## Value

Invisibly returns a string containing the interpretation; also prints it
to the console.
