# Batch Processing of Policy Trees and Related Visualisations

Process a list of multi-arm causal forest results: generate policy-tree
and decision-tree plots, Qini curves, and difference-gain summaries.
Users can toggle which outputs to include via the \`output_objects\`
parameter.

## Usage

``` r
margot_policy(
  result_outcomes,
  policy_tree_args = list(),
  decision_tree_args = list(),
  max_depth = 2L,
  depths_by_model = NULL,
  spend_levels = c(0.1, 0.4),
  label_mapping = NULL,
  original_df = NULL,
  model_names = NULL,
  output_objects = c("policy_tree", "decision_tree", "combined_plot", "qini_plot",
    "diff_gain_summaries"),
  qini_args = list(),
  baseline_method = "maq_no_covariates",
  seed = 12345
)
```

## Arguments

- result_outcomes:

  List returned by `margot_multi_arm_causal_forest()`.

- policy_tree_args:

  List of args for
  [`margot_plot_policy_tree()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_tree.md).
  Default: [`list()`](https://rdrr.io/r/base/list.html).

- decision_tree_args:

  List of args for
  [`margot_plot_decision_tree()`](https://go-bayes.github.io/margot/reference/margot_plot_decision_tree.md).
  Default: [`list()`](https://rdrr.io/r/base/list.html).

- max_depth:

  Integer, 1 or 2; fallback depth used when no per-model mapping is
  supplied via \`model_names\` or \`depths_by_model\` (default 2).

- depths_by_model:

  Optional named vector/list mapping models (with or without \`model\_\`
  prefix) to depth 1 or 2; merged with any depth hints supplied by
  \`model_names\`.

- spend_levels:

  Numeric vector of spend levels for difference-gain summaries. Default:
  `0.1`.

- label_mapping:

  Named list mapping variable names to display labels. Default: NULL.

- original_df:

  Optional data.frame of untransformed variables for axis annotations.
  Default: NULL.

- model_names:

  Character vector of model names to process. If named, values should be
  1 or 2 (depth assignments) and take precedence over \`max_depth\` and
  \`depths_by_model\`; NULL = all.

- output_objects:

  Character vector specifying which outputs to include. Options:
  "policy_tree", "decision_tree", "combined_plot", "qini_plot",
  "diff_gain_summaries". Default: all.

- qini_args:

  List of additional arguments to pass to margot_plot_qini(). Default:
  list().

- baseline_method:

  Method for generating baseline: "maq_no_covariates" (default), "auto",
  "simple", "maq_only", or "none". See details in
  margot_generate_qini_data().

- seed:

  Integer. Random seed for reproducibility in QINI computations (default
  12345).

## Value

A named list; each element corresponds to a model and contains only the
requested outputs. The returned object includes an attribute
\`depth_map\` giving the depth used for each model.
