# Plot policy-tree decision and projection panels

Builds the standard two-panel policy-tree display: the branching
decision tree in panel A, with the projection plot of evaluation points
below in panel B.

## Usage

``` r
margot_plot_policy_tree_panels(
  result_object,
  model_name,
  max_depth = 2L,
  original_df = NULL,
  label_mapping = NULL,
  show_leaf_metrics = TRUE,
  leaf_metrics = NULL,
  layout = list(heights = c(1, 2)),
  annotation = list(tag_levels = "A"),
  projection_args = list(),
  decision_tree_args = list()
)
```

## Arguments

- result_object:

  A list returned by
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)
  or a compatible policy-tree workflow object.

- model_name:

  Character scalar naming the model to plot.

- max_depth:

  Integer, usually `1` or `2`.

- original_df:

  Optional data frame with original-scale variables.

- label_mapping:

  Optional named list used for display labels.

- show_leaf_metrics:

  Logical. If `TRUE`, the decision tree panel includes leaf `T-C`
  contrasts and sample shares.

- leaf_metrics:

  Optional output from
  [`margot_policy_leaf_summary()`](https://go-bayes.github.io/margot/reference/margot_policy_leaf_summary.md)
  or
  [`margot_table_policy_tree()`](https://go-bayes.github.io/margot/reference/margot_table_policy_tree.md).

- layout:

  List passed to
  [`margot_plot_policy_combo()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_combo.md)
  for panel heights.

- annotation:

  List passed to
  [`margot_plot_policy_combo()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_combo.md)
  for tags.

- projection_args:

  Optional list of arguments passed to
  [`margot_plot_policy_projection()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_projection.md).

- decision_tree_args:

  Optional list of arguments passed to
  [`margot_plot_policy_decision_tree()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_decision_tree.md).

## Value

A list with `decision_tree`, `projection`, and `combined_plot`.
