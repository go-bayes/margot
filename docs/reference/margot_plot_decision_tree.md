# Plot a stored policy assignment tree

Plot a stored policy assignment tree

## Usage

``` r
margot_plot_decision_tree(
  result_object,
  model_name = NULL,
  max_depth = NULL,
  original_df = NULL,
  x_padding = 0.12,
  y_padding = 0.25,
  border_size = 0.5,
  text_size = 4,
  edge_label_offset = 0.025,
  span_ratio = 0.4,
  non_leaf_fill = "lightyellow",
  title = NULL,
  plot_margin = grid::unit(c(1, 1, 1, 1), "cm"),
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE,
  remove_underscores = TRUE,
  remove_action_label = TRUE,
  label_mapping = NULL,
  show_leaf_metrics = FALSE,
  leaf_metrics = NULL,
  leaf_metric_digits = 3L,
  branch_labels = c(left = "True", right = "False"),
  layout_style = c("legacy", "compact"),
  node_label_width = NULL
)
```

## Arguments

- result_object:

  A list returned by
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)
  or
  [`margot_policy_tree_display()`](https://go-bayes.github.io/margot/reference/margot_policy_tree_display.md),
  or a native
  [`policytree::policy_tree()`](https://rdrr.io/pkg/policytree/man/policy_tree.html)
  tree. Native trees need no causal-forest wrapper; supply stored leaf
  labels explicitly if desired.

- model_name:

  Name of the model in the results to visualise. For a native tree, NULL
  takes the model recorded on `leaf_metrics` when supplied, otherwise
  `"model_tree"`; an unprefixed name is matched against the prefixed
  `leaf_metrics` model.

- max_depth:

  Maximum depth of the tree (1L or 2L). When `result_object` is a
  `margot_policy_tree_display` object, `NULL` uses the held-out selected
  depth stored with that model. For other supported objects, `NULL`
  retains the historical default of 2L.

- original_df:

  Optional dataframe with original data for showing untransformed values

- x_padding:

  Horizontal padding for the plot (proportion)

- y_padding:

  Vertical padding for the plot (proportion)

- border_size:

  Size of node borders in lines

- text_size:

  Size of text in plot elements

- edge_label_offset:

  Horizontal offset of edge labels from the connecting lines, in x data
  units. Legacy layouts span a unit interval; compact layouts index
  leaves 1 to n, so the same value moves labels a smaller fraction of
  the panel width.

- span_ratio:

  Controls the fixed aspect ratio of the legacy layout; ignored by the
  compact layout, which uses the available panel aspect.

- non_leaf_fill:

  Colour for non-leaf nodes (decision nodes)

- title:

  Optional literal title, preserved exactly. An empty string suppresses
  the title. NULL uses the formatted model label, or 'Policy tree' for a
  native tree.

- plot_margin:

  Margins around the plot

- remove_tx_prefix:

  Whether to remove treatment prefixes from variable names

- remove_z_suffix:

  Whether to remove z-suffixes from variable names

- use_title_case:

  Whether to use title case for variable names

- remove_underscores:

  Whether to replace underscores with spaces in variable names

- remove_action_label:

  Whether to remove "Action:" prefix from leaf node labels

- label_mapping:

  Optional list for renaming variables in the display

- show_leaf_metrics:

  Logical; if `TRUE`, leaf labels include signed treatment-control
  contrasts and sample shares from
  [`margot_policy_leaf_summary()`](https://go-bayes.github.io/margot/reference/margot_policy_leaf_summary.md).

- leaf_metrics:

  Optional data frame from
  [`margot_policy_leaf_summary()`](https://go-bayes.github.io/margot/reference/margot_policy_leaf_summary.md).
  If supplied, these labels are used instead of recomputing metrics.
  Native trees accept metrics whose recorded model matches `model_name`
  (see above) and whose depth matches `max_depth`.

- leaf_metric_digits:

  Integer; number of decimals for leaf treatment-control contrasts.

- branch_labels:

  A named character pair with names `left` and `right` (default
  True/False), `"condition"` for threshold inequalities, a data frame
  with `parent_id`, `side` and `label` identifying every edge, or a
  function taking an edge data frame and returning one label per edge.
  Edge metadata includes `parent_id`, `child_id`, `side`, `variable`,
  `threshold`, `original_threshold` and `threshold_label`. Left branches
  retain the inclusive inequality. Display thresholds use the same
  rounding as node labels; they do not replace the stored routing
  threshold.

- layout_style:

  `"legacy"` retains the historical geometry. `"compact"` centres
  parents over their ordered children, lets the panel use the available
  aspect ratio and reduces default outer margins and title spacing.
  Explicit padding and margin arguments take precedence. Supply an
  appropriate output height for the number of levels and label lines.

- node_label_width:

  Optional positive integer for wrapping node labels by character count;
  existing line breaks are retained. NULL preserves supplied labels.
