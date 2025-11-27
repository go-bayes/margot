# Plot a policy tree (depth-adaptive)

Visualise the first one or two splits of a \`policytree\` stored inside
a multi-arm causal-forest result.

## Usage

``` r
margot_plot_policy_tree(
  result_object,
  model_name,
  max_depth = 2L,
  original_df = NULL,
  shading = NULL,
  color_scale = NULL,
  point_alpha = 0.5,
  theme_function = ggplot2::theme_classic,
  label_mapping = NULL,
  label_options = list(remove_tx_prefix = TRUE, remove_z_suffix = TRUE,
    remove_underscores = TRUE, use_title_case = TRUE),
  ...
)
```

## Arguments

- result_object:

  A list produced by \`margot_multiclass_cf()\` (or similar) whose
  \`results\` slot holds \`policy_tree_depth_1\`,
  \`policy_tree_depth_2\`, and \`plot_data\` entries for \`model_name\`.

- model_name:

  Character scalar identifying the result inside
  \`result_object\$results\`.

- max_depth:

  Integer, 1 or 2; which stored tree to visualise.

- original_df:

  Optional data frame of raw-scale variables (only used by the depth-2
  plot for secondary-axis labels).

- shading:

  Logical – draw shaded half-planes for non-decision regions (depth-2
  only). If \`NULL\` (default) the function decides automatically.

- color_scale:

  A pre-built \`ggplot2\` colour scale (rarely needed).

- point_alpha:

  Alpha transparency for plotted points.

- theme_function:

  A \`ggplot2\` theme function; default \`ggplot2::theme_classic\`.

- label_mapping:

  Named list for explicit string replacements; passed to
  \`transform_label()\`.

- label_options:

  List of logical flags understood by \`transform_label()\` (see that
  function for details).

- ...:

  Extra arguments forwarded \*\*only\*\* to the depth-2 helper (e.g.
  \`title_size\`, \`jitter_width\`, etc.).

## Value

A \`ggplot\` object (depth 1) or a patchwork object (depth 2).

## Details

\* \*\*Depth 1\*\* – a one–dimensional jitter plot coloured by the
predicted action and annotated with the split point. \* \*\*Depth 2\*\*
– the existing two-panel scatter-plot (hand-off to
\`margot_plot_policy_tree_depth2()\`).
