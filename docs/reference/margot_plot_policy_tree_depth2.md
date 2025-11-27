# plot a policy tree (depth 2)

visualise the first two splits of a \`policytree\` object stored inside
a multi-arm causal-forest result. each panel shows the primary split
variable on the x-axis against one of the two secondary split variables.

## Usage

``` r
margot_plot_policy_tree_depth2(
  result_object,
  model_name,
  original_df = NULL,
  shading = TRUE,
  color_scale = NULL,
  point_alpha = 0.5,
  theme_function = ggplot2::theme_classic,
  label_mapping = NULL,
  label_options = list(remove_tx_prefix = TRUE, remove_z_suffix = TRUE,
    remove_underscores = TRUE, use_title_case = TRUE),
  title_size = 16,
  subtitle_size = 14,
  axis_title_size = 14,
  legend_title_size = 14,
  jitter_width = 0.3,
  jitter_height = 0.3,
  split_line_color = "red",
  split_line_alpha = 0.7,
  split_line_type = "dashed",
  split_line_linewidth = 0.5,
  split_label_size = 10,
  split_label_color = "red",
  custom_action_names = NULL,
  legend_position = "bottom",
  plot_selection = "both",
  shade_fill = "#6e6e6e",
  shade_alpha = 0.35,
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

## Details

this version lets you fine-tune the placement of both annotations
independently: one for the top-of-panel (x split) and one for the y-axis
(y split).
