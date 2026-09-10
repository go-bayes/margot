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
  ...,
  jitter_width = 0.3,
  jitter_height = NULL,
  jitter_seed = NA,
  jitter_method = c("standard", "within_splits", "band_boundary"),
  display_weights = NULL,
  weight_max_size = 4
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
  \`title_size\`).

- jitter_width:

  Maximum horizontal displacement in plotted coordinates; default 0.3.

- jitter_height:

  Maximum vertical displacement in plotted coordinates. \`NULL\` retains
  0.06 for depth one and 0.3 for depth two.

- jitter_seed:

  Seed passed to \`ggplot2::position_jitter\`; supply an integer for
  reproducible positions without changing the caller's random-number
  state.

- jitter_method:

  \`"standard"\` retains ordinary symmetric jitter. \`"within_splits"\`
  samples within the displacement bounds and the interval containing the
  original value, preserving every displayed variable's tree split
  inequality, including equality at the cut point. Bounds follow
  continuous scale transformations. Predictions always use the original
  data. \`"band_boundary"\` (depth one only) retains symmetric jitter
  and places the separator at the outer edge of the inclusive jitter
  band. The width is capped uniformly at 49 sides, in transformed
  plotting coordinates. Closely spaced values therefore receive little
  horizontal jitter. The tree and subtitle retain the fitted cut point;
  the displaced line separates displayed bands, not numeric values at
  the fitted threshold. With no observations on one side, horizontal
  jitter is zero and the line stays at the fitted cut. Jitter is a
  display device, not a model of measurement error.

- display_weights:

  Optional finite non-negative weights aligned with every prediction
  row; at least one must be positive. Point area represents these
  weights, with a common scale across depth-two panels. Weighted
  projections retain exact predictor coordinates (vertical jitter only
  for a stump) and display each row in its applicable root branch,
  irrespective of shading. Existing unweighted jitter defaults remain
  unchanged.

- weight_max_size:

  Maximum weighted point size in millimetres; default 4.

## Value

A \`ggplot\` object (depth 1) or a patchwork object (depth 2).

## Details

\* \*\*Depth 1\*\* – a one–dimensional jitter plot coloured by the
predicted action and annotated with the split point. \* \*\*Depth 2\*\*
– the existing two-panel scatter-plot (hand-off to
\`margot_plot_policy_tree_depth2()\`).
