# Plot a Decision Tree from Margot Causal-Forest Results (robust labelling)

Plot a Decision Tree from Margot Causal-Forest Results (robust
labelling)

## Usage

``` r
margot_plot_decision_tree(
  result_object,
  model_name = NULL,
  max_depth = 2L,
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
  label_mapping = NULL
)
```

## Arguments

- result_object:

  A list returned by \`margot_causal_forest()\`

- model_name:

  Name of the model in the results to visualise

- max_depth:

  Maximum depth of the tree (1L or 2L)

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

  Offset for edge labels from connecting lines

- span_ratio:

  Controls the aspect ratio of the plot

- non_leaf_fill:

  Colour for non-leaf nodes (decision nodes)

- title:

  Optional custom title for the plot

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
