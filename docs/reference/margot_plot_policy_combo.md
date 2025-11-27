# Create a Combined Decision Tree and Policy Relationship Graph

This function generates a combined plot consisting of a decision tree
and a graph showing relationships between variables in the recommended
policy.

## Usage

``` r
margot_plot_policy_combo(
  result_object,
  model_name,
  max_depth = 2L,
  label_mapping = NULL,
  original_df = NULL,
  layout = list(heights = c(1, 2)),
  annotation = list(tag_levels = "A"),
  generate_policy_tree = TRUE,
  generate_decision_tree = TRUE,
  policy_tree_args = list(),
  decision_tree_args = list()
)
```

## Arguments

- result_object:

  An object containing the results from a multi-arm causal forest model.

- model_name:

  A character string specifying the name of the model.

- max_depth:

  Integer, 1 or 2; which decision tree depth to plot. Default: 2.

- label_mapping:

  Optional named list for custom label mappings.

- original_df:

  Optional dataframe with untransformed variables.

- layout:

  A list specifying the layout of the combined plot when max_depth==2.
  Default is \`list(heights = c(1, 2))\`, which sets the relative
  heights of the two plots.

- annotation:

  A list specifying the annotation for the combined plot when
  max_depth==2. Default is \`list(tag_levels = "A")\`, which adds
  alphabetic tags to the subplots.

- generate_policy_tree:

  Logical, whether to generate the policy tree plot. Default is TRUE.

- generate_decision_tree:

  Logical, whether to generate the decision tree plot. Default is TRUE.

- policy_tree_args:

  A list of arguments to pass to \`margot_plot_policy_tree\`. Default is
  list().

- decision_tree_args:

  A list of arguments to pass to \`margot_plot_decision_tree\`. Default
  is list().

## Value

A list containing:

- policy_tree:

  A ggplot object representing the policy tree (if generated)

- decision_tree:

  A ggplot object representing the decision tree (if generated)

- combined_plot:

  A ggplot object representing the combined plot (if both plots are
  generated)
