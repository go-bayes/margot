# Batch Processing of Policy Trees and Related Visualizations (Deprecated)

This function is deprecated as of margot 0.2.1.65. Please use
margot_policy() instead.

## Usage

``` r
margot_batch_policy(
  result_outcomes,
  policy_tree_args = list(),
  decision_tree_args = list(),
  dpi = 600,
  width = 12,
  height = 12,
  save_plots = TRUE,
  output_dir = here::here(push_mods),
  spend_levels = 0.1,
  label_mapping = NULL
)
```

## Arguments

- result_outcomes:

  A list containing the results from margot_multi_arm_causal_forest().

- policy_tree_args:

  A list of arguments to pass to margot_plot_policy_tree(). Default is
  list().

- decision_tree_args:

  A list of arguments to pass to margot_plot_decision_tree(). Default is
  list().

- dpi:

  The resolution of saved plots in dots per inch. Default is 600.

- width:

  The width of saved plots in inches. Default is 12.

- height:

  The height of saved plots in inches. Default is 12.

- save_plots:

  Logical indicating whether to save plots to disk. Default is TRUE.

- output_dir:

  The directory to save plots in. Default is here::here(push_mods).

- spend_levels:

  A vector of spend levels to use for difference gain summaries. Default
  is 0.1.

- label_mapping:

  Optional named list for custom label mappings. Keys should be original
  variable names (with or without "model\_" prefix), and values should
  be the desired display labels. Default is NULL.

## Value

A list where each element corresponds to a model in the input
\`result_outcomes\`. Each element is itself a list containing:

- policy_tree_plot:

  A ggplot object of the policy tree plot

- policy_tree_interpretation:

  A character string interpreting the policy tree

- qini_plot:

  A ggplot object of the Qini plot

- decision_tree_visualisation:

  A ggplot object visualizing the decision tree

- policy_combo_plot:

  A ggplot object of the policy combo plot

- diff_gain_summaries:

  A nested list containing difference gain summaries for each spend
  level
