# Plot a policy-tree branching decision tree

Thin, explicit wrapper around
[`margot_plot_decision_tree()`](https://go-bayes.github.io/margot/reference/margot_plot_decision_tree.md).
Use this helper when the intended artefact is the branching assignment
rule.

## Usage

``` r
margot_plot_policy_decision_tree(result_object, model_name, ...)
```

## Arguments

- result_object:

  A list returned by
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)
  or a compatible policy-tree workflow object.

- model_name:

  Character scalar naming the model to plot, with or without the
  `model_` prefix.

- ...:

  Arguments passed to
  [`margot_plot_decision_tree()`](https://go-bayes.github.io/margot/reference/margot_plot_decision_tree.md).

## Value

A ggplot object.
