# Plot a compact Margot policy-tree display

Draws the descriptive full-sample policy tree stored by
[`margot_policy_tree_display()`](https://go-bayes.github.io/margot/reference/margot_policy_tree_display.md).
The stored held-out selected depth is used automatically. The plot shows
the fitted assignment rule and supplies no additional policy-value
estimate.

## Usage

``` r
# S3 method for class 'margot_policy_tree_display'
plot(x, model_name = NULL, ...)
```

## Arguments

- x:

  A `margot_policy_tree_display` object.

- model_name:

  Optional model name, with or without the `model_` prefix. Required
  when `x` contains more than one model.

- ...:

  Arguments passed to
  [`margot_plot_policy_decision_tree()`](https://go-bayes.github.io/margot/reference/margot_plot_policy_decision_tree.md).

## Value

A ggplot object.
