# Plot Policy-Tree Regret and Value Contrasts

Creates a compact forest plot from
[`margot_policy_regret_summary`](https://go-bayes.github.io/margot/reference/margot_policy_regret_summary.md)
output. The default view shows regret relative to the oracle benchmark
and gains over universal treatment and universal control, with bootstrap
uncertainty intervals.

## Usage

``` r
margot_plot_policy_regret(
  x,
  metrics = c("regret_vs_oracle", "gain_vs_treat", "gain_vs_control"),
  depths = NULL,
  model_names = NULL,
  title = "Policy-tree regret",
  subtitle = "Points show value contrasts; lines show bootstrap intervals.",
  x_lab = "Estimated value contrast",
  y_lab = NULL,
  metric_labels = NULL,
  point_size = 2.4,
  interval_height = 0.22
)
```

## Arguments

- x:

  A data frame returned by
  [`margot_policy_regret_summary`](https://go-bayes.github.io/margot/reference/margot_policy_regret_summary.md)
  or
  [`policy_regret_summary`](https://go-bayes.github.io/margot/reference/margot_policy_regret_summary.md).

- metrics:

  Character vector of contrasts to plot. Supported values are
  `"regret_vs_oracle"`, `"gain_vs_treat"`, and `"gain_vs_control"`.

- depths:

  Optional integer vector of depths to include.

- model_names:

  Optional character vector of model names or bare outcome names to
  include.

- title:

  Optional plot title. Defaults to `"Policy-tree regret"`.

- subtitle:

  Optional plot subtitle. Defaults to a short note about intervals.

- x_lab:

  Optional x-axis label. Defaults to `"Estimated value contrast"`.

- y_lab:

  Optional y-axis label. Defaults to `NULL`.

- metric_labels:

  Optional named character vector for display labels.

- point_size:

  Numeric point size.

- interval_height:

  Numeric error-bar height.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
regret <- policy_regret_summary(policy_tree_results, depths = 2)
margot_plot_policy_regret(regret)
margot_plot_policy_regret(regret, metrics = "regret_vs_oracle")
} # }
```
