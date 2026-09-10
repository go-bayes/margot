# Plot stored contrasts within policy-tree leaves

Plot stored contrasts within policy-tree leaves

## Usage

``` r
margot_plot_policy_leaf_effects(
  data,
  digits = 3L,
  title = "Contrasts within policy leaves"
)
```

## Arguments

- data:

  A validated object from
  [`margot_policy_reporting_data()`](https://go-bayes.github.io/margot/reference/margot_policy_reporting_data.md).

- digits:

  Decimal places for displayed numbers; raw tables retain full
  precision.

- title:

  Optional plot title.

## Value

A ggplot. Its `data` contains the unrounded stored table and formatted
labels.
