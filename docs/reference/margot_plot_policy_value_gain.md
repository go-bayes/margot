# Plot a stored policy-value gain and practical margin

Plot a stored policy-value gain and practical margin

## Usage

``` r
margot_plot_policy_value_gain(
  data,
  digits = 3L,
  title = "Gain over constant assignment"
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

A ggplot containing the stored value table. The dashed line marks the
resolved practical gain margin. Intervention costs and confidence bounds
are distinct quantities.
