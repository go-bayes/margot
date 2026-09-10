# Describe stored contrasts within policy-tree leaves

Describe stored contrasts within policy-tree leaves

## Usage

``` r
margot_text_policy_leaf_effects(data, digits = 3L)
```

## Arguments

- data:

  A validated object from
  [`margot_policy_reporting_data()`](https://go-bayes.github.io/margot/reference/margot_policy_reporting_data.md).

- digits:

  Decimal places for displayed numbers; raw tables retain full
  precision.

## Value

A character vector: scope, one statement per leaf, and the supplied
inferential qualification. A difference between leaf effects requires a
direct contrast and its uncertainty.
