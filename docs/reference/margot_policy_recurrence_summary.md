# Summarise outcome-wide recurrence in held-out policy-tree splits

Builds a descriptive outcome-wide recurrence table from
[`margot_policy_tree_cv()`](https://go-bayes.github.io/margot/reference/margot_policy_tree_cv.md).
The summary counts how often baseline variables recur as root or
any-node splits across outcomes. It is a reporting diagnostic, not a
formal multiplicity-adjusted test.

## Usage

``` r
margot_policy_recurrence_summary(
  policy_cv,
  selected_depth_only = TRUE,
  gain_weighted = TRUE
)
```

## Arguments

- policy_cv:

  A `margot_policy_tree_cv` object.

- selected_depth_only:

  Logical; if `TRUE`, use the selected depth for each outcome when
  `policy_cv$depth_map` is available.

- gain_weighted:

  Logical; if `TRUE`, include a positive-held-out-gain weighted
  root-frequency column.

## Value

A tibble with one row per variable.
