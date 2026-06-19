# Summarise policy-tree leaves with estimated action gains and sample shares

Computes leaf-level summaries for a stored policy tree. Leaf gains are
estimated from doubly robust action scores and are action conditional:
treated leaves report the estimated gain of treatment relative to
control, whereas control leaves report the estimated gain of control
relative to treatment.

## Usage

``` r
margot_policy_leaf_summary(
  object,
  model_name,
  depth = 1L,
  weights = NULL,
  digits = 3L,
  label_mapping = NULL
)
```

## Arguments

- object:

  A
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)-style
  object containing `results`, `covariates`, and optionally `weights`.

- model_name:

  Outcome/model name, with or without the `model_` prefix.

- depth:

  Integer policy-tree depth, usually `1` or `2`.

- weights:

  Optional evaluation weights. Defaults to `object$weights`.

- digits:

  Integer; rounding used in formatted labels.

- label_mapping:

  Optional named list used to label actions.

## Value

A tibble with one row per leaf and columns for node id, action,
unweighted count, weighted sample share, action-conditional estimated
gain, and policy-value contributions.
