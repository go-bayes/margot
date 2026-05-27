# Summarise Policy-Tree Value and Oracle Regret

Computes policy-tree value contrasts from stored doubly robust action
scores. The summary compares a learned policy tree with universal
control, universal treatment, and an oracle that chooses the better
action score for each observation. The oracle is an optimistic benchmark
for describing remaining model-implied heterogeneity; it is not a
deployable policy.

## Usage

``` r
margot_policy_regret_summary(
  object,
  model_names = NULL,
  depths = c(1L, 2L),
  weights = NULL,
  R = 499L,
  seed = 42L,
  ci_level = 0.95,
  label_mapping = NULL
)

policy_regret_summary(
  object,
  model_names = NULL,
  depths = c(1L, 2L),
  weights = NULL,
  R = 499L,
  seed = 42L,
  ci_level = 0.95,
  label_mapping = NULL
)
```

## Arguments

- object:

  A list returned by
  [`margot_causal_forest`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md),
  [`margot_policy_tree`](https://go-bayes.github.io/margot/reference/margot_policy_tree.md),
  or a compatible policy-tree object with `results` and either top-level
  `covariates` or per-model `plot_data`.

- model_names:

  Optional character vector of model names to include, with or without
  the `model_` prefix. Defaults to all available models.

- depths:

  Integer vector of tree depths to evaluate. Defaults to `c(1, 2)`.

- weights:

  Optional numeric vector of evaluation weights. If `NULL`, the function
  uses `object$weights` when available.

- R:

  Integer. Bootstrap replicates for uncertainty intervals. Defaults to
  499.

- seed:

  Integer or `NULL`. Seed for bootstrap reproducibility.

- ci_level:

  Numeric confidence level for bootstrap percentile intervals. Defaults
  to 0.95.

- label_mapping:

  Optional named character vector for display labels.

## Value

A tibble with one row per model and depth, including policy value,
universal-action values, oracle value, gains relative to control-all and
treat-all, and regret relative to the oracle benchmark.

## Examples

``` r
if (FALSE) { # \dontrun{
policy_regret_summary(causal_forest_results, depths = 2)
margot_policy_regret_summary(policy_tree_results, model_names = "model_y")
} # }
```
