# Repeated Split Diagnostic for Policy Trees

Repeatedly fits policy trees on a training split and evaluates their
action summaries on the complementary split. This diagnostic describes
instability in the policy-learning procedure; it is not the displayed
full-data tree.

## Usage

``` r
margot_policy_split_diagnostic(
  object,
  model_names = NULL,
  depths = c(1L, 2L),
  n_splits = 50L,
  train_proportion = 0.5,
  seed = 12345,
  covariate_mode = c("original", "custom", "add", "all"),
  custom_covariates = NULL,
  exclude_covariates = NULL,
  tree_method = c("fastpolicytree", "policytree"),
  weights = NULL,
  verbose = TRUE
)
```

## Arguments

- object:

  A list returned by
  [`margot_causal_forest`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)
  with saved `covariates`, `not_missing`, and per-model `dr_scores`.

- model_names:

  Optional model names, with or without the `model_` prefix. Defaults to
  all models.

- depths:

  Integer vector containing 1, 2, or both. Defaults to `c(1L, 2L)`.

- n_splits:

  Integer. Number of repeated train/test splits. Defaults to 50.

- train_proportion:

  Numeric in (0,1). Fraction assigned to the training split. Defaults to
  0.5.

- seed:

  Integer or `NULL`. Seed for reproducibility. Defaults to 12345.

- covariate_mode:

  Character; one of "original", "custom", "add", or "all". Defaults to
  "original".

- custom_covariates:

  Optional character vector used with "custom" or "add".

- exclude_covariates:

  Optional character vector of exact names or patterns to exclude from
  the policy-tree covariates.

- tree_method:

  Character; "fastpolicytree" when available, otherwise "policytree".

- weights:

  Optional weights for held-out summaries. Defaults to `object$weights`.

- verbose:

  Logical; print progress messages. Defaults to TRUE.

## Value

A tibble with one row per model, split, and depth. The returned object
has class `margot_policy_split_diagnostic` and a `summary` attribute
with mean and standard deviation of key metrics.
