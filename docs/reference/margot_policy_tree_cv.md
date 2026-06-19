# Cross-validated held-out policy-tree diagnostics

Learns shallow policy trees on training folds and evaluates their policy
values, selected split variables, and split thresholds on held-out folds.
The target is the performance of the policy-learning procedure, not the
value of a final full-sample display tree.

## Usage

``` r
margot_policy_tree_cv(
  model_results,
  model_names = NULL,
  custom_covariates = NULL,
  exclude_covariates = NULL,
  covariate_mode = c("original", "custom", "add", "all"),
  depths = c(1L, 2L),
  num_folds = 5L,
  n_repeats = 20L,
  weights = NULL,
  min_gain_for_depth_switch = 0.01,
  max_stability_loss_for_depth_switch = 0.05,
  label_mapping = NULL,
  seed = 42L,
  tree_method = c("fastpolicytree", "policytree"),
  verbose = TRUE
)
```

## Arguments

- model_results:

  A list returned by
  [`margot_causal_forest`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md),
  [`margot_policy_tree_stability`](https://go-bayes.github.io/margot/reference/margot_policy_tree_stability.md),
  or a compatible object with `results`, `covariates`, and stored doubly
  robust action scores.

- model_names:

  Optional character vector of model names to process, with or without
  the `model_` prefix. Defaults to all models.

- custom_covariates:

  Optional character vector of covariates to use for policy trees.

- exclude_covariates:

  Optional character vector of covariate names or patterns to exclude.

- covariate_mode:

  Character. One of `"original"`, `"custom"`, `"add"`, or `"all"`.

- depths:

  Integer vector containing 1, 2, or both. Character values `"1"`, `"2"`,
  and `"both"` are also accepted.

- num_folds:

  Integer. Number of folds per repeat. Default is 5.

- n_repeats:

  Integer. Number of repeated fold partitions. Default is 20.

- weights:

  Optional numeric vector of evaluation weights. If `NULL`,
  `model_results$weights` is used when available.

- min_gain_for_depth_switch:

  Numeric. Minimum held-out value gain required before depth two can be
  selected over depth one. Default is 0.01.

- max_stability_loss_for_depth_switch:

  Numeric. Maximum allowed loss in root-split stability before depth two
  is rejected. Default is 0.05.

- label_mapping:

  Optional named list mapping outcome and variable names to display labels.

- seed:

  Integer. Base seed for reproducible fold assignments.

- tree_method:

  Character. `"fastpolicytree"` or `"policytree"`.

- verbose:

  Logical. Print progress messages.

## Value

A `margot_policy_tree_cv` list with fold-level held-out values, value
summaries, split summaries, threshold summaries, depth selection, and a
named `depth_map` that can be passed to
[`margot_policy_workflow`](https://go-bayes.github.io/margot/reference/margot_policy_workflow.md)
or
[`margot_policy_summary_compare_depths`](https://go-bayes.github.io/margot/reference/margot_policy_summary_compare_depths.md).

## Examples

``` r
if (FALSE) { # \dontrun{
cv <- margot_policy_tree_cv(
  policy_tree_result_stability,
  depths = "both",
  num_folds = 5,
  n_repeats = 20
)

cv$depth_selection
cv$depth_map
} # }
```

## References

Athey, S., & Wager, S. (2021). Policy learning with observational data.
*Econometrica*, 89(1), 133-161.
