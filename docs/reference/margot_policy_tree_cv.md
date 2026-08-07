# Cross-validated held-out policy-tree diagnostics

Learns shallow policy trees on training folds and evaluates their policy
values, selected split variables, split thresholds, and leaf-level
signed treatment-control contrasts on held-out folds. The target is the
performance of the policy-learning procedure, not the value of a final
full-sample display tree.

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
  min_root_stability_for_depth_switch = 0.5,
  label_mapping = NULL,
  seed = 42L,
  tree_method = c("fastpolicytree", "policytree"),
  verbose = TRUE
)
```

## Arguments

- model_results:

  A list returned by
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md),
  [`margot_policy_tree_stability()`](https://go-bayes.github.io/margot/reference/margot_policy_tree_stability.md),
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

  Character. One of `"original"`, `"custom"`, `"add"`, or `"all"`. **The
  default `"original"` does not search the full covariate set.** It
  restricts every policy-tree split to the variables already stored in
  `model_result$top_vars`, which is the top-15 variable-importance
  screen written by
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md).
  Trees therefore cannot split on a modifier that fell outside that
  screen, and the reported root-split stability describes agreement
  within the screened set rather than within all covariates. Use `"all"`
  to search every covariate, `"custom"` to supply your own set through
  `custom_covariates`, or `"add"` to append to the screen. The wider
  search is materially more expensive: at \\n = 23{,}000\\ with 57
  covariates, `covariate_mode = "all"` costs roughly 167 seconds per
  depth-two fit, which is about 4.6 hours per outcome under the
  registered cross-validation of 5 folds and 20 repeats, whereas the
  top-15 screen is roughly 14 times cheaper at depth two.

- depths:

  Integer vector containing 1, 2, or both. Character values `"1"`,
  `"2"`, and `"both"` are also accepted.

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
  is rejected. Default is 0.05. The loss is the depth-one root-split
  selection frequency minus the depth-two root-split selection
  frequency, so the guard is relative. When either frequency is
  unavailable the loss is `NA`; the guard then fails closed, depth two
  is refused, and a warning records that stability could not be
  computed. Before version 1.1.015 an `NA` loss passed the guard.

- min_root_stability_for_depth_switch:

  Numeric in \\\[0, 1\]\\. Absolute floor on the *depth-one* root-split
  selection frequency before depth two becomes eligible. Default is 0.5,
  that is, majority agreement on the first split. An unstable depth-one
  root means the procedure has found no reliable first split, so a
  depth-two "improvement" over it cannot be structure; the relative
  stability guard above passes trivially in exactly that case, because a
  low depth-one frequency makes the loss small or negative whatever
  depth two does. Requiring majority root agreement before depth two is
  eligible closes that loophole. Set to 0 to disable the floor and
  recover the pre-1.1.015 relative-only rule, which is the opt-out for
  studies registered under the old rule. Disabling the floor does not
  restore the pre-1.1.015 handling of an `NA` stability loss, which now
  always fails closed.

- label_mapping:

  Optional named list mapping outcome and variable names to display
  labels.

- seed:

  Integer. Base seed for reproducible fold assignments.

- tree_method:

  Character. `"fastpolicytree"` or `"policytree"`.

- verbose:

  Logical. Print progress messages.

## Value

A `margot_policy_tree_cv` list with fold-level held-out values, value
summaries, split summaries, leaf summaries, threshold summaries, depth
selection, and a named `depth_map` that can be passed to
[`margot_policy_workflow()`](https://go-bayes.github.io/margot/reference/margot_policy_workflow.md)
or
[`margot_policy_summary_compare_depths()`](https://go-bayes.github.io/margot/reference/margot_policy_summary_compare_depths.md).

## Details

Let \\\Gamma\_{ja}\\ denote the action score for observation \\j\\ under
action \\a\\. Policy-tree evaluation averages the score for the action
selected by the learned policy \\\pi\\. For binary actions \\C\\ and
\\T\\, held-out summaries report value against all-control,
all-treatment, and best-constant baselines. Leaf summaries report the
signed held-out evaluation contrast \\\Gamma\_{jT} - \Gamma\_{jC}\\ for
observations routed by trees learned on training folds. Selected actions
are the actions stored by those learned trees; held-out summaries do not
reselect actions from held-out means. Between-leaf differences describe
variation in score-contrast magnitude, not the policy decision rule
itself.

Depth one is the default selection. Depth two is selected only when
three guards pass together: the held-out value gain over depth one
exceeds `min_gain_for_depth_switch`; the root-split stability lost in
moving to depth two is no larger than
`max_stability_loss_for_depth_switch`; and the depth-one root-split
selection frequency is at least `min_root_stability_for_depth_switch`.
The third guard is absolute rather than relative, and it was added in
version 1.1.015 after a simulation under a constant-effect null selected
depth two in 1 of 10 replicates by clearing the two relative guards on
noise alone. When the stability comparison cannot be computed the guards
fail closed and depth one is kept.

## References

Athey, S., & Wager, S. (2021). Policy learning with observational data.
Econometrica, 89(1), 133-161.
