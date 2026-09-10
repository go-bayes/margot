# Learn a benefit-threshold rule and evaluate it on separate supplied scores

Learns a binary policy tree and constant comparator using development
data only, then evaluates those fixed rules on a separate evaluation
sample. This score-level interface performs no nuisance fitting,
preprocessing, sample splitting or causal identification.

## Usage

``` r
margot_policy_tree_evaluate(
  development_X,
  development_scores,
  evaluation_X,
  evaluation_scores,
  development_weights = NULL,
  evaluation_weights = NULL,
  value_threshold = "ate",
  threshold_multiplier = 1,
  depth = 1L,
  min_node_size = 1L,
  tree_method = "policytree",
  development_ids = NULL,
  evaluation_ids = NULL,
  treatment_column = NULL,
  control_column = NULL,
  gain_margin = 0.01
)
```

## Arguments

- development_X, evaluation_X:

  Complete finite numeric feature matrices or data frames with identical
  named columns in the same order. Features must have been prepared
  within the declared development boundary.

- development_scores, evaluation_scores:

  Unweighted finite binary action-score matrices or data frames, with
  identical named columns in the same order. Rows must align with the
  corresponding features and weights. Scores must already share outcome
  units and orientation, with larger values preferred. Evaluation
  outcomes must not have contributed to development scores, feature
  preparation, threshold estimation or rule selection.

- development_weights, evaluation_weights:

  Optional finite non-negative analysis weights with positive finite
  sums. Applied exactly once. Defaults to equal weights. Weight
  estimation and population projection remain the caller's
  responsibility.

- value_threshold:

  Benefit threshold, either `"ate"` (the default for this new interface)
  or a finite numeric value. The development weighted mean
  treatment-minus-control action score defines the ATE reference. Its
  resolved value is carried unchanged into evaluation.

- threshold_multiplier:

  Finite multiplier for an ATE reference; see
  [`margot_policy_value_threshold()`](https://go-bayes.github.io/margot/reference/margot_policy_value_threshold.md).

- depth:

  Prespecified maximum depth, one or two. No depth selection uses
  evaluation data.

- min_node_size:

  Positive integer minimum development leaf count, default one. This is
  a computational constraint, not a guarantee of inferential support.

- tree_method:

  Requested engine, `"policytree"` (default) or `"fastpolicytree"`. The
  requested package must be installed. There is no engine fallback; the
  fast engine uses `strategy.datatype = 1`.

- development_ids, evaluation_ids:

  Optional unique, non-missing participant identifiers aligned with
  rows. Supply both or neither. Supplied identifiers must be disjoint.
  Without identifiers, participant separation cannot be verified; matrix
  equality is not an identity test.

- treatment_column, control_column:

  Optional action-column identities passed to
  [`margot_policy_value_threshold()`](https://go-bayes.github.io/margot/reference/margot_policy_value_threshold.md).

- gain_margin:

  Non-negative population-level gain margin saved for reporting, default
  0.01 in the supplied outcome units. It does not change the treatment
  threshold, select the rule or define statistical significance.

## Value

A `margot_policy_tree_evaluation` list with the native fixed `tree`, its
`rule_id`, resolved `threshold`, development-selected `constant`,
partition identities and inference limitations. `development` stores
descriptive leaves and values. `evaluation` stores original and net leaf
contrasts, actions, weights, gross/cost/net values, all paired
tree-minus-comparator intervals and the primary `value` row. Original
scores are retained separately from net scores, and paired influence
contributions are saved. The object is compatible with adapters to
[`margot_policy_reporting_data()`](https://go-bayes.github.io/margot/reference/margot_policy_reporting_data.md).

## Details

The treatment reward is reduced by the development threshold before
weighting. A tree is retained only when its development net value
exceeds the development constant value beyond floating-point tolerance.
Otherwise a constant rule is saved. Universal-action ties select control
deterministically. This is training-objective simplification, not
evidence of out-of-sample superiority. All prespecified evaluation
comparisons are returned without choosing a winner using evaluation
outcomes.

For a paired score difference `D`, normalised weights `p`, and `n`
independent evaluation records, the standard error is
`sqrt(n/(n-1) * sum((p * (D - sum(p*D)))^2))`. Nominal pointwise 95%
normal intervals condition on the learned rule, realised threshold,
supplied nuisance scores and preparation. They do not account for
training uncertainty, nuisance estimation bias, imputation, estimated
weights, clustering, investigator outcome access or multiplicity.
Independent evaluation rows and suitable nuisance-score conditions
require separate justification. No claim of unconditional causal
coverage is made. Leaf intervals refer to the unchanged development
partition; a direct between-leaf comparison requires its own inference.
