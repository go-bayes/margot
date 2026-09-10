# Prepare stored reporting from an independently evaluated policy rule

Binds an evaluation object's original leaf contrasts and
threshold-adjusted rule gain to explicit outcome, scale and population
metadata. This adapter performs no fitting or interval estimation.

## Usage

``` r
margot_policy_evaluation_reporting_data(
  evaluation,
  context,
  reference = NULL,
  display_weights = NULL,
  reference_label = NULL,
  display_weight_id = NULL
)
```

## Arguments

- evaluation:

  An object returned by
  [`margot_policy_tree_evaluate()`](https://go-bayes.github.io/margot/reference/margot_policy_tree_evaluate.md).

- context:

  Reporting context as in
  [`margot_policy_reporting_data()`](https://go-bayes.github.io/margot/reference/margot_policy_reporting_data.md).
  Rule and development/evaluation identities are supplied from the
  evaluation object; conflicting caller identities fail. The evaluation
  qualification is retained.

- reference:

  Optional complete prediction data frame for A/B. Its rows define the
  display population, which may differ from the evaluation population.
  Only tree columns are retained. Supply unique participant rows
  verified using participant identifiers.

- display_weights:

  Optional weights aligned with reference rows. `NULL` means equal
  display weights. A zero-weight record contributes to the unweighted
  count and has zero weight in the share calculation.

- reference_label:

  Character scalar describing the display population; required with
  `reference`.

- display_weight_id:

  Character scalar identifying the display weights; required with
  `reference`.

## Value

A `margot_policy_reporting_data` object compatible with stored tables,
text and plots. Leaf estimates remain original treatment-control
effects; the value estimate uses the saved threshold-adjusted objective.
Full-sample refits require their own reporting object and cannot inherit
these intervals.
