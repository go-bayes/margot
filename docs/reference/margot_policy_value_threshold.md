# Resolve a benefit threshold from development action scores

Resolves a fixed signed threshold or the weighted development-sample
average treatment effect reference. The result is a hypothetical
outcome-scale benefit threshold, not a measured economic cost.

## Usage

``` r
margot_policy_value_threshold(
  dr_scores,
  weights = NULL,
  value_threshold = 0,
  threshold_multiplier = 1,
  treatment_column = NULL,
  control_column = NULL
)
```

## Arguments

- dr_scores:

  Numeric matrix of finite, original (unweighted) binary action scores,
  on a common outcome scale where larger scores are preferred.
  Recognisable control and treatment column names are required unless
  both column indices are supplied.

- weights:

  Optional finite non-negative development weights, aligned with rows
  and with positive total weight. Weights define the averaging
  population and are applied once.

- value_threshold:

  A finite numeric scalar (default zero, preserving the original
  objective), or `"ate"` to use the weighted mean
  treatment-minus-control action-score contrast in these development
  rows.

- threshold_multiplier:

  Finite non-negative scalar multiplying the fixed threshold or
  development ATE; default one. Multipliers are relative references, not
  guaranteed extreme ranges.

- treatment_column, control_column:

  Optional distinct integer column indices identifying the two actions.
  Supply both together.

## Value

A `margot_policy_value_threshold` list with resolved `value`, `source`,
`multiplier`, `development_ate`, development row and weight counts, and
action-column indices. Preserve this object unchanged when evaluating
the associated learned rule on separate observations.

## Details

Subtract the resolved value from the original treated action score
before applying the analysis weight. Apply this same objective to every
comparator. Original treatment effects retain their signs: an effect
below an ATE reference need not be harmful. A zero or negative ATE is a
relative reference and cannot automatically represent a positive
treatment expense. This helper does not establish causal identification
or independence of the supplied scores; callers must keep nuisance
fitting, threshold estimation and rule learning within their declared
development boundary.
