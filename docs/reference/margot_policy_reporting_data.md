# Bind stored policy estimates to a rule and reporting context

Validates supplied leaf contrasts and a rule-minus-comparator value
difference. Computes only descriptive reference counts and weighted
shares when reference rows are supplied. Metadata records
caller-supplied provenance. Validation checks internal compatibility.
Scientific identification and interval coverage require independent
justification.

## Usage

``` r
margot_policy_reporting_data(
  tree,
  leaves,
  value,
  context,
  value_context = context,
  reference = NULL,
  display_weights = NULL,
  reference_label = NULL,
  display_weight_id = NULL
)
```

## Arguments

- tree:

  The stored policy tree whose terminal node identifiers occur in
  `leaves`.

- leaves:

  Data frame with `node_id`, unique `leaf_label`, `estimate`, `lower`,
  `upper`, `interval_type`, `interval_level`, `interval_method` and
  `unavailable_reason`. Estimates are already on the declared scale and
  orientation. Every terminal node must appear exactly once. Use numeric
  `NA` for unavailable endpoints.

- value:

  One-row data frame with the same estimate and interval fields, plus
  `comparator_id`, `comparator_label` and finite non-negative
  `gain_margin`. Supply the resolved analysis margin.

- context:

  Named list of character scalars: `outcome`, `outcome_label`,
  `rule_id`, `population_id`, `population_label`, `scale_id`,
  `scale_label`, `orientation` (`as_scored` or `reversed`), `weight_id`,
  `evaluation_mode`, `contrast_label` and `qualification`. Evaluation
  modes are `independent_fixed_rule`, `selected_full_sample`,
  `repeated_learning` and `constructed`. Independent evaluation
  additionally requires distinct `development_id` and `evaluation_id`.
  The qualification states the inferential limitations, including any
  multiplicity adjustment.

- value_context:

  Context for D; defaults to `context`. A separate rule identity is
  allowed only for explicitly labelled `selected_full_sample` leaves
  with `repeated_learning` value. Outcome, population, scale,
  orientation and weight identities must agree.

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

A `margot_policy_reporting_data` list containing unrounded tables,
contexts, rule signature and optional reference rows and weights. New
plotting and text functions revalidate this object before use.

## Details

Available interval types are `pointwise` or `simultaneous` for
independent rules and repeated procedures, `nominal_fixed_leaves` for
selected full-sample results, and `constructed` for illustrative
fixtures. A nominal fixed-leaf interval ignores selection. An
unavailable interval requires type `unavailable` and a reason.
Repeated-fold quantiles describe partition variability. Sampling
intervals require a method that accounts for participant reuse. The
supplied method and qualification appear in plots and text. Reversal
metadata labels an already reversed estimate. The supplied numbers are
preserved.
