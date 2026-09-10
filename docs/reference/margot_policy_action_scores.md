# Construct binary action scores from independent nuisance predictions

Computes unweighted doubly robust control and treatment scores. The
caller supplies predictions estimated without the scored observation's
outcome; evaluation predictions must respect the policy-development
boundary.

## Usage

``` r
margot_policy_action_scores(
  outcome,
  treatment,
  outcome_mean,
  propensity,
  treatment_effect
)
```

## Arguments

- outcome:

  Numeric outcome vector, already on the intended oriented scale.

- treatment:

  Binary numeric vector, zero for control and one for treatment.

- outcome_mean:

  Predicted conditional outcome mean under the observed exposure
  distribution.

- propensity:

  Predicted treatment probabilities strictly between zero and one. No
  truncation or clipping is performed.

- treatment_effect:

  Predicted conditional treatment-minus-control effect.

## Value

A two-column numeric matrix named \`control\` and \`treated\` on the
supplied outcome scale.

## Details

Conditional action means are recovered as \`outcome_mean - propensity \*
treatment_effect\` for control and \`outcome_mean + (1 - propensity) \*
treatment_effect\` for treatment. The observed action's residual is
corrected by its inverse propensity. Correct interpretation requires the
causal identification and nuisance-estimation conditions for the
supplied design. This function verifies numerical inputs, not
independence or those conditions. It applies neither analysis weights
nor a benefit threshold. Apply each once in the subsequent learning and
evaluation procedure.
