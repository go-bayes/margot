# Cross-fitted exposure-only density ratios

Estimate the ratio of the shifted exposure law to the natural exposure
law by classifying equally weighted copies of each observed record. Each
copy has the same history; its exposure is either observed or
policy-shifted. Observation indicators and outcomes must be excluded
from \`history\`. Both copies remain together in outer and inner
validation, including the penalty-selection folds used by the default
lasso learner.

## Usage

``` r
margot_lmtp_exposure_ratio(
  data,
  exposure,
  history,
  shifted,
  id,
  fold_id,
  reached,
  learners = c("SL.mean", "SL.glmnet", "SL.ranger", "SL.xgboost"),
  inner_folds = 10L,
  structural_zero = rep(FALSE, nrow(data)),
  retain_fits = TRUE
)
```

## Arguments

- data:

  Data frame containing the exposure and pre-exposure history.

- exposure:

  Name of one numeric exposure column.

- history:

  Unique names of pre-exposure predictors, excluding exposure.

- shifted:

  Numeric vector of policy-shifted exposures in input row order.

- id:

  Unique, nonmissing participant identifiers in input row order.

- fold_id:

  Integer outer-fold assignments, numbered consecutively from one.

- reached:

  Logical vector identifying records observed at this assignment.

- learners:

  SuperLearner library, in order.

- inner_folds:

  Number of participant-grouped inner validation folds. The default
  lasso learner retains ten separate penalty-selection folds.

- structural_zero:

  Logical vector identifying observed exposure values outside the policy
  image. Defaults to no known structural zeros.

- retain_fits:

  Logical. When \`FALSE\`, each outer fold's fitted SuperLearner is
  replaced, inside its worker, by a small record of learner names,
  ensemble coefficients, cross-validated risks, error flags and inner
  folds, so fitted learners never reach the calling process. Ratios,
  probabilities, validation membership and warnings are unchanged.
  Default \`TRUE\` keeps the earlier behaviour.

## Value

A list containing uncapped \`ratio\`, classifier \`probability\`,
complete outer-fold \`fits\`, input row identities and the requested
learner settings. Unreached records have missing ratios and
probabilities. Fits retain models, validation membership, warnings and
worker process identifiers.

## Details

No probability floor, ratio cap or observation probability is applied. A
probability of one stops estimation unless a known structural zero is
supplied for that evaluation record. The caller must justify structural
zeros from the policy's image, rather than from fitted probabilities.

Configure the future plan, one thread per learner and the RNG before
calling. The function uses \`future.seed = TRUE\`; it does not change
the plan. Each outer fold is one future. It does not select an
intervention schedule, estimate an outcome or establish adequate
support.

## References

Diaz et al. (2023), Nonparametric causal effects based on longitudinal
modified treatment policies, Section 5.4.
