# Fit one binary probability model for a support assessment

Fit a SuperLearner ensemble on an explicitly supplied training set and
predict an explicitly supplied validation set. Use this for each
conditional exposure regression and each observation regression in the
study design.

## Usage

``` r
margot_lmtp_support_probability(
  y,
  x,
  new_x,
  ids,
  learners,
  inner_folds,
  seed,
  method = "method.NNLS"
)
```

## Arguments

- y:

  Binary training responses.

- x, new_x:

  Numeric data frames with identical columns for training and
  validation. Only variables available before the relevant event belong
  here.

- ids:

  Unique training participant identifiers, passed to SuperLearner.

- learners:

  Ordered SuperLearner library selected by the study.

- inner_folds:

  Number of inner cross-validation folds, at least two.

- seed:

  Explicit integer seed used before fitting a nonconstant response.

- method:

  SuperLearner combination method; defaults to "method.NNLS".

## Value

A list with state (fitted, constant, or unassessable), ensemble
probability, learner_probability, a compact fit record, warnings, reason
and seed. The compact record retains coefficients, cross-validated
risks, error flags and fold sizes via the same helper as
[`margot_lmtp_exposure_ratio()`](https://go-bayes.github.io/margot/reference/margot_lmtp_exposure_ratio.md).
It also retains the inner validation-row indices. Fitted learner objects
are not returned.

## Details

Empty and one-class training responses return constant predictions. An
empty conditional set is usable only when its parent probability is
zero; the calling conditional-law calculation must check this condition.
Failed learners and invalid predictions give an unassessable result.
Probabilities within 1e-12 outside \[0, 1\] are snapped to the nearest
bound; larger departures remain invalid. Warnings are recorded and
muffled, preserving the support-stage caller's behaviour. Invalid
arguments raise an error before fitting; fitting failures return an
unassessable record. The empty_training field explicitly marks an empty
conditional set. The caller owns outer folds, checkpoints and
worker/thread settings. This function sets the seed before a nonconstant
fit and uses no future plan of its own. It does not inspect terminal
outcomes or select a schedule.

## Examples

``` r
margot_lmtp_support_probability(rep(1, 4), data.frame(x = 1:4),
  data.frame(x = 5:6), letters[1:4], "SL.mean", 2L, 42L)
#> $state
#> [1] "constant"
#>
#> $probability
#> [1] 1 1
#>
#> $learner_probability
#>      SL.mean_All
#> [1,]           1
#> [2,]           1
#>
#> $fit
#> NULL
#>
#> $reason
#> [1] "one observed class in the conditional training set"
#>
#> $empty_training
#> [1] FALSE
#>
#> $warnings
#> character(0)
#>
#> $seed
#> [1] 42
#>
```
