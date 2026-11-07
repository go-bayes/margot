# Check that an object holds no fitted learner

Walks \`x\` (lists and their elements, recursively) and stops if it
reaches a fitted learner object: a SuperLearner ensemble (including
lmtp's \`lmtp_ensemble\`), a ranger or randomForest forest, an xgboost
booster, a glmnet fit, or an earth or gam fit. Lists are walked;
environments (including R6 objects) and attributes are not, so a learner
held inside an environment is not detected. Use it at each stage
boundary of an analysis to confirm that results kept in memory or saved
for reporting hold estimates and diagnostics, not fitted models.

## Usage

``` r
margot_lmtp_assert_lean(x, what = deparse1(substitute(x)))
```

## Arguments

- x:

  Object to check.

- what:

  Label used in the error message.

## Value

\`x\`, invisibly, when no fitted learner is found.

## Examples

``` r
margot_lmtp_assert_lean(list(estimate = 1, weights = c(a = 0.5, b = 0.5)))
```
