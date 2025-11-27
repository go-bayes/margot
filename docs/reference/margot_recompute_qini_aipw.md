# Recompute QINI Curves Using AIPW Scores

Recomputes QINI curves for binary treatment causal forest models using
Augmented Inverse Probability Weighted (AIPW) scores instead of IPW
scores. This provides more robust estimates, especially for
observational data where treatment assignment may be confounded.

## Usage

``` r
margot_recompute_qini_aipw(
  margot_result,
  model_name = NULL,
  method = c("regression_forest", "grf_scores"),
  n.trees = 2000,
  treatment_var = NULL,
  verbose = TRUE
)
```

## Arguments

- margot_result:

  A list returned by
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)
  with `save_models = TRUE` and `save_data = TRUE`.

- model_name:

  Character string specifying which model to recompute. If NULL
  (default), all models will be recomputed.

- method:

  Character string specifying the method to estimate mu.hat. Options are
  "regression_forest" (default) or "grf_scores".

- n.trees:

  Number of trees to use for regression forests when method =
  "regression_forest". Default is 2000.

- treatment_var:

  Character string specifying the treatment variable name. If NULL, the
  function will try to detect it automatically.

- verbose:

  Logical. If TRUE, prints progress messages. Default is TRUE.

## Value

A modified version of the input margot_result with updated QINI curves
based on AIPW scores. The structure remains compatible with all existing
plotting and interpretation functions.

## Details

AIPW scores provide double robustness: the estimator is consistent if
either the propensity score model OR the outcome model is correctly
specified. This is particularly valuable in observational studies.

The function supports two methods for estimating conditional means
(mu.hat):

- **regression_forest**: Trains separate regression forests on control
  and treated units to estimate E\[Y\|W=0,X\] and E\[Y\|W=1,X\].

- **grf_scores**: Uses the doubly robust scores from grf's internal
  computations (if available).

## Examples

``` r
if (FALSE) { # \dontrun{
# Recompute QINI curves with AIPW for all models
results_aipw <- margot_recompute_qini_aipw(margot_results)

# Recompute for a specific model
results_aipw <- margot_recompute_qini_aipw(
  margot_results,
  model_name = "model_anxiety"
)

# Use with plotting functions as normal
margot_plot_qini(results_aipw, "model_anxiety")

# Specify treatment variable if auto-detection fails
results_aipw <- margot_recompute_qini_aipw(
  margot_results,
  treatment_var = "t1_treatment"
)
} # }
```
