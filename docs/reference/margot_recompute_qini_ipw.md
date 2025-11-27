# Recompute QINI Curves Using IPW Scores

Recomputes QINI curves for binary treatment causal forest models using
Inverse Probability Weighted (IPW) scores. This is a simpler alternative
to
[`margot_recompute_qini_aipw()`](https://go-bayes.github.io/margot/reference/margot_recompute_qini_aipw.md)
that uses IPW scores only.

## Usage

``` r
margot_recompute_qini_ipw(
  margot_result,
  model_name = NULL,
  treatment_var = NULL,
  W.hat = NULL,
  verbose = TRUE
)
```

## Arguments

- margot_result:

  A list returned by
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md).

- model_name:

  Character string specifying which model to recompute. If NULL
  (default), all models will be recomputed.

- treatment_var:

  Character string specifying the treatment variable name. If NULL, the
  function will try to detect it automatically.

- W.hat:

  Numeric vector of propensity scores. If NULL, will use propensity
  scores from the model or estimate them.

- verbose:

  Logical. If TRUE, prints progress messages. Default is TRUE.

## Value

A modified version of the input margot_result with updated QINI curves
based on IPW scores. The structure remains compatible with all existing
plotting and interpretation functions.

## Details

This function provides a way to recompute QINI curves using the modern
maq API with IPW scores. It's useful for:

- Debugging QINI curve differences

- Faster computation compared to AIPW

- Ensuring consistency across different model computations

The function handles various data storage patterns including models
stored in the results list or in a separate full_models list.

## Examples

``` r
if (FALSE) { # \dontrun{
# Recompute QINI curves with IPW for all models
results_ipw <- margot_recompute_qini_ipw(margot_results)

# Recompute for a specific model
results_ipw <- margot_recompute_qini_ipw(
  margot_results,
  model_name = "model_anxiety"
)

# Specify treatment variable if auto-detection fails
results_ipw <- margot_recompute_qini_ipw(
  margot_results,
  treatment_var = "t1_treatment"
)

# Compare with AIPW results
results_aipw <- margot_recompute_qini_aipw(margot_results)
# Plot both for comparison
} # }
```
