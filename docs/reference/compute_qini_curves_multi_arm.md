# Compute Qini Curves for Multi-Arm Treatments

This function computes Qini curves for multi-arm treatment effects using
the maq package. It handles various edge cases and provides detailed
information about the computation process.

This function computes Qini curves for multi-arm causal forests using
doubly robust scores. It handles various edge cases and provides
fallback mechanisms to ensure valid outputs.

## Usage

``` r
compute_qini_curves_multi_arm(
  model,
  tau_hat,
  Y,
  W_multi,
  W.hat = NULL,
  cost = NULL,
  verbose = TRUE
)

compute_qini_curves_multi_arm(
  model,
  tau_hat,
  Y,
  W_multi,
  W.hat = NULL,
  cost = NULL,
  verbose = TRUE
)
```

## Arguments

- model:

  A fitted GRF multi-arm causal forest model.

- tau_hat:

  Matrix of estimated treatment effects (one column per treatment arm
  comparison).

- Y:

  Vector or matrix of observed outcomes.

- W_multi:

  Factor vector of multi-arm treatment assignments.

- W.hat:

  Optional vector of treatment probabilities (one for each arm).

- cost:

  Optional vector of costs (one for each arm).

- verbose:

  Logical; if TRUE, print diagnostic information during execution.

## Value

A list containing:

- qini_data:

  A data frame containing Qini curve data for plotting.

- qini_objects:

  A list of maq objects for each curve, which can be used to compute
  average gain.

The qini_data has an attribute "imputed" which is TRUE if any curves
were imputed with zeros.

A list containing:

- qini_data:

  A data frame containing Qini curve data for plotting.

- qini_objects:

  A list of maq objects for each treatment comparison.

Returns NULL if an error occurs.

## Details

The function computes Qini curves for all arms combined, a baseline (no
covariates), and each individual treatment arm. It handles cases where
some or all Qini objects have zero length or are NULL, extending curves
with zeros when necessary.
