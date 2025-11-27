# Compute Qini Curves for Multi-Arm and Binary Treatments (Deprecated)

DEPRECATED: This function is no longer functional and will be removed in
a future version. Please use \`compute_qini_curves_binary()\` for binary
treatments and \`compute_qini_curves_multi_arm()\` for multi-arm
treatments instead.

## Usage

``` r
compute_qini_curves(tau_hat, Y, W = NULL, W_multi = NULL)
```

## Arguments

- tau_hat:

  Matrix or vector of estimated treatment effects.

- Y:

  Vector of observed outcomes.

- W:

  Vector of treatment assignments for binary treatment.

- W_multi:

  Factor of treatment assignments for multi-arm treatment.

## Value

A data frame containing Qini curve data for plotting.

## See also

[`compute_qini_curves_binary`](https://go-bayes.github.io/margot/reference/compute_qini_curves_binary.md)
for binary treatments `compute_qini_curves_multi_arm` for multi-arm
treatments
