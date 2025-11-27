# Create Fallback Qini Data for Multi-Arm Treatments

Creates a basic data frame to use when Qini calculations fail.

## Usage

``` r
create_fallback_qini_data(tau_hat, W_multi)
```

## Arguments

- tau_hat:

  Matrix of treatment effect estimates.

- W_multi:

  Factor vector of treatment assignments.

## Value

A data frame with proportion, gain, and curve columns.
