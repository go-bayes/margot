# Create Fallback Qini Objects for Multi-Arm Treatments

Creates basic Qini objects to use when calculations fail.

## Usage

``` r
create_fallback_qini_objects(tau_hat, W_multi)
```

## Arguments

- tau_hat:

  Matrix of treatment effect estimates.

- W_multi:

  Factor vector of treatment assignments.

## Value

A list of Qini objects with minimal structure.
