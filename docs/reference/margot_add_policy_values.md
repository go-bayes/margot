# Add bootstrap policy-value tests for multiple depths

Add bootstrap policy-value tests for multiple depths

## Usage

``` r
margot_add_policy_values(model, depths = c(1L, 2L), R = 499L, seed = 42L)
```

## Arguments

- model:

  list. One element of \`margot\$results\`.

- depths:

  integer vector. Depths to evaluate (default \`c(1, 2)\`).

- R, seed:

  Passed to \[margot_compute_policy_value()\].

## Value

The modified \`model\` (invisibly).
