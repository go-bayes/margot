# Run Simulations for Estimating ATE

This function simulates data and estimates the Average Treatment Effect
(ATE) using different methods under a specified causal model.

## Usage

``` r
run_simulations(
  num_simulations,
  N,
  prob_L1,
  A_on_Y,
  L_on_A,
  L_on_Y,
  method = "ps"
)
```

## Arguments

- num_simulations:

  Integer, number of simulations to run.

- N:

  Integer, sample size for each simulation.

- prob_L1:

  Double, probability parameter for generating L1.

- A_on_Y:

  Double, effect size of A on Y.

- L_on_A:

  Double, effect size of L on A.

- L_on_Y:

  Double, effect size of L on Y.

- method:

  Character, method for IPTW and DR adjustment ("ps" or other).

## Value

A tibble containing the estimated ATEs and their confidence intervals
for each method.
