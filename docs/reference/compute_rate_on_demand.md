# Compute RATE (Rank Average Treatment Effect) On-Demand

This internal helper function computes RATE metrics (AUTOC or QINI)
on-demand from a causal forest and treatment effect estimates. It
ensures proper out-of-sample validation when possible.

## Usage

``` r
compute_rate_on_demand(
  forest,
  tau_hat = NULL,
  target = c("AUTOC", "QINI"),
  q = seq(0.1, 1, by = 0.1),
  policy = c("treat_best", "withhold_best"),
  subset = NULL,
  use_oob_predictions = TRUE,
  verbose = FALSE,
  seed = 12345,
  ...
)
```

## Arguments

- forest:

  A causal_forest object from grf

- tau_hat:

  Optional vector of treatment effect estimates. If NULL, will be
  computed from the forest using out-of-bag predictions by default.

- target:

  Character; either "AUTOC" or "QINI"

- q:

  Numeric vector of quantiles at which to evaluate the TOC. Default is
  seq(0.1, 1, by = 0.1) which matches the GRF default.

- policy:

  Character; either "treat_best" (default) or "withhold_best"

- subset:

  Optional indices for subsetting the evaluation data

- use_oob_predictions:

  Logical; if TRUE and tau_hat is NULL, use out-of-bag predictions for
  better validity (default TRUE)

- verbose:

  Logical; print informative messages (default FALSE)

- seed:

  Random seed for reproducibility (default 12345)

- ...:

  Additional arguments passed to grf::rank_average_treatment_effect()

## Value

A rank_average_treatment_effect object from grf

## Details

For valid statistical performance, the prioritization scores (tau_hat)
should be constructed independently from the evaluation forest training
data. This function attempts to ensure this by: - Using out-of-bag
predictions when tau_hat is not provided - Supporting subset indices for
proper train/test splitting - Warning when validation might be
compromised
