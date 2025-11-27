# RATE on “resistance” (–τ̂), robust to old/new GRF APIs

RATE on “resistance” (–τ̂), robust to old/new GRF APIs

## Usage

``` r
margot_rate_resistance(model_results, round_digits = 3, ...)
```

## Arguments

- model_results:

  list from \`margot_causal_forest()\`, \*not\* \`\$results\`.

- round_digits:

  integer; decimals to keep.

- ...:

  extra args for \`grf::rank_average_treatment_effect()\`.

## Value

tibble(model, outcome, estimate, variance, 2.5
