# Batch-compute RATEs for each outcome in a margot_causal_forest result

This replaces the legacy internal RATE code and is the only function
that actually talks to grf. It flips the CATE vector on the fly when
policy is "withhold_best".

## Usage

``` r
margot_rate_batch(
  model_results,
  policy = c("treat_best", "withhold_best"),
  target = c("AUTOC", "QINI"),
  level = 0.95,
  round_digits = 3,
  model_prefix = "model_",
  verbose = TRUE,
  seed = 12345,
  q = seq(0.1, 1, by = 0.1),
  use_evaluation_subset = TRUE,
  ...
)
```

## Arguments

- model_results:

  List returned by margot_causal_forest(), containing results and
  full_models.

- policy:

  Character; either "treat_best" (default) or "withhold_best".

- target:

  Character; weighting scheme: "AUTOC" (default) or "QINI".

- level:

  Numeric; Wald confidence level (default 0.95).

- round_digits:

  Integer; decimal places to keep (default 3).

- model_prefix:

  Character; common prefix on model names (default "model\_").

- verbose:

  Logical; print progress with cli (default TRUE).

- seed:

  Integer; base seed for reproducible RATE computations (default 12345).

- q:

  Numeric vector of quantiles at which to evaluate. Default is seq(0.1,
  1, by = 0.1) which matches the GRF default.

- use_evaluation_subset:

  Logical; if TRUE, use test indices from qini_metadata when available
  for proper out-of-sample evaluation (default TRUE).

- ...:

  Additional arguments passed to compute_rate_on_demand() and ultimately
  to grf::rank_average_treatment_effect().
