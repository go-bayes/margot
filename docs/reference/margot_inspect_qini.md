# Inspect qini diagnostics for one or several models

Inspect qini diagnostics for one or several models

## Usage

``` r
margot_inspect_qini(
  model_results,
  model_names = NULL,
  test_prop = 0.5,
  propensity_bounds = c(0.05, 0.95),
  seed = 2025
)
```

## Arguments

- model_results:

  list returned by \`margot_causal_forest()\` \*\*with\*\* \`save_models
  = TRUE, save_data = TRUE\`.

- model_names:

  optional character vector of outcome names (with or without the
  \`model\_\` prefix). default = \*all\*.

- test_prop:

  fraction of trimmed rows to allocate to the validation/Qini test set.
  default 0.5.

- propensity_bounds:

  numeric length-2 vector giving the lower and upper trimming thresholds
  for \`forest\$W.hat\`. default c(0.05, 0.95).

- seed:

  integer for reproducibility.

## Value

a tibble of diagnostics (class \`"margot_qini_diag"\`).
