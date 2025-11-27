# Compute QINI Curves (Alternative Implementation)

Computes QINI curves using existing data in margot_causal_forest
results. This version works with the standard output structure without
requiring save_models = TRUE.

## Usage

``` r
margot_qini_alternative(
  margot_result,
  model_names = NULL,
  seed = 12345,
  n_bootstrap = 200,
  verbose = TRUE,
  spend_levels = c(0.1, 0.4),
  label_mapping = NULL
)
```

## Arguments

- margot_result:

  Output from margot_causal_forest()

- model_names:

  Character vector of model names to process (NULL = all)

- seed:

  Random seed for reproducibility

- n_bootstrap:

  Number of bootstrap replicates for confidence intervals

- verbose:

  Print progress messages

- spend_levels:

  Numeric vector of spend levels for diff_gain_summaries (default 0.1)

- label_mapping:

  Ignored (for compatibility)
