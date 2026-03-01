# Wrapper Function for QINI Analysis with Backward Compatibility

This function provides backward compatibility for margot_interpret_qini
by formatting the output of margot_qini_alternative into the expected
structure.

## Usage

``` r
margot_qini_wrapper(
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

  Numeric vector of spend levels for analysis (default 0.1)

- label_mapping:

  Named list for label transformations

## Value

List formatted for margot_interpret_qini with diff_gain_summaries
