# Parallel flip of CATE estimates with reproducible RNG and memory guard

This function modifies models IN PLACE by flipping their treatment
effects, unlike margot_flip_forests which creates new models with "\_r"
suffix. The remove_original and grf_defaults parameters are ignored in
this version.

## Usage

``` r
margot_flip_forests_parallel(
  model_results,
  flip_outcomes,
  model_prefix = "model_",
  grf_defaults = NULL,
  recalc_policy = TRUE,
  parallel_policy = FALSE,
  n_policy_cores = future::availableCores() - 1,
  verbose = TRUE,
  n_cores = future::availableCores() - 1,
  max_size_GB = 2,
  seed = TRUE,
  remove_original = TRUE
)
```

## Arguments

- model_results:

  A list containing the model results from margot_causal_forest().

- flip_outcomes:

  A character vector of outcome variable names for which models should
  be flipped.

- model_prefix:

  A character string indicating the prefix used for model names in the
  results list. Default is "model\_".

- grf_defaults:

  A list of parameters to pass to the GRF causal forest models. Note:
  This parameter is ignored in the parallel version which modifies
  existing models in place rather than creating new ones.

- parallel_policy:

  logical. if TRUE, policy-tree refits are parallelised.

- n_policy_cores:

  integer. number of workers for policy-tree refits.

- verbose:

  Logical indicating whether to display detailed messages during
  execution. Default is TRUE.

- n_cores:

  Number of cores to use for parallel processing. Default is
  availableCores() - 1.

- max_size_GB:

  numeric. globals cap passed to \`future.globals.maxSize\` (default 2
  GB).

- seed:

  \`TRUE\` for automatic parallel-safe RNG, or an integer for
  deterministic streams.

- remove_original:

  logical indicating whether to remove the original (non-flipped) models
  after creating their flipped versions. Default is TRUE. Note: This
  parameter is ignored in the parallel version which modifies models in
  place.
