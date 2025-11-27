# Development Version of Flip CATE Estimates with Custom Covariate Support

This development function extends margot_flip_forests to allow custom
covariate specification for policy tree recalculation. Users can flip
CATE estimates, recalculate RATE/QINI results, and explore policy trees
with different sets of covariates than those originally selected by
margot_causal_forest.

## Usage

``` r
margot_flip_forests_dev(
  model_results,
  flip_outcomes = NULL,
  custom_covariates = NULL,
  exclude_covariates = NULL,
  covariate_mode = c("custom", "add", "all"),
  model_prefix = "model_",
  recalc_policy = TRUE,
  verbose = TRUE
)
```

## Arguments

- model_results:

  A list containing the model results from margot_causal_forest().

- flip_outcomes:

  A character vector of outcome variable names for which CATE estimates
  should be flipped. Can be NULL if only recalculating policy trees with
  custom covariates.

- custom_covariates:

  A character vector of covariate names to use for policy tree
  calculation. If NULL, uses the original top_n_vars from the model.

- exclude_covariates:

  A character vector of covariate names or patterns to exclude from
  policy trees. Supports exact matches and regex patterns (e.g.,
  "^t0_log\_" to exclude all variables starting with "t0_log\_").
  Exclusions are applied after covariate selection/combination.

- covariate_mode:

  Character string specifying how to use custom covariates:

  - "custom"Use only the specified custom_covariates

  - "add"Add custom_covariates to existing top_n_vars

  - "all"Use all available covariates (warns if \> 20)

- model_prefix:

  A character string indicating the prefix used for model names in the
  results list. Default is "model\_".

- recalc_policy:

  Logical; if TRUE (default) recalculates policy trees.

- verbose:

  Logical indicating whether to display detailed messages during
  execution. Default is TRUE.

## Value

A modified copy of the model_results list with updated CATE estimates
and/or recalculated policy trees.

## Details

When only `exclude_covariates` is specified (no flipping or custom
covariates), the function uses an optimized path that filters the
existing top_vars without recalculating policy trees. This significantly
improves performance for sensitivity analyses.

## Examples

``` r
if (FALSE) { # \dontrun{
# flip outcomes and use custom covariates
results_dev <- margot_flip_forests_dev(
  model_results,
  flip_outcomes = c("outcome1", "outcome2"),
  custom_covariates = c("age", "gender", "income"),
  covariate_mode = "custom"
)

# only recalculate policy trees with all covariates
results_all <- margot_flip_forests_dev(
  model_results,
  flip_outcomes = NULL,
  covariate_mode = "all"
)
} # }
```
