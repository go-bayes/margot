# Flip (Reverse) Causal Forest Treatment Effects

This function is now a convenience wrapper around margot_causal_forest()
with the flip_outcomes parameter. It allows flipping existing models by
recomputing them with inverted outcomes.

## Usage

``` r
margot_flip_forests(
  model_results,
  flip_outcomes,
  model_prefix = "model_",
  flip_method = "zscore",
  flip_scale_bounds = NULL,
  grf_defaults = NULL,
  parallel = FALSE,
  n_cores = future::availableCores() - 1,
  verbose = TRUE,
  remove_original = TRUE,
  qini_treatment_cost = 1,
  train_proportion = NULL,
  use_train_test_split = NULL,
  seed = 12345
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

- flip_method:

  Character string specifying default inversion method: - "zscore":
  Simple negation (default). Assumes data is already standardized. -
  "ordinal": Invert on ordinal scale using bounds.

- flip_scale_bounds:

  Numeric vector of length 2 \[min, max\] or named list of bounds per
  outcome. Required when flip_method = "ordinal" unless bounds should be
  inferred from data.

- grf_defaults:

  A list of parameters to pass to the GRF causal forest models. Default
  is NULL, which attempts to extract parameters from the original fitted
  models. Providing explicit values ensures consistency between original
  and flipped models. Common parameters include num.trees, honesty,
  honesty.fraction, alpha, min.node.size, and mtry.

- parallel:

  Logical indicating whether to use parallel processing. Default is
  FALSE.

- n_cores:

  Number of cores to use for parallel processing. Default is
  availableCores() - 1.

- verbose:

  Logical indicating whether to display detailed messages during
  execution. Default is TRUE.

- remove_original:

  Logical indicating whether to remove the original (non-flipped) models
  after creating their flipped versions. Default is TRUE. When TRUE,
  only the flipped models with "\_r" suffix remain; when FALSE, both
  original and flipped models are kept.

- qini_treatment_cost:

  Scalar treatment cost per unit for QINI calculations. Default 1. Lower
  values (e.g., 0.2) represent cheap treatments creating steeper QINI
  curves; higher values (e.g., 5) represent expensive treatments
  creating shallower curves.

- seed:

  Random seed for reproducible QINI curve generation. Default 12345
  matches the default seed used in compute_qini_improved for
  consistency.

## Value

A modified copy of model_results with flipped models (with "\_r"
suffix). If remove_original is TRUE (default), original models are
removed; otherwise both versions are kept.

## Details

This function creates entirely new models by: 1. Extracting the original
data for each outcome 2. Flipping the outcome variable (Y_flipped = -Y)
3. Calling margot_causal_forest() with the flipped outcome 4. Storing
the result with "\_r" suffix 5. Optionally removing the original models
(default behavior)

This ensures all components (forest model, CATE estimates, QINI curves,
policy trees, E-values, etc.) are consistently computed based on the
flipped outcome.

When grf_defaults is NULL, the function attempts to extract key GRF
parameters from the first fitted model to ensure consistency between
original and flipped models.

## Examples

``` r
if (FALSE) { # \dontrun{
# Flip models and remove originals (default)
results_flipped <- margot_flip_forests(
  results,
  flip_outcomes = c("anxiety", "depression")
)
# Results now contain anxiety_r and depression_r, originals removed

# Keep both original and flipped models
results_both <- margot_flip_forests(
  results,
  flip_outcomes = c("anxiety", "depression"),
  remove_original = FALSE
)
# Results contain both anxiety/depression and anxiety_r/depression_r

# Use custom GRF parameters for flipped models
results_custom <- margot_flip_forests(
  results,
  flip_outcomes = c("anxiety", "depression"),
  grf_defaults = list(num.trees = 4000, honesty = TRUE, honesty.fraction = 0.5)
)
} # }
```
