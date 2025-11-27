# Recalculate Policy Trees with Custom Covariates

This function allows recalculation of policy trees using custom
covariate sets. It provides flexibility to explore different covariate
specifications for policy tree estimation while maintaining the original
causal forest results.

## Usage

``` r
margot_recalculate_policy_trees(
  model_results,
  outcomes_to_recalculate = NULL,
  custom_covariates = NULL,
  exclude_covariates = NULL,
  covariate_mode = c("original", "custom", "add", "all"),
  model_prefix = "model_",
  verbose = TRUE,
  parallel = FALSE,
  n_cores = future::availableCores() - 1,
  seed = 12345,
  tree_method = c("policytree", "fastpolicytree")
)
```

## Arguments

- model_results:

  A list containing the model results from margot_causal_forest().

- outcomes_to_recalculate:

  Character vector of outcome names to recalculate. If NULL (default),
  recalculates all models marked as needing recalculation.

- custom_covariates:

  Character vector of covariate names to use for policy trees. If NULL,
  uses the original top variables from the model.

- exclude_covariates:

  Character vector of covariate names or patterns to exclude. Supports
  exact matches and regex patterns (e.g., "^t0_log\_" excludes all
  variables starting with "t0_log\_").

- covariate_mode:

  Character string specifying how to handle covariates:

  - "original"Use original top variables from model (default)

  - "custom"Use only the specified custom_covariates

  - "add"Add custom_covariates to existing top variables

  - "all"Use all available covariates

- model_prefix:

  Character string prefix for model names. Default is "model\_".

- verbose:

  Logical indicating whether to display messages. Default is TRUE.

- parallel:

  Logical indicating whether to use parallel processing. Default is
  FALSE.

- n_cores:

  Number of cores for parallel processing. Default is availableCores() -
  1.

- seed:

  Random seed for reproducibility. Default is NULL.

- tree_method:

  Character string specifying which method to use for policy tree
  computation:

  - "policytree"Use the policytree package (default)

  - "fastpolicytree"Use the fastpolicytree package (about 10x faster)

## Value

A modified copy of model_results with recalculated policy trees.

## Details

This function is useful for:

- Exploring sensitivity of policy recommendations to covariate selection

- Excluding covariates with measurement error or other concerns

- Using domain knowledge to select policy-relevant covariates

- Testing robustness of policy tree findings

The function preserves all original model results and only updates:

- policy_tree_depth_1: Single-split policy tree

- policy_tree_depth_2: Two-split policy tree (if ≥2 covariates)

- plot_data: Data for visualization

- Metadata about which covariates were used

## Examples

``` r
if (FALSE) { # \dontrun{
# recalculate with custom covariates
results_custom <- margot_recalculate_policy_trees(
  model_results,
  custom_covariates = c("age", "gender", "income"),
  covariate_mode = "custom"
)

# exclude measurement error variables
results_clean <- margot_recalculate_policy_trees(
  model_results,
  exclude_covariates = c("bmi", "^log_"),
  covariate_mode = "original"
)

# use all covariates
results_all <- margot_recalculate_policy_trees(
  model_results,
  covariate_mode = "all"
)
} # }
```
