# Recompute Policy Trees with Custom Parameters

Computes policy trees for causal forest models with flexible covariate
selection and train/test split options. This function provides a direct
way to generate policy trees without running full causal forest
analysis, paralleling the functionality of margot_rate() and
margot_qini().

## Usage

``` r
margot_policy_tree(
  model_results,
  model_names = NULL,
  custom_covariates = NULL,
  exclude_covariates = NULL,
  covariate_mode = c("original", "custom", "add", "all"),
  depth = "both",
  train_proportion = 0.5,
  label_mapping = NULL,
  verbose = TRUE,
  seed = 12345,
  tree_method = c("policytree", "fastpolicytree")
)
```

## Arguments

- model_results:

  List returned by margot_causal_forest() or margot_flip_forests(),
  containing results and optionally covariates and data.

- model_names:

  Optional character vector specifying which models to process. Default
  NULL (all models).

- custom_covariates:

  Character vector of covariate names to use for policy trees. If NULL,
  uses the original top variables from the model.

- exclude_covariates:

  Character vector of covariate names or patterns to exclude. Supports
  exact matches and regex patterns (e.g., "\_log" excludes all variables
  containing "\_log").

- covariate_mode:

  Character string specifying how to handle covariates:

  - "original"Use original top variables from model (default)

  - "custom"Use only the specified custom_covariates

  - "add"Add custom_covariates to existing top variables

  - "all"Use all available covariates

- depth:

  Numeric or character specifying which depth(s) to compute: 1 for
  single split, 2 for two splits, or "both" for both depths (default).

- train_proportion:

  Numeric value between 0 and 1 for the proportion of data used for
  training depth-2 trees. Default is 0.5. Note: depth-1 trees use all
  available data but only the selected covariates (same as depth-2).

- label_mapping:

  Named character vector for converting variable names to readable
  labels.

- verbose:

  Logical; print progress messages (default TRUE).

- seed:

  Integer; base seed for reproducible computations (default 12345).

- tree_method:

  Character string specifying the package to use: "policytree" (default)
  or "fastpolicytree". The fastpolicytree package provides ~10x faster
  computation with identical results. Falls back to policytree if
  fastpolicytree is not installed.

## Value

A list structured similarly to margot_causal_forest() output,
containing:

- `results`: List where each element corresponds to a model and
  contains:

  - `dr_scores`: Doubly robust scores (original or flipped if available)

  - `policy_tree_depth_1`: Single-split policy tree (if requested)

  - `policy_tree_depth_2`: Two-split policy tree (if requested and
    possible)

  - `plot_data`: Data for visualization (X_test, X_test_full,
    predictions)

  - `top_vars`: Variables used for policy trees

  - `policy_tree_covariates`: Final covariate selection

  - `policy_tree_metadata`: Metadata about the computation

- `covariates`: The covariate matrix used

- `not_missing`: Indices of complete cases

- `train_proportion`: The train/test split proportion used

## Details

This function allows you to:

- Exclude specific covariates (e.g., log-transformed variables with
  "\_log")

- Use custom covariate sets for policy optimization

- Adjust the train/test split for depth-2 trees

- Recompute policy trees without re-running causal forests

The output is structured to be compatible with margot_policy(),
margot_plot_policy_tree(), margot_plot_policy_combo(), and
margot_interpret_policy_tree().

## Examples

``` r
if (FALSE) { # \dontrun{
# Recompute policy trees with default settings
policy_trees <- margot_policy_tree(causal_forest_results)

# Exclude log-transformed variables
policy_trees_no_log <- margot_policy_tree(
  causal_forest_results,
  exclude_covariates = "_log"
)

# Use custom covariates with 80/20 train/test split
policy_trees_custom <- margot_policy_tree(
  causal_forest_results,
  custom_covariates = c("age", "gender", "income"),
  covariate_mode = "custom",
  train_proportion = 0.8
)

# Compute only depth-1 trees using all covariates
policy_trees_d1 <- margot_policy_tree(
  causal_forest_results,
  covariate_mode = "all",
  depth = 1
)

# Process specific models with larger training set
policy_trees_selected <- margot_policy_tree(
  causal_forest_results,
  model_names = c("anxiety", "depression"),
  train_proportion = 0.8
)

# Visualize results
plot <- margot_plot_policy_tree(policy_trees, "model_anxiety")

# Create combined plots
plots <- margot_plot_policy_combo(policy_trees, "model_anxiety")
print(plots$combined_plot)

# Interpret results
margot_interpret_policy_tree(policy_trees, "model_anxiety")
} # }
```
