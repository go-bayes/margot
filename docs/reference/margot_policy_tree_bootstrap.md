# Bootstrap Analysis for Policy Trees

Performs bootstrap analysis of policy trees to assess stability and
generate consensus trees. By default, varies random seeds to create
different train/test splits rather than bootstrap resampling, as trees
are highly sensitive to data perturbations. Uses memory-efficient
streaming approach to handle large datasets.

This function is deprecated. Please use
[`margot_policy_tree_stability`](https://go-bayes.github.io/margot/reference/margot_policy_tree_stability.md)
instead.

## Usage

``` r
margot_policy_tree_bootstrap(...)

margot_policy_tree_bootstrap(...)
```

## Arguments

- ...:

  Arguments passed to margot_policy_tree_stability

- model_results:

  List returned by margot_causal_forest() or margot_flip_forests()

- model_names:

  Character vector of model names to analyze. NULL = all models.

- custom_covariates:

  Character vector of covariate names to use for policy trees. If NULL,
  uses the original top variables from the model.

- exclude_covariates:

  Character vector of covariate names or patterns to exclude. Supports
  exact matches and regex patterns (e.g., "\_log" excludes all variables
  containing "\_log").

- covariate_mode:

  Character string specifying how to handle covariates: "original" (use
  original top variables), "custom" (use only custom_covariates), "add"
  (add custom to existing), "all" (use all available covariates).

- depth:

  Numeric or character specifying which depth(s) to compute: 1 for
  single split, 2 for two splits (default), or "both" for both depths.

- n_bootstrap:

  Integer. Number of bootstrap iterations (default 300).

- vary_type:

  Character. Type of variation: "split_only" (vary train/test split via
  seeds), "sample_only" (bootstrap resample), "both" (resample + split).
  Default is "split_only".

- consensus_threshold:

  Numeric. Minimum inclusion frequency for consensus (default 0.5).

- train_proportion:

  Numeric. Train/test split when vary_train_proportion = FALSE (default
  0.5).

- vary_train_proportion:

  Logical. Whether to vary train proportion (default FALSE).

- train_proportions:

  Numeric vector. Proportions to cycle through when
  vary_train_proportion = TRUE (default c(0.4, 0.5, 0.6, 0.7)).

- label_mapping:

  Named character vector for converting variable names to readable
  labels.

- return_consensus_trees:

  Logical. Return fitted consensus trees (default TRUE).

- metaseed:

  Integer. Master seed for reproducibility (default 12345).

- parallel:

  Logical. Use parallel processing (default FALSE).

- n_cores:

  Integer. Number of cores for parallel processing.

- verbose:

  Logical. Print progress messages (default TRUE).

- seed:

  Integer. Additional seed parameter for compatibility (default 12345).

- tree_method:

  Character string specifying the package to use: "fastpolicytree"
  (default) or "policytree". The fastpolicytree package provides ~10x
  faster computation, which is particularly beneficial for bootstrap
  analysis. Falls back to policytree if fastpolicytree is not installed.

## Value

Object of class "margot_bootstrap_policy_tree" containing:

- results: List with consensus trees and bootstrap metrics per model

- summary_metrics: Variable importance and convergence diagnostics

- metadata: Bootstrap parameters and seeds used

Result from margot_policy_tree_stability

## Details

The function uses a memory-efficient approach:

- Processes one tree at a time

- Extracts only essential split information

- Accumulates statistics without storing all trees

- Reconstructs single consensus trees for compatibility

By default, the function varies random seeds to create different
train/test splits for each iteration rather than using bootstrap
resampling. This is because decision trees are highly sensitive to small
data perturbations, and seed variation provides a more realistic
assessment of tree stability.

## Theoretical Background

Policy trees inherit the instability of decision trees, where small
changes in the data can lead to completely different tree structures
(Breiman, 1996). This instability is particularly pronounced when
predictors are correlated, as the tree can arbitrarily choose between
similar variables at split points. Athey and Wager's (2021) policy
learning framework acknowledges these challenges while providing methods
to extract robust insights despite the instability.

The bootstrap analysis helps distinguish between:

- Fundamental instability due to weak or absent treatment effect
  heterogeneity

- Apparent instability due to correlated predictors that capture similar
  information

- Robust patterns that emerge consistently across different data samples

Use the companion functions \`margot_assess_variable_correlation()\` and
\`margot_stability_diagnostics()\` to better understand the sources of
instability.

Three types of variation are supported:

- "both": Varies both bootstrap sampling and train/test splits

- "sample_only": Only bootstrap resampling, fixed train/test split

- "split_only": Fixed sample, only varies train/test split

## Complete Workflow Example


    # 1. Run causal forest (save data for correlation analysis)
    cf_results <- margot_causal_forest(
      data = your_data,
      outcome_vars = c("outcome1", "outcome2"),
      save_data = TRUE  # Important for correlation analysis
    )

    # 2. Run bootstrap analysis to assess stability
    boot_results <- margot_policy_tree_bootstrap(
      cf_results,
      n_bootstrap = 300,
      tree_method = "fastpolicytree"  # 10x faster if available
    )

    # 3. Check variable correlations
    cor_analysis <- margot_assess_variable_correlation(
      cf_results,  # Use original results, NOT boot_results
      "model_outcome1"
    )

    # 4. Identify clusters of correlated variables
    clusters <- margot_identify_variable_clusters(cor_analysis)

    # 5. Get comprehensive diagnostics
    diagnostics <- margot_stability_diagnostics(
      bootstrap_results = boot_results,
      model_results = cf_results,
      model_name = "model_outcome1"
    )

    # 6. Interpret results
    interpretation <- margot_interpret_bootstrap(
      boot_results,
      "model_outcome1",
      include_theory = TRUE  # Include theoretical context
    )

## References

Athey, S., & Wager, S. (2021). Policy learning with observational data.
Econometrica, 89(1), 133-161.

Breiman, L. (1996). Bagging predictors. Machine Learning, 24(2),
123-140.

Zhou, Z., Athey, S., & Wager, S. (2023). Offline multi-action policy
learning: Generalization and optimization. Operations Research, 71(1),
148-183.

## See also

[`margot_policy_tree`](https://go-bayes.github.io/margot/reference/margot_policy_tree.md)
for computing policy trees without bootstrap
[`margot_assess_variable_correlation`](https://go-bayes.github.io/margot/reference/margot_assess_variable_correlation.md)
for correlation analysis
[`margot_stability_diagnostics`](https://go-bayes.github.io/margot/reference/margot_stability_diagnostics.md)
for comprehensive diagnostics

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic bootstrap with fixed train proportion
boot_results <- margot_policy_tree_bootstrap(
  causal_forest_results,
  n_bootstrap = 300
)

# Vary train proportion with default values
boot_results <- margot_policy_tree_bootstrap(
  causal_forest_results,
  vary_train_proportion = TRUE
)

# Custom train proportions
boot_results <- margot_policy_tree_bootstrap(
  causal_forest_results,
  vary_train_proportion = TRUE,
  train_proportions = c(0.3, 0.5, 0.7)
)

# Use bootstrap resampling instead of seed variation
boot_results <- margot_policy_tree_bootstrap(
  causal_forest_results,
  vary_type = "sample_only",
  n_bootstrap = 300
)

# Plot consensus tree
margot_plot_policy_tree(boot_results, "model_anxiety")

# Get bootstrap summary
summary(boot_results)

# Interpret results with theoretical context
interpretation <- margot_interpret_bootstrap(
  boot_results,
  "model_anxiety",
  format = "text"
)

# Assess variable correlations (using original causal forest results)
cor_analysis <- margot_assess_variable_correlation(
  causal_forest_results, # NOT boot_results
  "model_anxiety"
)

# Identify variable clusters
clusters <- margot_identify_variable_clusters(cor_analysis)

# Run comprehensive stability diagnostics
diagnostics <- margot_stability_diagnostics(
  bootstrap_results = boot_results,
  model_results = causal_forest_results,
  model_name = "model_anxiety"
)
} # }
```
