# Stability Analysis for Policy Trees

Performs stability analysis of policy trees to assess robustness and
generate consensus trees. By default, varies random seeds to create
different train/test splits to assess stability. Optionally supports
bootstrap resampling for traditional bootstrap analysis. Uses
memory-efficient streaming approach to handle large datasets.

## Usage

``` r
margot_policy_tree_stability(
  model_results,
  model_names = NULL,
  custom_covariates = NULL,
  exclude_covariates = NULL,
  covariate_mode = c("original", "custom", "add", "all"),
  depth = 2,
  n_iterations = 300,
  vary_type = c("split_only", "bootstrap", "both"),
  consensus_threshold = 0.5,
  train_proportion = 0.5,
  vary_train_proportion = FALSE,
  train_proportions = c(0.4, 0.5, 0.6, 0.7),
  label_mapping = NULL,
  return_consensus_trees = TRUE,
  metaseed = 12345,
  parrallel = NULL,
  parallel = FALSE,
  n_cores = NULL,
  verbose = TRUE,
  seed = 12345,
  tree_method = c("fastpolicytree", "policytree"),
  n_bootstrap = NULL,
  compute_policy_values = FALSE,
  policy_value_R = 499L,
  policy_value_seed = 42L,
  policy_value_baseline = c("control_all", "treat_all"),
  future_globals_max_size = 20 * 1024^3
)
```

## Arguments

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

- n_iterations:

  Integer. Number of stability iterations (default 300).

- vary_type:

  Character. Type of variation: "split_only" (vary train/test split via
  seeds), "bootstrap" (bootstrap resample), "both" (resample + split).
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

- parrallel:

  Deprecated misspelling of \`parallel\`; retained for backwards
  compatibility.

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
  faster computation, which is particularly beneficial for stability
  analysis. Falls back to policytree if fastpolicytree is not installed.

- n_bootstrap:

  Deprecated. Use n_iterations instead.

- compute_policy_values:

  Logical. If TRUE, compute bootstrap policy-value tests for each
  consensus tree and store results in \`policy_value_depth\_\*\` slots
  (default FALSE).

- policy_value_R:

  Integer ≥ 1. Number of bootstrap replicates for policy values when
  \`compute_policy_values = TRUE\` (default 499).

- policy_value_seed:

  Optional integer seed for policy-value bootstraps. Use NULL to inherit
  the ambient RNG state.

- policy_value_baseline:

  Contrast baseline for stored policy values when
  \`compute_policy_values = TRUE\`. One of "control_all" (default) or
  "treat_all".

- future_globals_max_size:

  Maximum size (in bytes) of globals that can be exported to parallel
  workers when \`parallel = TRUE\`. Defaults to 20 GiB. Increase this if
  large model objects trigger the \`future.globals.maxSize\` guard.

## Value

Object of class "margot_stability_policy_tree" containing:

- results: List with consensus trees and stability metrics per model

- summary_metrics: Variable importance and convergence diagnostics

- metadata: Analysis parameters and seeds used

## Details

The function uses a memory-efficient approach:

- Processes one tree at a time

- Extracts only essential split information

- Accumulates statistics without storing all trees

- Reconstructs single consensus trees for compatibility

When \`compute_policy_values = TRUE\`, each consensus tree is also
evaluated against the specified baseline and bootstrap policy-value
estimates are stored in \`policy_value_depth_1\` /
\`policy_value_depth_2\`. This allows downstream reporters to reuse
consistent estimates without rerunning the bootstrap.

By default, the function varies random seeds to create different
train/test splits for each iteration. This assesses the stability of the
policy tree structure without the computational overhead and statistical
assumptions of bootstrap resampling. True bootstrap resampling can be
enabled with vary_type = "bootstrap".

## Theoretical Background

Policy trees inherit the instability of decision trees, where small
changes in the data can lead to completely different tree structures
(Breiman, 1996). This instability is particularly pronounced when
predictors are correlated, as the tree can arbitrarily choose between
similar variables at split points. Athey and Wager's (2021) policy
learning framework acknowledges these challenges while providing methods
to extract robust insights despite the instability.

The stability analysis helps distinguish between:

- Fundamental instability due to weak or absent treatment effect
  heterogeneity

- Apparent instability due to correlated predictors that capture similar
  information

- Robust patterns that emerge consistently across different data samples

Use the companion functions \`margot_assess_variable_correlation()\` and
\`margot_stability_diagnostics()\` to better understand the sources of
instability.

Three types of variation are supported:

- "split_only": Fixed sample, only varies train/test split (default)

- "bootstrap": Bootstrap resampling with replacement

- "both": Varies both bootstrap sampling and train/test splits

## Complete Workflow Example


    # 1. Run causal forest (save data for correlation analysis)
    cf_results <- margot_causal_forest(
      data = your_data,
      outcome_vars = c("outcome1", "outcome2"),
      save_data = TRUE  # Important for correlation analysis
    )

    # 2. Run stability analysis to assess robustness
    stability_results <- margot_policy_tree_stability(
      cf_results,
      n_iterations = 300,
      tree_method = "fastpolicytree"  # 10x faster if available
    )

    # 3. Check variable correlations
    cor_analysis <- margot_assess_variable_correlation(
      cf_results,  # Use original results, NOT stability_results
      "model_outcome1"
    )

    # 4. Identify clusters of correlated variables
    clusters <- margot_identify_variable_clusters(cor_analysis)

    # 5. Get comprehensive diagnostics
    diagnostics <- margot_stability_diagnostics(
      stability_results = stability_results,
      model_results = cf_results,
      model_name = "model_outcome1"
    )

    # 6. Interpret results
    interpretation <- margot_interpret_stability(
      stability_results,
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
for computing policy trees without stability analysis
[`margot_assess_variable_correlation`](https://go-bayes.github.io/margot/reference/margot_assess_variable_correlation.md)
for correlation analysis
[`margot_stability_diagnostics`](https://go-bayes.github.io/margot/reference/margot_stability_diagnostics.md)
for comprehensive diagnostics

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic stability analysis with fixed train proportion
stability_results <- margot_policy_tree_stability(
  causal_forest_results,
  n_iterations = 300
)

# Vary train proportion with default values
stability_results <- margot_policy_tree_stability(
  causal_forest_results,
  vary_train_proportion = TRUE
)

# Custom train proportions
stability_results <- margot_policy_tree_stability(
  causal_forest_results,
  vary_train_proportion = TRUE,
  train_proportions = c(0.3, 0.5, 0.7)
)

# Use bootstrap resampling instead of seed variation
stability_results <- margot_policy_tree_stability(
  causal_forest_results,
  vary_type = "bootstrap",
  n_iterations = 300
)

# Plot consensus tree
margot_plot_policy_tree(stability_results, "model_anxiety")

# Get stability summary
summary(stability_results)

# Interpret results with theoretical context
interpretation <- margot_interpret_stability(
  stability_results,
  "model_anxiety",
  format = "text"
)

# Assess variable correlations (using original causal forest results)
cor_analysis <- margot_assess_variable_correlation(
  causal_forest_results, # NOT stability_results
  "model_anxiety"
)

# Identify variable clusters
clusters <- margot_identify_variable_clusters(cor_analysis)

# Run comprehensive stability diagnostics
diagnostics <- margot_stability_diagnostics(
  stability_results = stability_results,
  model_results = causal_forest_results,
  model_name = "model_anxiety"
)
} # }
```
