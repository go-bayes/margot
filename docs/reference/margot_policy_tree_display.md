# Fit full-sample display trees at held-out selected depths

Fits one descriptive policy tree per model on the complete registered
policy sample, using the depth selected by held-out policy-tree
cross-validation. The returned trees supply no additional held-out value
estimate.

## Usage

``` r
margot_policy_tree_display(
  model_results,
  policy_cv,
  weights = NULL,
  model_names = NULL,
  custom_covariates = NULL,
  exclude_covariates = NULL,
  covariate_mode = c("original", "custom", "add", "all"),
  label_mapping = NULL,
  tree_method = c("fastpolicytree", "policytree"),
  min_node_size = NULL,
  verbose = TRUE
)
```

## Arguments

- model_results:

  A causal-forest result accepted by \[margot_policy_tree_cv()\].

- policy_cv:

  A \[margot_policy_tree_cv()\] result containing a named \`depth_map\`.

- weights:

  Optional positive training weights. Zero weights exclude rows from the
  display-tree target sample.

- model_names:

  Optional model names to process.

- custom_covariates, exclude_covariates, covariate_mode:

  Covariate-selection arguments with the same meaning as in
  \[margot_policy_tree_cv()\].

- label_mapping:

  Optional display-label mapping.

- tree_method:

  Policy-tree engine.

- min_node_size:

  Smallest terminal-node size.

- verbose:

  Logical; print progress messages.

## Value

A list with fitted trees, split and leaf tables, and metadata.
