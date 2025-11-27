# Run Multiple Generalized Random Forest (GRF) Causal Forest Models with Enhanced Qini Cross-Validation

This function runs multiple GRF causal forest models with enhanced
features. In addition to estimating causal effects, it can compute the
Rank-Weighted Average Treatment Effect (RATE) for each model. It also
gives you the option to train a separate "Qini forest" on a subset of
data and compute Qini curves on held-out data, thereby avoiding
in-sample optimism in the Qini plots.

## Usage

``` r
margot_causal_forest(
  data,
  outcome_vars,
  covariates,
  W,
  weights,
  grf_defaults = list(),
  save_data = FALSE,
  compute_rate = TRUE,
  top_n_vars = 15,
  save_models = TRUE,
  train_proportion = 0.5,
  train_prop = NULL,
  qini_train_prop = NULL,
  compute_conditional_means = TRUE,
  compute_marginal_only = FALSE,
  verbose = TRUE,
  qini_treatment_cost = 1,
  seed = 12345,
  use_train_test_split = TRUE,
  flip_outcomes = NULL,
  flip_method = "zscore",
  flip_scale_bounds = NULL
)
```

## Arguments

- data:

  A data frame containing all necessary variables.

- outcome_vars:

  A character vector of outcome variable names to be modelled.

- covariates:

  A matrix of covariates to be used in the GRF models.

- W:

  A vector of binary treatment assignments.

- weights:

  A vector of weights for the observations.

- grf_defaults:

  A list of default parameters for the GRF models.

- save_data:

  Logical indicating whether to save data, covariates, and weights.
  Default is FALSE.

- compute_rate:

  Logical indicating whether to compute RATE for each model. Default is
  TRUE. Note: Direct computation of RATE, QINI, and policy trees within
  this function may be deprecated in future versions. Use margot_rate(),
  margot_qini(), and margot_policy_tree() instead.

- top_n_vars:

  Integer specifying the number of top variables to use for additional
  computations. Default is 15.

- save_models:

  Logical indicating whether to save the full GRF model objects. Default
  is TRUE.

- train_proportion:

  Numeric value between 0 and 1 indicating the proportion of data to use
  for training set, or NULL. Used for policy trees and QINI evaluation
  when compute_marginal_only=FALSE. Must be NULL when
  compute_marginal_only=TRUE. Default is 0.5.

- train_prop:

  Deprecated. Use train_proportion instead.

- qini_train_prop:

  Deprecated. Use train_proportion instead.

- compute_conditional_means:

  Logical indicating whether to compute conditional means using
  [`policytree::conditional_means()`](https://rdrr.io/pkg/policytree/man/conditional_means.html).
  These represent expected outcomes under each treatment arm. Default is
  TRUE.

- compute_marginal_only:

  Logical indicating whether to compute only marginal (average)
  treatment effects, skipping heterogeneity metrics (QINI curves, RATE,
  policy trees). When TRUE, train_proportion must be NULL. When FALSE
  (default), heterogeneity metrics are computed using train/test splits.

- verbose:

  Logical indicating whether to display detailed messages during
  execution. Default is TRUE.

- qini_treatment_cost:

  Scalar treatment cost per unit for QINI calculations. Default 1. Lower
  values (e.g., 0.2) represent cheap treatments creating steeper QINI
  curves; higher values (e.g., 5) represent expensive treatments
  creating shallower curves.

- seed:

  Integer. Random seed for reproducibility of train/test splits for
  policy trees and QINI evaluation. Default is 12345.

- use_train_test_split:

  Logical. If TRUE (default), uses a consistent train/test split
  where: - The main causal forest is trained on all data (following GRF
  best practices for honesty) - All reported results (ATE, E-values,
  combined_table) are computed on the TEST SET - Policy trees and QINI
  also use the same train/test split - All-data results are stored in
  split_info for reference If FALSE, the main forest uses all data for
  estimation and policy trees/QINI get their own splits.

- flip_outcomes:

  Character vector or list specifying outcomes to flip (reverse
  score). - Character vector: applies default flip_method and
  flip_scale_bounds to all listed outcomes - List: allows per-outcome
  specification, e.g., list(anxiety = list(method = "ordinal",
  scale_bounds = c(1, 7))) Flipped outcomes are appended with "\_r"
  suffix in results.

- flip_method:

  Character string specifying default inversion method when
  flip_outcomes is provided: - "zscore": Simple negation (default).
  Assumes data is already standardized (e.g., outcomes ending in
  "\_z"). - "ordinal": Invert on ordinal scale using bounds. Use for
  Likert scales or bounded measures.

- flip_scale_bounds:

  Numeric vector of length 2 \[min, max\] or named list of bounds per
  outcome. Required when flip_method = "ordinal" unless bounds should be
  inferred from data. Example: c(1, 7) for a 7-point Likert scale, or
  list(anxiety = c(1, 7), depression = c(0, 21)).

## Value

A list containing: \* \`results\` - per-outcome diagnostics and objects
\* \`combined_table\` - combined e-value table across outcomes \*
\`outcome_vars\` - vector of outcome names \* \`not_missing\` - indices
of complete-case rows \* (\`data\`, \`covariates\`, \`weights\`, \`W\`)
when \`save_data = TRUE\` \* \`full_models\` when \`save_models = TRUE\`
\* \`flip_info\` when \`flip_outcomes\` is used - contains mapping and
metadata for flipped outcomes
