# Run Multiple Generalized Random Forest (GRF) Models with Enhanced Features

This function runs multiple GRF models for specified outcome variables,
calculates average treatment effects, tests calibration, creates custom
evaluation tables, and includes additional features such as tau.hat
estimates, RATE calculations, policy trees, variable importance
rankings, best linear projections, and depth-2 policy trees.

## Usage

``` r
margot_run_models_grf(
  data,
  outcome_vars,
  covariates,
  W,
  weights,
  grf_defaults = list(),
  save_data = FALSE,
  compute_rate = TRUE,
  top_n_vars = 10
)
```

## Arguments

- data:

  A data frame containing all necessary variables.

- outcome_vars:

  A character vector of outcome variable names to be modeled.

- covariates:

  A matrix of covariates to be used in the GRF models.

- W:

  A matrix of treatment assignments.

- weights:

  A vector of weights for the observations.

- grf_defaults:

  A list of default parameters for the GRF models.

- save_data:

  Logical indicating whether to save data, covariates, and weights.
  Default is FALSE.

- compute_rate:

  Logical indicating whether to compute RATE for each model. Default is
  TRUE.

- top_n_vars:

  Integer specifying the number of top variables to use for additional
  computations. Default is 15.

## Value

A list containing:

- results:

  A list of model results, one for each outcome variable.

- combined_table:

  A data frame combining all custom evaluation tables.

- outcome_vars:

  The character vector of outcome variable names that were modeled.

- tau_hats:

  A list of tau.hat estimates for each model.

- tau_hat_plots:

  A list of ggplot objects for tau.hat histograms.

- rate_results:

  A list of RATE results (if compute_rate is TRUE).

- dr_scores:

  A list of double robust scores for each model.

- policy_trees:

  A list of policy trees of depth 1 for each model.

- not_missing:

  A vector of indices for complete cases.

- variable_importance_rankings:

  A list of top n variables by importance for each model, where n is
  specified by top_n_vars.

- best_linear_projections:

  A list of best linear projection results using top n variables for
  each model, where n is specified by top_n_vars.

- policy_trees_depth_2:

  A list of policy trees of depth 2 using top n variables for each
  model, where n is specified by top_n_vars.

- data:

  The input data (if save_data is TRUE).

- covariates:

  The input covariates (if save_data is TRUE).

- weights:

  The input weights (if save_data is TRUE).
