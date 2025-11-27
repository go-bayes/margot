# Run Multiple Generalized Random Forest (GRF) Causal Forest Models with Enhanced Features

This function is a wrapper for grf::causal_forest that runs multiple GRF
causal forest models for specified outcome variables. It calculates
average treatment effects, tests calibration, creates custom evaluation
tables, and includes additional features such as tau.hat estimates, RATE
calculations, policy trees, variable importance rankings, and best
linear projections.

## Usage

``` r
margot_grf_causal_forest(
  data,
  outcome_vars,
  covariates,
  W,
  weights,
  grf_defaults = list(),
  save_data = FALSE,
  compute_rate = TRUE,
  top_n_vars = 10,
  save_models = FALSE
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
  computations. Default is 10.

- save_models:

  Logical indicating whether to save the full GRF model objects. Default
  is FALSE.

## Value

A list containing:

- results:

  A list of model results, one for each outcome variable.

- combined_table:

  A data frame combining all custom evaluation tables.

- outcome_vars:

  The character vector of outcome variable names that were modeled.

- not_missing:

  A vector of indices for complete cases.

- data:

  The input data (if save_data is TRUE).

- covariates:

  The input covariates (if save_data is TRUE).

- weights:

  The input weights (if save_data is TRUE).

- full_models:

  A list of full GRF model objects (if save_models is TRUE).
