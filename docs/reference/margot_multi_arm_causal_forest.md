# Run Multiple Generalized Random Forest (GRF) Multi-Arm Causal Forest Models with Enhanced Features

This function is a wrapper for grf::multi_arm_causal_forest that runs
multiple GRF multi-arm causal forest models for specified outcome
variables. It calculates average treatment effects, creates custom
evaluation tables, and includes additional features such as tau.hat
estimates, policy trees, variable importance rankings, and Qini curves.
It also prepares data for policy tree visualization, using a specified
proportion of the data for training.

## Usage

``` r
margot_multi_arm_causal_forest(
  data,
  outcome_vars,
  covariates,
  W_multi,
  weights,
  exposure_name,
  grf_defaults = list(),
  save_data = FALSE,
  top_n_vars = 20,
  save_models = FALSE,
  compute_qini = TRUE,
  train_proportion = 0.7,
  W.hat = NULL,
  cost = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing all necessary variables.

- outcome_vars:

  A character vector of outcome variable names to be modeled.

- covariates:

  A matrix of covariates to be used in the GRF models.

- W_multi:

  A factor vector of multi-arm treatment assignments.

- weights:

  A vector of weights for the observations.

- exposure_name:

  A character string specifying the name of the exposure variable.

- grf_defaults:

  A list of default parameters for the GRF models.

- save_data:

  Logical indicating whether to save data, covariates, and weights.
  Default is FALSE.

- top_n_vars:

  Integer specifying the number of top variables to use for additional
  computations. Default is 10.

- save_models:

  Logical indicating whether to save the full GRF model objects. Default
  is FALSE.

- compute_qini:

  Logical indicating whether to compute Qini curves for each model.
  Default is TRUE.

- train_proportion:

  Numeric value between 0 and 1 indicating the proportion of non-missing
  data to use for training policy trees. Default is 0.7.

- W.hat:

  Optional vector specifying known treatment assignment probabilities
  for each arm.

- cost:

  Optional vector specifying the cost associated with each treatment
  arm.

- verbose:

  Logical indicating whether to display detailed messages during
  execution. Default is TRUE.

## Value

A list containing:

- results:

  A list of model results, one for each outcome variable. Each model
  result includes:

  - ateAverage treatment effect

  - custom_tableCustom evaluation table

  - tau_hatIndividual treatment effect estimates

  - top_varsTop variables by importance

  - variable_importanceData frame of variable importance rankings

  - dr_scoresDouble robust scores

  - policy_tree_depth_2Policy tree of depth 2, trained on
    train_proportion of non-missing data

  - plot_dataData prepared for policy tree visualization, using the
    remaining proportion of non-missing data

  - qini_dataData frame containing Qini curve data for plotting (if
    compute_qini is TRUE)

  - qini_objectsList of maq objects for each curve, used for computing
    average gain

- combined_tables:

  A list of data frames combining custom evaluation tables grouped by
  comparison levels.

- outcome_vars:

  The character vector of outcome variable names that were modeled.

- not_missing:

  A vector of indices for complete cases.

- exposure_name:

  The name of the exposure variable.

- data:

  The input data (if save_data is TRUE).

- covariates:

  The input covariates (if save_data is TRUE).

- weights:

  The input weights (if save_data is TRUE).

- full_models:

  A list of full GRF model objects (if save_models is TRUE).

## Note

Setting save_models = TRUE typically results in very large objects
(often several GB). Ensure you have sufficient memory available when
using this option.
