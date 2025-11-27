# Development Version of Causal Forest with Enhanced Features

This development version implements the new architecture with proper
train/test splits, evaluation forests, and integrated missing data
handling. It maintains a 50/50 train/test split by default and supports
toy data sampling.

## Usage

``` r
margot_causal_forest_dev(
  data,
  outcome_vars,
  treatment = "A",
  covariates = NULL,
  weights = NULL,
  train_prop = 0.5,
  toy_data_prop = NULL,
  grf_defaults = list(),
  eval_forest = TRUE,
  save_data = TRUE,
  save_forests = TRUE,
  handle_missing = "complete",
  seed = 12345,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing all variables

- outcome_vars:

  Character vector of outcome variable names

- treatment:

  Character name of treatment variable (default: "A")

- covariates:

  Character vector of covariate names. If NULL, uses all variables
  except outcomes, treatment, and special columns.

- weights:

  Optional weights vector or column name

- train_prop:

  Numeric. Proportion for train/test split (default: 0.5)

- toy_data_prop:

  Numeric. If provided, uses a random sample of this proportion for
  testing (default: NULL)

- grf_defaults:

  List of parameters passed to grf::causal_forest()

- eval_forest:

  Logical. Whether to create evaluation forest for test set doubly
  robust scores (default: TRUE)

- save_data:

  Logical. Whether to save input data (default: TRUE)

- save_forests:

  Logical. Whether to save forest objects (default: TRUE)

- handle_missing:

  Character. How to handle missing data: "complete" (default), "impute",
  or "forest" (let forest handle it)

- seed:

  Integer. Random seed (default: NULL)

- verbose:

  Logical. Whether to print progress (default: TRUE)

## Value

List containing:

- results:

  List of model results by outcome

- data_info:

  Information about data splits and missing values

- forests:

  Causal forest objects (if save_forests = TRUE)

- eval_forests:

  Evaluation forest objects (if eval_forest = TRUE)

- data:

  Original data (if save_data = TRUE)

- metadata:

  Model metadata and parameters

## Details

Key differences from original margot_causal_forest(): - Consistent 50/50
train/test split for all analyses - Optional evaluation forests for
computing test set DR scores - Integrated missing data handling
options - Support for toy data sampling - Simplified structure focused
on forest estimation - Returns data needed for downstream analysis
functions

The evaluation forest is trained on the training data and used to
compute doubly robust scores for the test set, ensuring proper
out-of-sample evaluation for QINI curves and other metrics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate test data
test_data <- margot_simulate_test_data()

# Run causal forest with default settings
cf_results <- margot_causal_forest_dev(
  data = test_data$data,
  outcome_vars = c("Y1", "Y2", "Y3", "Y4"),
  treatment = "A"
)

# Run with toy data for quick testing
cf_results_toy <- margot_causal_forest_dev(
  data = test_data$data,
  outcome_vars = c("Y1", "Y2"),
  treatment = "A",
  toy_data_prop = 0.1
)
} # }
```
