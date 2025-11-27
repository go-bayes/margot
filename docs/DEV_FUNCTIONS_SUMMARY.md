# Development Functions Summary

## Overview

The new development functions have been successfully created and tested.
They implement a cleaner architecture with proper train/test separation
and integrated functionality.

## Functions Created

### 1. `margot_simulate_test_data()`

- Generates synthetic panel data with known treatment effects
- Supports multiple effect patterns: positive, negative, null,
  heterogeneous
- Only introduces missing data in covariates (not treatment/outcomes)
  per GRF requirements
- Returns data, true effect functions, and metadata

### 2. `margot_simulate_test_data_flip()`

- Companion function for testing flipped outcomes
- Creates reversed versions of specified outcomes (suffix “\_r”)

### 3. `margot_causal_forest_dev()`

- Clean architecture with 50/50 train/test split by default
- Supports evaluation forests for test set DR scores (TODO: fix
  implementation)
- Handles missing data: complete cases, imputation, or let forest handle
- Toy data sampling for quick iteration
- Returns organized results with all components needed for analysis

### 4. `margot_qini_dev()`

- Integrates QINI computation and interpretation (no separate interpret
  function)
- Uses test set for honest evaluation
- Multiple baseline methods: maq_no_covariates, simple, maq_only, auto,
  none
- Bootstrap inference for confidence intervals
- Comprehensive gain summaries at specified spend levels
- Returns curves, objects, summaries, and metrics in one structure

### 5. `margot_plot_qini_dev()`

- Handles both single and multiple models (no separate batch function)
- Scale options: average (default), cumulative, population
- Flexible faceting for multiple outcomes/models
- Spend markers and confidence intervals
- Enhanced styling and labeling

### 6. Additional plotting functions:

- [`margot_plot_qini_annotated_dev()`](https://go-bayes.github.io/margot/reference/margot_plot_qini_annotated_dev.md) -
  Adds gain annotations at spend levels
- [`margot_plot_qini_compare_methods_dev()`](https://go-bayes.github.io/margot/reference/margot_plot_qini_compare_methods_dev.md) -
  Compares different baseline methods

## Test Results

The functions were successfully tested with: - 1000 simulated
observations - Positive homogeneous effect (Y1) and heterogeneous effect
(Y2) - 50/50 train/test split - QINI curves computed and plotted
successfully - Estimated ATEs close to true values

## Known Issues

1.  Evaluation forest DR scores not yet properly implemented for test
    set
2.  Warning about vector length mismatch in baseline comparison
    (harmless)
3.  Functions need to be properly exported (add to NAMESPACE via
    roxygen2)

## File Locations

- `R/margot_simulate_test_data.R`
- `R/margot_causal_forest_dev.R`
- `R/margot_qini_dev.R`
- `R/margot_plot_qini_dev.R`

## Next Steps

1.  Run
    [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
    to export functions
2.  Continue with high-priority development functions:
    - `margot_hetero_dev()`
    - `margot_policy_tree_dev()`
    - `margot_rate_dev()`
    - `margot_flip_forests_dev()`
3.  Fix evaluation forest implementation for proper DR scores
4.  Add unit tests for all functions
5.  Create vignette demonstrating new workflow

## Usage Example

``` r

# Generate test data
test_data <- margot_simulate_test_data(n = 1000, seed = 123)

# Fit forests
cf_results <- margot_causal_forest_dev(
  data = test_data$data,
  outcome_vars = c("Y1", "Y2"),
  treatment = "A"
)

# Compute QINI
qini_results <- margot_qini_dev(cf_results)

# Plot
plot <- margot_plot_qini_dev(qini_results, scale = "cumulative")
```
