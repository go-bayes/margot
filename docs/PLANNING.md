# Development Architecture Planning

## Overview

This document outlines the new development architecture for margot
functions, moving from a monolithic approach to a modular, cleaner
design with proper train/test separation.

## Phase 2 Plan: Transformation Metadata in Causal Forest Models

### Overview

Currently, displaying CATEs on the original data scale requires passing
`original_df` to interpretation functions. Phase 2 will embed
transformation metadata directly in the model objects, making the
process more robust and eliminating the need for external data.

### Implementation Plan

#### 1. Update margot_causal_forest() and margot_flip_forest()

Add transformation detection and metadata storage:

``` r

# When save_data = TRUE, also save transformation info
if (save_data) {
  output$transformation_info <- detect_transformations(
    outcome_vars = outcome_vars,
    data = data,
    covariates = covariates
  )
}
```

#### 2. Transformation Detection Function

Create `detect_transformations()` to analyse outcome variables:

``` r

detect_transformations <- function(outcome_vars, data, covariates) {
  transform_info <- list()
  
  for (outcome in outcome_vars) {
    # Detect transformation type from name
    has_z <- grepl("_z$", outcome)
    has_log <- grepl("_log_", outcome)
    
    # Find original variable
    original_var <- find_original_variable(outcome, data)
    
    # Store metadata based on transformation type
    if (has_z && has_log) {
      # Compound transformation: log then z
      transform_info[[outcome]] <- list(
        type = "log_z",
        original_var = original_var,
        original_mean = mean(data[[original_var]], na.rm = TRUE),
        original_sd = sd(data[[original_var]], na.rm = TRUE),
        log_data = log(data[[original_var]] + 1),
        log_mean = mean(log_data, na.rm = TRUE),
        log_sd = sd(log_data, na.rm = TRUE),
        log_offset = 1
      )
    } else if (has_z) {
      # Simple z-transformation
      transform_info[[outcome]] <- list(
        type = "z",
        original_var = original_var,
        original_mean = mean(data[[original_var]], na.rm = TRUE),
        original_sd = sd(data[[original_var]], na.rm = TRUE)
      )
    }
  }
  
  return(transform_info)
}
```

#### 3. Update margot_policy_tree_stability()

Preserve transformation metadata through stability analysis:

``` r

# In the output structure
output$transformation_info <- model_results$transformation_info
```

#### 4. Update Interpretation Functions

Modify
[`margot_interpret_policy_tree()`](https://go-bayes.github.io/margot/reference/margot_interpret_policy_tree.md)
to use embedded metadata:

``` r

# Check for transformation info in model
transform_info <- NULL
if (!is.null(model$transformation_info)) {
  outcome_name <- sub("^model_", "", model_name)
  transform_info <- model$transformation_info[[outcome_name]]
}

# Use transform_info for inverse transformations
# No need for original_df parameter
```

### Benefits

1.  **Self-contained models**: All necessary information travels with
    the model
2.  **Robustness**: No risk of mismatched data or missing original_df
3.  **Backwards compatibility**: Old models without transformation_info
    still work
4.  **Cleaner API**: Interpretation functions don’t need original_df
    parameter
5.  **Accurate transformations**: Metadata captured at model creation
    time

### Timeline

- Phase 1 (completed): Basic functionality with original_df parameter
- Phase 2 (planned): Embed transformation metadata in models
- Migration: Update existing workflows to use new functionality

## Completed Components

### 1. margot_simulate_test_data() ✓

- Creates consistent synthetic datasets for testing
- Supports multiple treatment effect patterns (positive, negative, null,
  heterogeneous)
- Includes missing data generation and censoring
- Has companion function margot_simulate_test_data_flip() for flipped
  outcomes

### 2. margot_causal_forest_dev() ✓

- Implements proper 50/50 train/test split by default
- Supports evaluation forests for computing test set DR scores
- Integrated missing data handling (complete, impute, or forest)
- Toy data sampling capability for quick testing
- Saves all necessary components for downstream analysis

### 3. margot_qini_dev() ✓

- Computes QINI curves on test set for honest evaluation
- Integrates interpretation functionality (no separate interpret
  function needed)
- Multiple baseline methods with automatic fallback
- Bootstrap inference for uncertainty quantification
- Comprehensive gain summaries and metrics

### 4. margot_plot_qini_dev() ✓

- Handles both single and multiple models without separate batch
  function
- Multiple scale options (average, cumulative, population)
- Faceting support for multiple outcomes/models
- Enhanced annotation capabilities
- Method comparison functionality

## Pending Components

### High Priority

#### margot_hetero_dev()

- Combines functionality of margot_interpret_heterogeneity()
- Works directly with causal forest results
- Includes visualization of heterogeneity patterns
- Subgroup discovery and testing

#### margot_policy_tree_dev()

- Integrates policy tree fitting and interpretation
- Handles multi-arm treatments
- Includes all visualization (decision trees, QINI, etc.)
- No separate margot_interpret_policy_tree needed

#### margot_rate_dev()

- Combines margot_rate() and margot_rate_batch()
- Supports both AUTOC and QINI targeting
- Works with test set for honest evaluation
- Integrated plotting capabilities

#### margot_flip_forests_dev()

- Maintains flipped forest functionality
- Adapts to new architecture
- Consistent with train/test splits

### Medium Priority

#### margot_plot_rate_dev()

- Visualizes RATE results for both AUTOC and QINI
- Handles multiple outcomes
- Consistent with plot_qini_dev design

#### margot_plot_policy_tree_dev()

- All policy tree visualizations in one function
- Decision trees, policy plots, combined views
- Handles multi-arm and binary treatments

#### margot_subgroups_dev()

- Planned subgroup analysis
- Integrates with forest results
- Multiple testing corrections

#### margot_subset_dev()

- Subset analyses on specific populations
- Maintains train/test consistency
- Proper inference for subsets

#### margot_compare_dev()

- Group comparisons
- Heterogeneity testing across groups
- Visualization of differences

#### margot_ate_dev()

- Flexible ATE computation
- Supports various estimands
- Works with evaluation forests

### Low Priority

#### margot_bind_dev()

- Combines multiple model runs
- Maintains metadata consistency
- Supports different forest specifications

#### margot_plot_tau_dev()

- Treatment effect visualization
- Individual and group-level plots
- Uncertainty visualization

## Key Design Principles

1.  **Modularity**: Each function has a clear, focused purpose
2.  **Integration**: Related functionality combined (e.g., qini +
    interpret)
3.  **Consistency**: Uniform interfaces and naming conventions
4.  **Honesty**: Proper train/test separation throughout
5.  **Flexibility**: Support for various use cases via parameters
6.  **Simplicity**: Reduced number of functions, clearer workflow

## Example Workflow

``` r

# 1. Generate test data
data <- margot_simulate_test_data()

# 2. Fit causal forests
forests <- margot_causal_forest_dev(
  data = data$data,
  outcome_vars = c("Y1", "Y2", "Y3", "Y4"),
  treatment = "A"
)

# 3. QINI analysis
qini <- margot_qini_dev(forests)
plot_qini <- margot_plot_qini_dev(qini)

# 4. Heterogeneity analysis
hetero <- margot_hetero_dev(forests)

# 5. Policy trees
policy <- margot_policy_tree_dev(forests)

# 6. RATE analysis
rate <- margot_rate_dev(forests)
```

## Migration Strategy

1.  All new functions have “\_dev” suffix to avoid conflicts
2.  Existing functions remain unchanged
3.  Documentation clearly indicates which functions to use
4.  Gradual migration as users adopt new workflow
5.  Eventually deprecate old functions

## Testing Plan

1.  Unit tests for each new function
2.  Integration tests for complete workflows
3.  Performance benchmarks vs old functions
4.  Missing data scenario testing
5.  Edge case validation

## Next Steps

Continue implementing high-priority components: - margot_hetero_dev() -
margot_policy_tree_dev() - margot_rate_dev() - margot_flip_forests_dev()

Focus on maintaining consistency with completed components and ensuring
smooth integration across the ecosystem.
