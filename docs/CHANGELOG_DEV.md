# Development Changelog

## 2025-08-02: Fixed compute_conditional_means to respect compute_marginal_only

### Fixed

- Updated
  [`margot_causal_forest()`](https://go-bayes.github.io/margot/reference/margot_causal_forest.md)
  so that `compute_conditional_means` respects the
  `compute_marginal_only` setting
- When `compute_marginal_only = TRUE`, conditional means are now skipped
  for performance
- Creates defensive empty structure with `computed = FALSE` and reason
  for pipeline compatibility

### Technical Details

- Conditional means from
  [`policytree::conditional_means()`](https://rdrr.io/pkg/policytree/man/conditional_means.html)
  are heterogeneity-related metrics
- These show expected outcomes under each treatment arm for different
  covariate patterns
- Skipping them when `compute_marginal_only = TRUE` is consistent with
  skipping RATE and QINI
- Maintains same defensive structure pattern as other skipped
  computations

## 2025-08-02: Fixed label mapping for reversed variables in margot_plot()

### Fixed

- Updated `transform_label()` in helpers.R to prioritize exact matches
  in label_mapping
- When an exact match is found, it returns immediately without further
  processing
- Added automatic removal of `_r` suffix alongside `_z` suffix for
  reversed variables
- This prevents cases where partial pattern matching would leave “\_r”
  as a stray suffix on plot labels

### Technical Details

- The issue occurred when label_mapping contained both base and reversed
  variable names
- Pattern matching could match the base pattern, leaving “\_r”
  unprocessed
- Now exact matches take precedence and are returned without
  modification

## 2025-08-02: Fixed handling of reversed variables in margot_plot()

### Fixed

- Updated `back_transform_estimates()` in helpers.R to properly handle
  reversed variables with `_r` suffix
- The function now removes the `_r` suffix before looking for variables
  in original_df
- Silently skips variables not found in original_df instead of throwing
  warnings
- This fix aligns with how `margot_interpret_policy()` and
  [`margot_interpret_policy_batch()`](https://go-bayes.github.io/margot/reference/margot_interpret_policy_batch.md)
  handle reversed variables

## 2025-01-25: New Development Architecture Implementation

### Overview

Started implementation of new modular architecture for margot functions
with proper train/test separation and integrated functionality.

### Completed Functions

#### margot_simulate_test_data()

- Creates consistent synthetic datasets with known treatment effects
- Supports positive, negative, null, and heterogeneous effect patterns
- Includes missing data generation and censoring indicators
- Companion function margot_simulate_test_data_flip() for testing flip
  forests
- Located in: R/margot_simulate_test_data.R

#### margot_causal_forest_dev()

- Replaces margot_causal_forest() with cleaner architecture
- Default 50/50 train/test split (configurable)
- Optional evaluation forests for test set DR scores
- Integrated missing data handling: complete cases, imputation, or
  forest-based
- Toy data sampling for quick testing (toy_data_prop parameter)
- Saves all components needed for downstream analysis
- Located in: R/margot_causal_forest_dev.R

#### margot_qini_dev()

- Combines margot_qini() and margot_interpret_qini() functionality
- Uses test set for honest QINI evaluation
- Multiple baseline methods with automatic fallback
- Bootstrap inference for confidence intervals
- Comprehensive gain summaries at specified spend levels
- Returns integrated results with metrics and interpretation
- Located in: R/margot_qini_dev.R

#### margot_plot_qini_dev()

- Replaces both margot_plot_qini() and margot_plot_qini_batch()
- Intelligently handles single or multiple models
- Scale options: average (default), cumulative, population
- Faceting support for multiple outcomes/models
- Enhanced annotation capabilities
- Method comparison functionality
- Located in: R/margot_plot_qini_dev.R

### Test Script

- Created test_dev_functions.R demonstrating the new workflow
- Shows data generation, forest fitting, QINI analysis, and plotting
- Includes examples of different scales and baseline method comparisons

### Documentation

- Created PLANNING.md with full architecture overview
- Lists completed and pending components
- Outlines design principles and migration strategy

### Key Improvements

1.  **Separation of Concerns**: Forest fitting separate from analysis
    functions
2.  **Honest Evaluation**: Consistent use of test set throughout
3.  **Integrated Functionality**: Related features combined (e.g.,
    QINI + interpretation)
4.  **Simplified API**: Fewer functions with clearer purposes
5.  **Better Missing Data Support**: Multiple handling strategies
6.  **Reproducibility**: Consistent seed handling and data generation

### Next Steps

- Implement margot_hetero_dev() for heterogeneity analysis
- Create margot_policy_tree_dev() with integrated visualization
- Develop margot_rate_dev() combining rate functionality
- Add margot_flip_forests_dev() for the new architecture

### Breaking Changes

None - all new functions use “\_dev” suffix to maintain compatibility

### Bug Fixes

N/A - New implementation

### Performance

- Toy data sampling allows quick iteration during development
- Evaluation forests computed only when needed
- Efficient train/test splitting
