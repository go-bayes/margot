# QINI Implementation Summary

## Overview

We’ve created a new QINI implementation for the margot package that
follows GRF/MAQ methodology exactly.

## Key Functions Created

### 1. `margot_qini_alternative()` (in R/margot_qini_alternative.R)

- Main function to compute QINI curves
- Works with existing margot_causal_forest output structure
- Requires `save_data = TRUE` (but not save_models)
- Uses test set when train/test split was used
- Computes both CATE curve and no-priority baseline

### 2. `margot_plot_qini_simple()` (in R/margot_plot_qini_simple.R)

- Creates ggplot2 visualizations of QINI curves
- Shows CATE vs baseline with optional confidence intervals
- Works with the updated results structure

## Key Design Decisions

1.  **No complex baseline methods** - Only one baseline:
    `target.with.covariates = FALSE` (no-priority assignment)
2.  **Consistent with GRF** - Uses exact same methodology as GRF example
3.  **Test set usage** - Automatically uses test set when train/test
    split was performed
4.  **Updates existing structure** - Updates
    results$`model_name`$qini_objects and qini_data

## Usage Example

``` r

# Run causal forest with required parameters
cf_results <- margot_causal_forest(
  data = mydata,
  outcome_vars = c("outcome1", "outcome2"),
  covariates = covariates,
  W = treatment,
  save_data = TRUE,  # Required
  use_train_test_split = TRUE
)

# Compute QINI curves
cf_results <- margot_qini_alternative(cf_results)

# Plot results
plot <- margot_plot_qini_simple(
  cf_results,
  model_name = "model_outcome1",
  show_ci = TRUE
)
```

## Technical Notes

1.  The function uses IPW scores computed via
    [`maq::get_ipw_scores()`](https://rdrr.io/pkg/maq/man/get_ipw_scores.html)
2.  CATE curve uses individual treatment effects for targeting
3.  Baseline represents random allocation (no prioritization)
4.  Different path lengths between CATE and baseline are handled
    properly

## Next Steps

1.  Rename
    [`margot_qini_alternative()`](https://go-bayes.github.io/margot/reference/margot_qini_alternative.md)
    to
    [`margot_qini()`](https://go-bayes.github.io/margot/reference/margot_qini.md)
    after testing
2.  Update existing
    [`margot_plot_qini()`](https://go-bayes.github.io/margot/reference/margot_plot_qini.md)
    to use new structure
3.  Update
    [`margot_interpret_qini()`](https://go-bayes.github.io/margot/reference/margot_interpret_qini.md)
    for simplified approach
4.  Update documentation and vignettes

## Testing

The implementation has been tested against GRF reference implementation
and produces similar results (differences due to random seeds in
train/test splits).
