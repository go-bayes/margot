# margot NEWS

## Changes in version 1.0.150

### New features
* Added `compute_rate_on_demand()` helper function for flexible RATE computation
* Enhanced `margot_plot_rate()` to support on-demand RATE computation from causal forests
* Added `target` parameter to `margot_plot_rate_batch()` to choose between AUTOC and QINI
* Added `q` parameter to `margot_rate()` for custom quantile grids
* Added `use_evaluation_subset` parameter to properly use test indices for validation

### Parameter changes
* **BREAKING**: Changed `qini_train_prop` to `train_prop` in `margot_causal_forest()` 
  - Default changed from 0.7 to 0.5 for better train/test balance
  - `qini_train_prop` is now deprecated but still works with a warning
* Added validation warnings when RATE is computed without proper test/train splits

### Improvements
* RATE functions now automatically use test indices from `qini_metadata` when available
* Better validation messages to help users understand when results may be optimistic
* Consistent handling of out-of-sample evaluation across the pipeline

### Backward compatibility
* Existing results from `margot_causal_forest()` work with all new features
* The deprecated `qini_train_prop` parameter still works but shows a deprecation warning
* Users only need to re-run `margot_causal_forest()` if they want proper validation and previously used `qini_split = FALSE`