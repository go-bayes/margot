
# To do

* Begin writing tutorials for use cases.
* fix decision tree layout
* write examples
* remove arrow from "margot_plot_policy_tree()"
* improve interpretations in `margot_compare_groups()`
* margot_screen_models - develop use_boot option
* Get flipped outcome labels into rate graphs 

## [2025-07-20] margot 1.0.102

### Policy Tree Enhancements
- **Decision tree title clarification**:
  - Enhanced `margot_plot_decision_tree()` to append "Outcome" to titles
  - Example: "Neuroticism" now displays as "Neuroticism Outcome"
  - Clarifies that the tree shows policy decisions for the outcome variable

### Label Improvements
- **Reversed outcome formatting**:
  - Updated `margot_reversed_labels()` to prepend rather than append "(reduced)"
  - Changed from "{outcome} (reduced)" to "(reduced) {outcome}"
  - Example: "Anxiety (reduced)" now displays as "(reduced) Anxiety"
  - Improves readability and understanding of flipped outcomes

## [2025-07-20] margot 1.0.101

- **new**: margot_censor_lead() - allows for conditional censoring. 

## [2025-07-20] margot 1.0.100

### Policy Tree Enhancements
- **Policy tree interpretation improvements**:
  - Modified `margot_interpret_policy_batch()` to remove redundant "Policy tree analysis results:" line
  - Added "at the end of study" to all Findings headings for temporal clarity
  - Added "baseline" prefix to all variable names in tree splits and leaf descriptions
  - Clarifies that splits are based on baseline characteristics, not outcomes
  - Simplified CATE reporting to just state values without subjective interpretation
  - More appropriate for standardized outcomes where negative values are common

- **Policy tree plotting updates**:
  - Enhanced `margot_plot_policy_tree()` to add "(baseline)" to all axis labels
  - Added "(baseline)" to plot titles and subtitles for clarity
  - Updated both depth-1 and depth-2 plots for consistency
  - Ensures users understand splits are made on baseline measures
  - Enhanced `margot_plot_decision_tree()` to append "Outcome" to outcome titles
  - Example: "Neuroticism" now displays as "Neuroticism Outcome"

### Label Transformation Improvements
- Updated `transform_var_name()` helper function:
  - Now removes "_l" suffix from variable names
  - Properly capitalizes NZSEI and NZDEP acronyms
  - Example: "t0_nzsei_13_l" now displays as "NZSEI 13 (baseline)"
- Updated `margot_reversed_labels()` formatting:
  - Changed from "{outcome} (reduced)" to "(reduced) {outcome}"
  - Example: "Anxiety (reduced)" now displays as "(reduced) Anxiety"


## [2025-07-19] margot 1.0.90

### new functions
- added `margot_assess_overlap()` for evaluating propensity score overlap
  - shows common support regions for treatment validity
  - generates propensity score distribution plots by treatment group
  - uses `transform_label` helper for automatic label formatting
  - includes `text_summary` output suitable for dropping into documents
  - added `theme` parameter for ggplot2 theme customization
  - updated colors to package standards: treatment (#d8a739), control (#4f88c6)
  - enhanced text summary with interpretation of overlap meaning

### enhanced Qini analysis
- **margot_interpret_qini()** improvements:
  - new `spend_levels` parameter for custom spend level analysis
  - new `concise_summary` output groups outcomes by benefit/harm/no effect
  - added explanatory text about CATE and Qini curves (`include_intro` parameter)
  - auto detection of available spend levels with fallback handling
  - warnings when requested spend levels don't exist in data

- **margot_plot_qini()** enhancements:
  - new vertical dashed lines at spend levels with text annotations
  - replace multiple label parameters with single `label_mapping`
  - added `theme` parameter matching margot_assess_overlap
  - spend level lines now labeled (e.g., "20% spend", "50% spend")
  - smart label positioning

### API Consistency
- renamed `spend` to `spend_levels` in `margot_policy()` and `margot_batch_policy()`
  - Ccnsistent with `margot_interpret_qini()` parameter naming
  - **breaking change**: Update code using `spend` parameter

### Bug Fixes
- fixed `margot_policy()` and `margot_batch_policy()` to pass spend_levels to Qini plots
  - Qini plot vertical lines now match the spend levels used for calculations
  - previously always showed 20% and 50% regardless of parameter

## [2025-07-19] margot 1.0.80

- Added experimental support for conditional means computation via `policytree::conditional_means()`
- New `compute_conditional_means` parameter in `margot_causal_forest()` (default TRUE)
- Conditional means are stored in model results when computed
- Enhanced `margot_interpret_policy_tree()` to include conditional means interpretation
  - Shows expected outcomes under each treatment within policy tree leaves
  - Computes average treatment effects within each leaf
  - New `include_conditional_means` parameter (default TRUE)
  - Changed default output to clear, simple language (new `use_math_notation` parameter, default FALSE)
  - Added `output_format` parameter: "bullet" (default) or "prose" for flowing narrative text
  - Enhanced leaf output with sample sizes, percentages, and effect interpretations
  - Removed uncertainty quantification disclaimer
- `margot_flip_forests()` completely rewritten to create new models with "_r" suffix
  - Properly recomputes all statistics (ATE, RATE, E-values, policy trees, QINI)
  - Flips conditional means columns for binary treatment
  - Removes original models and rebuilds combined_table
- New `margot_recalculate_policy_trees()` function for customizing policy tree covariates
  - Multiple covariate modes: "original", "custom", "add", "all"
  - Regex-based covariate exclusion support
  - Automatic covariate expansion for depth-2 trees
  - Parallel processing capability
- Updated `margot_reversed_labels()` to handle new "_r" suffix convention
  - Creates new label entries with "_r" suffix by default
  - Maintains backward compatibility with `use_r_suffix = FALSE`
  - Option to remove original entries with `remove_original = TRUE`
- Removed `margot_flip_forests_dev()` as functionality now available in `margot_recalculate_policy_trees()`

## [2025-07-17] margot 1.0.70
- `margot_flip_forests_dev()` allows custom policy trees 

## [2025-06-18] margot 1.0.65
- `margot_plot_policy_tree()` better graphs
  - Shape + Color coding - Points use both different shapes AND colors for better distinction
  - Clean masking - Shaded regions properly filter out points (no more overlapping mess)
  - Visible annotations - Split values appear as readable horizontal text near the dashed lines
  - Proper positioning - Annotations stay within plot boundaries

## [2025-06-18] margot 1.0.64
- `margot_plot_categorical()` handles binary data

## [2025-06-09] margot 1.0.63
- developing simulation within margot

## [2025-06-09] margot 1.0.62

## New
- Complete refactor of `margot_simulate()` with clearer data generation process
- Added support for heterogeneous treatment effects in simulations
- Enhanced vignette with comprehensive examples and power analysis templates

## Improved
- Fixed syntax errors in vignettes (removed numeric underscores)
- Improved test structure with cleaner helper functions
- Better documentation of simulation parameters and options

## Removed
- Removed `crayon` package dependency (down to 51 dependencies)
- Removed `gt` and `gtsummary` package dependencies
- Updated `margot_summary_panel()` to output markdown tables by default

# [2025-06-04] margot 1.0.61
- added simulation function `magot_simulate()` + units tests plus vignette

# [2025-06-04] margot 1.0.60 
- fixed namespace issue/ margot_model_evalue now exported. 

# [2025-06-04] margot 1.0.59
- removing 'export' from purely internal functions/ tidying

# [2025-06-03] margot 1.0.58
- `margot_plot()` when data are corrected using e.g. bonferroni the table is also corrected -- so the interpretation text and interpretation table match. 

# [2025-05-30] margot 1.0.57

- `margot_make_tables()` better handling in MS Word through flextable integration

# [2025-05-29] margot 1.0.56

- `margot_make_tables()` now renders variables with hyphens correctly. 


# [2025-05-28] margot 1.0.55
## new
- `margot_plot_tau()` - totally ehanced so that (a) zero is always shown (b) colour matches sign

# [2025-05-26] margot 1.0.54
## new
- `margot_plot_tau()`  creates a faceted grid of histograms showing the distribution of tau hat (individual treatment effects) for multiple models. the range is standardised across all facets to facilitate comparison. Useful for qualitatively displaying heterogenity of causal effects. 
## improved 
- `margot_plot()` three paragraph skips before the list

# [2025-05-25] margot 1.0.53
## improved 
- `margot_plot_policy_tree_depth2()`, `margot_plot_policy_combo()` - plotting enhancements to 1L and 2L trees (note Rstudio will sometime make panels gray -- just a quirk -- not an issue for final graph project.)


# [2025-05-19] margot 1.0.52
## improved 
- `margot_interpret_rate()` user may specify flipped outcome labels 

# [2025-05-18] margot 1.0.51

## improved 
- `margot_interpret_qini()` improved to select reliable Qini models

# [2025-05-18] margot 1.0.50
- `margot_interpret_rate_comparison()` exported.

# [2025-05-18] margot 1.0.49
## fixed
- `margot_interpret_rate()` - exported

# [2025-05-18] margot 1.0.48
## new
- `margot_reversed_labels()` -- helper that gets display labels and marks reversed outcomes (for policy trees)
- `margot_interpret_rate()` (`margot_interpret_rate_comparison()`)- now lists models that are not ruled out by the qini and autoc rate analysis.

# [2025-05-17] margot 1.0.47
## improved
- `margot_plot_create_options()` defaults for correcting for multiple comparisons,
  `adjust = "bonferroni"`, `alpha = 0.05`, #<- new
- `margot_planned_subgroup_batch()` and `margot_subset_batch()` -- numerous enhancements for clear reporting 
- `margot_compare_groups()` enhanced reporting


# [2025-05-16] margot 1.0.46
## improved
- `margot_process_longitudinal_data_wider()` correctly creates "NA" value for censoring indicator when unit is lost to follow up. (under development, defaults to older version, which works fine)

# [2025-05-15] margot 1.0.45
# improved
- `margot_rate()` - allows a subset of models to be passed, allows for adjustment for multiple comparisons, reflected in reporting.(`margot_interpret_rate()` and `margot_interpret_rate_comparison()`) both modified. 
- `margot_adjust_policy_p()` - extends methods
- `margot_plot_rate()` takes labels for outcomes.
- `margot_plot_rate_batch()` also takes labels for outcomes

# [2025-05-15] margot 1.0.44
- `margot_plot()` only uses correction method if there is more than one outcome. 
- `margot_interpret_marginal()` also updated. 

# [2025-05-15] margot 1.0.43
- `margot_compare_groups()` computation for relative risk fixed. 
- `margot_plot()` no passes to `margot_correct_combined_tables()` automatically (no need for a separate call)

# [2025-05-15] margot 1.0.42
## changed
- `margot_process_longitudinal_data_wider` fixed error in warnings that was causing the function to fail when ordinal data are passed. 


# [2025-05-14] margot 1.0.41
## New
- `margot_correct_combined_table()` does family–wise-error correction for multiple outcomes (ATE)
- `margot_add_policy_values()` - attach policy values to forest models (utility)
- `margot_collect_policy_values()` -- gather policy values from forest models
- `margot_screen_models()` select models for policy-tree analysis
- `margot_summarise_all()` summarise (family-wise) corrected analysis for policy trees
-  `margot_adjust_policy_p()
## changed
- `margot_rate()`, and `margot_rate_batch` have deterministic seeds for reproducible results
- `margot_plot` has two parmaters for `adjust` and `alpha` to describe whether family-wise adjustment has been made. 
- `margot_bind_table` now has default e_val_bound_threshold = 1.1 (previously 1, which is arguably too liberal).
- `margot_adjust_policy_p()` - family wise corrections
- `margot_add_policy_values_batch()` - batch add policy values to models
- `margot_report_policy()` - succinct reporting of corrected policy values.

# [2025-05-13] margot 1.0.40
- exported `margot_causal_forest_parallel()`

# [2025-05-13] margot 1.0.39

## New
- `margot_causal_forest_paralle()`  parallel implementation of `margot_causal_forest`. 
- `margot_flip_forests_parallel()` parallel implementatino of `margot_flip_forests()`

## improved
- `margot_process_longitudinal_data_wider()` - removes otios warnings from fastDummies.
- `margot_bind_tables()` - now accepts a dataframe and will work with single outputs. 

# [2025-05-12] margot 1.0.38
## improved
- `margot_impute_carry_forward()`
  - eligibility now requires an observed value in the **current** or a following wave, rather than only in a future wave.  
  - The baseline wave (`t0_`) is always checked and reported --even when no later waves exist—preventing silent skips.  
  - Internal check now uses  
    ```r
    cols_check <- c(col, future_cols)
    ok         <- rowSums(!is.na(out[, cols_check, drop = FALSE])) > 0
    ```  
    to align behaviour with the documentation.
- `margot_wide_impute_machine()` print flags now set to true
- `margot_wide_impute_baseline()` soft deprecated
- `margot_wide()` soft deprecated

# [2025-05-07] margot 1.0.37
- `margot_policy_tree_combo()` now exported

# [2025-05-07] margot 1.0.36
- `margot_policy_tree()` level 2 fixed

# [2025-05-06] margot 1.0.35
## improved
- `here_save()` and  `here_save_qs()` now correctly reports size of saved file.
-  tidying of roxogen2 code ('mc_test' removed and replaced with 'result_object')
- `margot_plot_policy_tree()` nicer label placement.

# [2025-05-04] margot 1.0.34
## fixed
- restored `margot_interpret_qini()`

# [2025-05-04] margot 1.0.33
## fixed
- namespace issues for `margot_plot_policy()` and `margot_rescue_qini()`

# [2025-05-04] margot 1.0.32
## New 
- `margot_planned_subgroup_batch()` overhauled to work with `margot_plot()` updates. 
-  restored decision tree plot results labelling
-  restored margot_interpret_marginal cutpoint results on datascale


# [2025-05-04] margot 1.0.31
## New 
- `margot_get_labels()` utility function for fetching labels. 

# [2025-05-03] margot 1.0.30
- `margot_plot_decision_tree()`- colours robustly match decisions 'control', 'treat'
- internal helpers `transform_label()`, and `transform_var_name()` labelling handles SDO and RWA correctly, also `get_original_value_plot()` and `get_original_var_info()`

# [2025-05-03] margot 1.0.29
## New
- `margot_rate_batch()` - Two allocation rules are available: **"treat_best"**  (default) ─ administer treatment to those with the largest predicted benefits; **"withhold_best"** ─ *remove* treatment from that same subgroup, useful when the intervention harmful. The CATE vector is flipped (multiplied by −1) *after* model fitting and *before* calling `grf::rank_average_treatment_effect()`.  Outcome direction flips performed earlier by `margot_flip_forests()` are thus kept conceptually separate from the policy flip implemented here.
-  removed `add_resistance`

## Refactored to accept 1L depth policy trees 👍
- `margot_plot_policy_tree()` will plot a tree of depth = 1L. 
- `margot_recalculate_policy_trees()`
- `margot_flip_forests()`
- `margot_interpret_policy_batch()`
- `margot_interpret_policy_tree()`
- `margot_interpret_rate()`
- `margot_plot_decision_tree()`
- `margot_plot_policy_combo()`
- `margot_policy()`


# [2025-05-02] margot 1.0.28
## New 
- `margot_rate()` now computes resistance to exposure (if requested) using `add_resistance`

# [2025-05-02] margot 1.0.27
## New 
- `margot_flip_forests()` automatic recomputing of the policy trees by default.
- `margot_inspect_qini()` helper to evaluate extreme propensity scores from grf models. 
- `margot_rescue_qini()` (developing) - to recompute qini data for grf models when `qini_objects` are null / empty.

## Improve
- `margot_bind_models()` -- accommodates changes

# [2025-05-02] margot 1.0.26
## Improved
- `margot_wide_machine()` now handles allows for `imputation_method` = "none". new parameters `extend_baseline` effectively pivots all date from long to wide, and allows for time indexing of columns - userful for `grf` models. 

# [2025-05-02] margot 1.0.25
## Improved
- `margot_plot_shift()` and `margot_plot_categorical` more tightly integrated. Both now plot the mean/median and flexibly plot sds.  (Both are now in the `margot_plot_categorical.R` script).

# [2025-05-01] margot 1.0.24
## Improved
- `margot_bind_models()` works with `lmtp` batched outputs with more than two conditions.

# [2025-04-30] margot 1.0.23
## Improved
- `margot_plot()` interpretation tracks user specified E-value

# [2025-04-30] margot 1.0.22
## NEW
- `margot_trim_sample_weights()`: standardises the (possibly trimmed) weights to have a mean of 1.  Missing values are preserved.

# [2025-04-29] margot 1.0.21
## Improved
- `margot_plot_rate_batch()` user can pass specific model names 
- `margot_rate_comparision()` outputs correct model names (and never statistically significant negative results.)

# [2025-04-28] margot 1.0.20
## Improved
- `margot_rate()`, `margot_rate_interpret()` now output model names for reliable result estimates. 

# [2025-04-21] margot 1.0.19
## Improved
- `margot_bind_table()` flexibly modify column names 
- `margot_plot()` - added options to transform colname for the estimand to "ATE", also the E_value colnames. 
- `margot_interpret_marginal()` and `group_tab()` now return order consistent with the plot
- `margot_interpret_rate()` better language, `margot_interpret_rate_comparison()` now a helper (not exported).
- `margot_plot_policy_tree()` and `margot_policy` now defaults to shading out regions where there is no interest.


# [2025-04-21] margot 1.0.18
## Improved
- `margot_log_transform_vars()` handles edge cases better by (1) requiring numeric input (2) requiring no NAs.

# [2025-04-21] margot 1.0.18
## Improved
- `margot_plot_categorical()` robust to wider range of data values

# [2025-04-20] margot 1.0.17
## Improved
- `margot_lmtp_tab()` -- fixes breaking change introduced by `lmtp v1.50`


# [2025-04-20] margot 1.0.16
## Improved
- `margot_transition_table()` -- allows filtering by observation/censoring variable, can be useful where data are entirely missing from certain waves.
- `margot_bind_tables()` -- correct output retaining outcome names
- `margot_impute_carry_forward()` -- better error handling/ if observation is seen in a year, may impute forward. 

# [2025-04-19] margot 1.0.15
## Improved
- `margot_interpret_marginal()` - clearer wording, outcomes listed rather than dull sentences. 

# [2025-04-19] margot 1.0.14
## Improved
- `margot_bind_tables()` has additional parameter,  `sort_E_val_bound = c("none", "asc", "desc")`, which allows users to order multiple tables by E_value thresholds. 
-  fixed multiple functions to get assending order for `margot_plot()`, these are `margot_plot()`, `group_tab()`, `margot_interpret_marginal()` `transform_label()`, `transform_table_rownames()` -- and removing stray `transform_label()` label functions from various functions. `transform_table_rownames()` now internal. 


# [2025-04-10] margot 1.0.13
## Improved
- `margot_bind_tables()` streamlined the function by focusing on markdown and LaTeX outputs. reduced dependencies by eliminating reliance on dplyr, stringr, and cli packages. enhanced markdown output with bold variable names for better readability, improved LaTeX output with better multi-page support and alternating row colors, maintained all core functionality.
- `margot_omnibus_hetero_test()` - considerably improved for clarity.

# [2025-04-10] margot 1.0.12

## Improved
- `margot_bind_models()` now bolds reliable results in markdown
- `margot_interpret_rate()` and `margot_interpret_rate_comparison()` more accurate reporting and deal with edge cases (no RATE reliable) better. 

# [2025-04-09] margot 1.0.11

## New
- `margot_bind_models()` - bind model outputs (currently implemented for batch caual forest models)
- `margot_interprete_rate_comparison()` compare qini and auto RATE estimates, also called by `margot_interpret_rate()`
- `margot_bind_tables()` - better markdown

## Improved
- `margot_interpret_rate()` combines qini and rate estimates.
- `margot_rate()` output better worded, still reate "rate_result" for backward compatibility; table only highlight "significant" if RATE is positive.
- `margot_interpret_qini()` - better outputs


# [2025-04-07] margot 1.0.10
## Improved
- `margot_causal_forest()` consistent use of training proportion rather than mixing training/test. 

# [2025-04-07] margot 1.0.9
## Improved
- `margot_causal_forest()` reverted --enhancements failing

# [2025-04-07] margot 1.0.8
## Improved
- `margot_causal_forest()` added checks

# [2025-04-07] margot 1.0.7
## Improved
- `margot_plot_decision_tree()`, `margot_plot_policy_tree()` -- enhancements


# [2025-04-07] margot 1.0.7
## Improved
- `margot_interpret_qini()` language improved for clarity.

# [2025-04-06] margot 1.0.6
## Improved
- `margot_interpret_policy_tree()` language improved for clarity.

# [2025-04-06] margot 1.0.5
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` and `margot_interpret_qini()` more aggressive error handlings

# [2025-04-06] margot 1.0.4
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` even better with LaTeX.

# [2025-04-06] margot 1.0.3
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` play better with LaTeX. Use "statistically reliable" in place of statistically significant. 


# [2025-04-06] margot 1.0.2
## Improved
- `margot_interpret_qini()` and `margot_interpret_rate()` improved warnings for negative estimates

# [2025-04-06] margot 1.0.1
## Fixed
- restored accidentally deleted helpers
- `margot_omnibus_hetero_test()` now works as it should

# [2025-04-05] margot 1.0.0
## Improved
-  we are now using semantic versioning. 
-  `margot_omnibus_hetero_test` now has label mapping

# [2025-03-26] margot 0.3.3.3
## Improved
- `margot_censor()` - explicit call for 'cli' to avoid conflict with crayon 
- `create_ordered_variable()` - removed crayon
- `margot_summary_tables_cat()` - deleted this experimental table
- `margot_log_transform_vars()` removed crayon, made all function calls explicit, removed `require` for packages.
- `margot_summary_tables()` - remove `require` and explicit function calls now.
- `causal_contrast_engine()` - refactored - removes `require`
- `margot_wide_machine()` - removes `require`
- `margot_wide()` - removes `require`

# [2025-03-26] margot 0.3.3.2
## Improved
- `margot_causal_forest()`: we now default to honest splitting for computing qini curves.
- `margot_plot_categorical()`: fixed error in handling of binary variables.
- `margot_bind_tables()` - nicer printout, user can change column names.


# [2025-03-22] margot 0.3.3.1
## Improved
- `margot_causal_forest()`- correct calculation of qini ate (reverted to older function)

# [2025-03-21] margot 0.3.3.0
## Improved
- `margot_causal_forest()`- refactored to save qini_data and qini_objects for each model. (was not working before).
- `margot_multi_arm_causal_forest()` - refactored.

# [2025-03-21] margot 0.3.2.9
## Improved
- `margot_causal_forest()` now passes doubly robust scores to policy decision trees. We now compute policy trees using doubly robust (DR) scores from \texttt{grf::get_scores()} rather than the previous IPW-based approach. DR scores combine outcome and propensity estimates, yielding improved statistical power and more efficient estimation of treatment effects. This change is backward compatible -- no modifications to user code are required -- and a note is printed during execution indicating that DR scores are used.

# [2025-03-21] margot 0.3.2.8
## Improved
- `margot_interpret_qini()` prints out more economical explanation, only giving explanations for reliable results.

# [2025-03-21] margot 0.3.2.7
## New
- margot_bind_tables(): binds results from multiple out-comewide studies into one table

# [2025-03-21] margot 0.3.2.6
## New
- `margot_planned_subgroups_batch()` -- batch the batched analysis of planned subgroups analysis for causal forests (time-saver).
- `backtransform_log_z()` utility function to backtranform scores from logged values to original data values. 

## Improved
- `margot_subset_batch()` now working as it should for complex conditions.

# [2025-03-21] margot 0.3.2.5
## Improved
- `margot_process_binary()`: if a variable already ends with "_binary" it is not processed again as binary. Also removed dependency on the `color` package.
- `coloured_histogram_shift()`... and other deprecated functions no longer exported or maintained.
- `margot_plot_shift()` - improved to print mean value and also to show +/- sd of distribution
- `margot_causal_forest()` - default parameter is 0.5 training. 

# [2025-03-19] margot 0.3.2.4
## Improved
- `margot_subset_batch()` - now correctly passes all parameters required by `margot_plot()`. 
- `margot_plot()` - default is now: interpret_all_E_gt1 = TRUE (all results with non-null Evalues reported).


# [2025-03-19] margot 0.3.2.3
## Improved
- `margot_subset_batch()` - now exports plots and explanations for all submodels models (causal forests).

# [2025-03-19] margot 0.3.2.2
## New
- `margot_subset_batch()` batch process subsetted models (causal forests)
- `margot_plot_rate_batch()` replaces `margot_plot_batch_rate()`, which is deprecated. 

## Improved
- `margot_subset_model()` - streamlined


# [2025-03-19] margot 0.3.2.1
## Improved
- `group_tab()` now only in helper functions
- `margot_plot()` reverted to remove errors.
- `margot_compare_groups()`, `margot_interpret_qini()` removed redundant function.


# [2025-03-19] margot 0.3.2.0
## New 
- added `.strict_exposure_outcome_censoring()` helper function for strict dyadic censoring -- now encompassed by `margot_process_longitudinal_data_wider()` but added for backward compatibility.

## Improved
-  `coloured_histogram()`, `margot_impute_carry_forward()`, `margot_process_longitudinal_data_wider()`, `margot_lmtp_evalue()`,`margot_plot_batch_rate()`, `margot_plot_boxplot()`, `margot_plot_boxplot_covariate()`,  `margot_plot_discontinuity()`, `margot_plot_histogram()`, `margot_plot_slope_covariate_batch()`, `margot_plot_slope_covariate_combo()`, `margot_plot_slope_covariate()`, `margot_plot_slope()`, `margot_plot_individual_responses()`, `margot_wide_machine()`, `margot_process_longitudinal_data()`, `margot_count_dyads()`, `margot_count_ids()` correct package calls, explicit namespace calls.
- `group_tab()` function re-factored
- `margot_interpret_marginal()` corrected order & small improvements
- `margot_plot()` function refactored for clarity.


# [2025-03-18] margot 0.3.1.9

## Improved

- `here_save()` removed the call to base R
- `here_read()` removed the call to base R
- `margot_combine_results()` - made helper function
- `margot_summary_panel()` - specified functions required
- `margot_censor()` - specified functions required by data.table (#' @importFrom data.table as.data.table copy set get uniqueN)
- `read_multiple_images()` - made internal
- `margot_wide_machine()` - stated explicit functions in dependencies (removed zoo)
- `margot_interpret_qini()` - improved for reporting
- `margot_policy()` - now allows subsetting specific models
- `margot_interpret_policy_batch()` - now allows subsetting specific models
- `margot_causal_forest()` - added roxygen code (forgotten in last update). 

# [2025-03-17] margot 0.3.1.8

## New
- `margot_rate()` produce a table for RATE estimates from batched processed causal forests.
- `margot_flip_forests()` when interpreting the effect as *benefitial* we may need to invert the outcomes. This function does this on already processed models.
- `margot_interpret_rate()` provides automated reporting of rate outputs, allowing users to specify whether AUTOC or QINI was targeted. 

## Improved
- `margot_causal_forest()` - now also computes RATE targeting the QINI when this is selected. 

# [2025-03-15] margot 0.3.1.7

## Improved
- `create_ordered_variable()` appends "_binary" when binary variables are made
- `margot_transition_table()` user may specify a table name.
- `margot_process_longitudinal_wider()` new parameters so that no continuous vars are scaled

# [2025-03-14] margot 0.3.1.6
## Improved
- `margot_interpret_marginal()` made robust when when units are not given in the the input model.

# [2025-03-13] margot 0.3.1.5
## Improved
- `margot_compute_gender_weights_by_wave()` actually added (was not omitted by accident in 0.3.1.4).

# [2025-03-12] margot 0.3.1.4
## Improved
- `margot_compute_gender_weights_by_wave()` correctly assigns weights to a target population, specified by the `target_wave` parameters.

# [2025-03-12] margot 0.3.1.3

## Improved
- `margot_censor()`: censoring is conditional on both the condition and the wave: if $\texttt{censor_final_wave = FALSE}$ then rows in the final wave (determined as $\texttt{max(dt[[wave_var]])}$) are preserved (default behaviour). This allows for estimating a per-protocal effect where changing a censoring condition is part of the causal effect of the intervention for the population at baseline.

# [2025-03-05] margot 0.3.1.2

## New
 - `margot_transition_table()` replaces `transition_table()` and `create_transition_matrix()` and will print out state change for multiple waves. Useful for longitudinal modified treatment policies. (Old functions soft deprecated as helper functions.)

# [2025-02-06] margot 0.3.1.1

## Improved
- `margot_plot` and `margot_interpret_marginal` now all reporting of all coefficients if E-value is above 1 (instead of 1.1, the default threshold). 

- `margot_censor` now deletes all values of a variable if the censoring indicator is set to 1 (or year_measured == 0), instead of merely changing the indicator. This can be helpful downstream of this function when handling dyadic data.

# [2025-02-05] margot 0.3.1.0

## Improved

- `margot_process_longitudinal_data_wider` can now handle dyadic data, so that if an agent's partner is censored, then the agent is also censored.

# [2025-01-18] margot 0.3.0.9

## New
- `margot_combine_results` Takes multiple results tables from `margot_plot` objects and combines them into a single
formatted table using `kableExtra`, with optional group headers for each section.

## Improved
-`margot_plot` has option `standardize_label` which when to display Standardised, Standardized, or to remove it altogether ("none").
  - For Risk Ratios (type = "RR"), the code always omits that word and shows the label as "Effect (Risk Ratio Scale)".
	- For Risk Differences (type = "RD"), if standardize_label = "NZ" you get "Standardised Effect (Difference Scale)"; if standardize_label = "US" you get "Standardized Effect (Difference Scale)"; and if standardize_label = "none", you get "Effect (Difference Scale)".

# [2025-01-17] margot 0.3.0.8

## Improved
- `margot_plot` has a new parameter `include_coefficients` which if set to TRUE will remove the coefficients from a graph. This makes it easier to view the results. 
-  minor fix to `margot_save_png` so that it defaults to the output folder set by `push_mods`

# [2024-12-29] margot 0.3.0.7

## Improved
`margot_impute_carry_forward()` - prevents imputing variables at end of study if they are included as time-varying variables.

# [2024-12-29] margot 0.3.0.6

## Improved
`margot_impute_carry_forward()` is more efficient. Can carry forward more than one-wave if a future observation is made and 'require_one_observed' is set to "TRUE".

# [2024-12-08] margot 0.3.0.5

## New
- re-factored `margot_make_tables()` to work with the table1 package, for extending flexibility of tables (esp with latex.)

# [2024-12-07] margot 0.3.0.4

## New
- `margot_prop_missing()` - utility function to get propoportion of missing data at baseline. 

# [2024-11-26] margot 0.3.0.3

## Fixed
- `margot_amelia_to_mice()` - fixed to be unconstrained to previous workflow.

# [2024-11-11] margot 0.3.0.2

## Fixed
- `margot_plot()` label now reads "causal difference" rather than "causal risk difference".

# [2024-11-09] margot 0.3.0.1

## New
-`margot_impute_carry_forward` - carry forward last observed value (for handling 'prodigal' id's that are lost and found).

## Fixed
- `margot_wide_machine` - removed functionality now handled by `margot_impute_carry_forward` 


# [2024-11-06] margot 0.3.0.0

## New
- `margot_make_table()` - flexible longitudinal tables 
- `margot_amelia_to_mice()` - converts `Amelia` output to `mice` output. 

# [2024-10-30] margot  0.2.3.80

## New

- `margot_count_ids()` - track cumulative counts of participants, returning participants, 

## Improved
- `margot_count_dyads()` - more informative information. 

# [2024-10-30] margot 0.2.3.70
## Improved
- `margot_interpret_marginal` clean appearance

## Improved
- `margot_plot` and `margot_interpret_marginal` functions overhauled again to focus on compact reporting.


# [2024-10-29] margot 0.2.3.60

## Improved
- `margot_plot` and `margot_interpret_marginal` functions overhauled again to focus on compact reporting.


# [2024-10-29] margot 0.2.3.50
- `margot_plot` and `margot_interpret_marginal` functions

# [2024-10-29] margot 0.2.3.40

## Improved
- `margot_plot` to work with new `margot_interpret_marginal` function

# [2024-10-29] margot 0.2.3.16

## Improved
- `margot_interpret_marginal` removed cli formatting for compatibility with quarto documents

# [2024-10-28] margot 0.2.3.15

## Improved
- `margot_wide_impute` - small bug fix, na indicators not automatically created (functionality was lost at previous iteration)

# [2024-10-27] margot 0.2.3.14

## Improved
- `margot_process_longitudinal_data_wider` - more robust.
- `margot_wide_impute` - small bug fix. 

# [2024-10-26] margot 0.2.3.13


## New

- `margot_censor` create censoring indictors for correct handling of lost-to-follow-up when conditions are not met, includes a clustering variable (useful for romantic dyads)

## Improved

- `margot_wide_machine` now correctly handling multiple time points. 

# [2024-10-26] margot 0.2.3.12

## Improved

- `margot_wide_machine()` correctly handles NA values at baseline, and prints out message

# [2024-10-23] margot 0.2.3.11

## Improved

- `margot_save_png()` better defaults
- `margot_plot()` enhancements, and improved documentation


# [2024-10-23] margot 0.2.3.10
## Improved
- `margot_save_png()` flexibly handles any plot object, not merely ggplot2 objects.


# [2024-10-22] margot 0.2.3.9

## Improved
- `margot_interpret_marginal()` gives correct interpretation of 'strong' evidence using Evalues.

# [2024-10-02] margot 0.2.3.8
- `margot_process_longitudinal_data_wider()` performance enhancement. Users can save the outcome variable even if previous exposures are missing.  Useful for `lmtp` survival models

# [2024-09-27] margot 0.2.3.7 :)!


## Improved
- `margot_process_longitudinal_data_wider()` performance enhancement
- `margot_wide_machine()` simplified.  We now have a time-varying treatment workflow in place! 

# [2024-09-27] margot 0.2.3.6

## New
- `margot_wide_machine()` converts wide data to long data so using indicators for missing observations, which allows for non-parametric stacked learning in `lmtp` without multiple-imputation assumptions. Also handles more than three time-points. Optional `imputation_method = 'mice` allows users to impute, while also creating NA dummy variables for non-parametric learning.

- `margot_process_longitudinal_data_wider()` extends flexibility of `margot_process_longitudinal_data()` to more than three waves, and allows users to specify variable names. 

# [2024-09-26] margot 0.2.3.5

## New
- helper function `back_transform_estimates()` is unique for the marginal plots and marginal interpretation, to avoid confusions with back-transforming helpers for split-points in policy trees. 


## Improved
- `margot_plot()`, and `margot_interpret_marginal()` produce interpretable results. Fixed issue with `margot_plot()` when risk ratios are selected, where colours were not being plotted. 


# [2024-09-25] margot 0.2.3.4

## New
- `margot_plot_slope_covariate_combo()` - batch multiple `margot_plot_slope_covariate()` plots onto one graph using `patchwork`.


## Improved
- `margot_plot_slope_covariate()` improved for flexibility




# [2024-09-25] margot 0.2.3.3


## Improved

- `margot_plot_histogram()` now take optional `vertical_facets` parameter, allowing for more interpretable time-series graphs.
-  placed all internal function under `helpers.R` in the R directory, to avoid clutter. 


# [2024-09-25] margot 0.2.3.2


## Improved

- `margot_plot()` `margot_interpret_marginal()` now back transform values to data scale. 

# [2024-09-24] margot 0.2.3.1

## Improved

- `margot_plot_policy_tree()`, `margot_plot_decision_tree()`, `margot_interpret_policy_tree()`, `margot_plot_qini_tree()` use same global function names. New helper functions back-transform logged values (as well as z-transformed values) so that we get interpretations on the data scale for variables that have been log-transformed.  This aids with interpretation. 


# [2024-09-20] margot 0.2.3.0

## New

-  Refactored causal tree graphs and interpretations for flexible labelling and for providing both standardised results (where relevant), and results on the data scale. Makes the interpretation of policies easier to understand. 
- `margot_count_dyads()` counts dyads in a longitudinal dataset. 
- `margot_summary_panel()` summaries participants by panel wave; counts unique participants by wave, ...
- `margot_interpret_policy_batch()` interprets the policytree results. 

## Improved

- `margot_summary_tables()` - now pass multiple tables, better exposure plots.
- `margot_interpret_policy_tree()` - refactored: now returns results on data scale, better labels. 
- `margot_plot_policy_tree()` - refactored: now returns results on data scale, better labels. 



## Deprecated

- `margot_batch_policy()`

# [2024-09-19] margot 0.2.1.64

## Improved

- `margot_plot()`,`margot_interpret_marginal()`, `group_tab()` overhauled so that now we get reporting back-tranformed from standardised effects to effects on the data scale -- greatly benefiting interpretations.
- `transform_to_original_scale()` new helper introduced to back-transform estimates.

# [2024-09-18] margot 0.2.1.63

- `margot_save_png()` replaces `margot_plot_save_png()` for consistent function labelling, and to spare a burden of remembering function names.

# [2024-09-17] margot 0.2.1.62

- `margot_compare_groups()` added bold formatting to alert readers to reliable group differences



## Improved

- `margot_interpret_qini()` - now formats tables to alert readers to where prioritising results are reliably *worse* or **better** than than none.



# [2024-09-17] margot 0.2.1.60

## Improved

- `margot_plot_create_options()` updated to work with improve plotting workflow

## Deprecated

- `compute_difference()` now use the more general `margot_compare_groups()` workflow. 

## Removed 

- Removed the following deprecated functions from vignettes, instead use [https://github.com/go-bayes/boilerplate](https://github.com/go-bayes/boilerplate)
    - `boilerplate_measures`
    - `boilerplate_methods_additional_sections`
    - `boilerplate_methods`
    - `boilerplate_methods_causal_interventions`
    - `boilerplate_methods_confounding_control`
    - `boilerplate_methods_eligibility_criteria`
    - `boilerplate_methods_identification_assumptions`
    - `boilerplate_methods_missing_data`
    - `boilerplate_methods_sample`
    - `boilerplate_methods_statistical_estimator`
    - `boilerplate_methods_target_population`
    - `boilerplate_methods_variables`
    - `create_ordered_variable_custom`
    - `margot_compute_gender_weights`
    - `margot_create_bibliography`
    - `margot_create_database`
    - `margot_grf_subset_table`
    - `margot_merge_databases`
    - `manager_boilerplate_measures`
    - `compute_difference()`




# [2024-09-17] margot 0.2.1.59

## New
- `margot_plot_save_png()` saves a margot_plot output graph as a png, user can change width, heigh, dpi, and specify a path... 

## Improved
- `margot_plot()` automatic saving of the output with optional timestamps
- `margot_plot_multi_arm()` modified to work with new and improved `margot_plot()`

# [2024-09-16] margot 0.2.1.58
## New
- `margot_compare_groups()` compare treatment effects by groups and evaluate evidence for differences 

# [2024-09-16] margot 0.2.1.57

## New
- `margot_plot_multi_arm()` wrapper for `margot_plot` that enables each plots/tables for multi arm treatment models

## Improved
- reporting of multi arm treatment models in `margot_plot_qini()` is easier to follow. 
- `margot_lmtp()` now has automatic saving of models with optional prefix label and optional time-stamping. Also actually saves table when computing contrasts with only the null model. 

# [2024-09-16] margot 0.2.1.56

## Improved

- `margot_interpet_qini()` robust for both binary and multi-arm treatments. 
- `margot_plot_qini()` correct label for binarhy treatments
- `margot_batch_policy_tree()` correctly modified function added: commputes multiple 'spends'

# [2024-09-15] margot 0.2.1.55

## New

- `margot_interpet_qini()` interprets results of the qini curves at pre-specified levels. 


## Improved

- `extract_qini_data()` improved handling of `margot_multi_arm_causal_forest()`
-  numerous plot functions enhanced to produce "NZ" instead of "Nz"

# [2024-09-12] margot 0.2.1.54

## New

- `margot_summary_cate_difference_gain()` computes the difference in average gains and the integrated difference
between a reference curve (maq object) and a comparison curve at a specified spend level -- to see if there is support for CATEs

## Improved

- `compute_qini_curves_multi_arm()` - modified so that we can now get quantitative estimates for support for CATEs
- `margot_multi_arm_causal_forest()` - enhanced in several ways, for example to support `margot_summary_cate_differences()`
- `margot_causal_forest()` - likewise enhanced.
- `margot_batch_policy()` now outputs `margot_summary_cate_difference_gain() models by default 

# [2024-09-12] margot 0.2.1.53

## Improved

- `margot_summary_tables()` plots take upper case letters, remove '_'
- `margot_adjust_weights()` censored individuals are assigned zero weights, and only uncensored individuals contribute to the final analysis. 

# [2024-09-12] margot 0.2.1.52

## Improved

- Fixed `margot_plot_response_timeline()` to print dates, and to optionally save a 'png` image. 

# [2024-09-12] margot 0.2.1.51

## Improved

- `margot_plot_discontinuity()` and `margot_plot_slope()` have correct end years (+1 final wave, as waves overlap years)

# [2024-09-12] margot 0.2.1.50

## New

- `margot_plot_boxplot_covariate()` descriptive trends by groups


## Improved

- `margot_plot_slope_covariate()` automatic title, save png, and optional time stamp
- `margot_plot_individual_responses()` fixed so there is no missingness

# [2024-09-12] margot 0.2.1.49

## Improved

- reverted `compute_qini_curves` (only works with binary vars)
- `margot_causal_forest()` now working again

# [2024-09-12] margot 0.2.1.48

## Improved

- `margot_multi_arm_causal_forest()`
- `extract_qini_data()` improved to work with `margot_multi_arm_causal_forest()`

## New
- `compute_qini_curves_multi_arm()` internal function to support `margot_multi_arm_causal_forest()`



# [2024-09-11] margot 0.2.1.47

## Improved
-  Coordinated the following functions to play well with `margot_multi_arm_causal_forest()`
- `margot_plot_qini()`
- `extract_qini_data()``
- `compute_qini_curves()`

# [2024-09-11] margot 0.2.1.46

## Improved

- `hear_read` does not require that an `.rds` file is passed. 
- `extract_qini_data` made robust


# [2024-09-11] margot 0.2.1.45

## New

`build_formula_str` - helper function now documented. Used in `causal_contrast_engine` and `causal_contraset_marginal`

## Improved

- `causal_contrast_marginal` and `causal_contrast_marginal` modified to accommated breaking change in `WeigthIt` package. 
- `double_robust_marginal` checks if object is a winmids object.

# [2024-09-02] margot 0.2.1.44

## Improved

- `margot_plot_individual_responses()` now plotting all cases by default. Default `random_draws` of 100. 

# [2024-09-02] margot 0.2.1.43

## Improved

- `margot_plot_histogram()`, optional coloured mean/sd lines.

# [2024-09-02] margot 0.2.1.42

## Improved

- `margot_plot_individual_responses()`. Now handles factors, and robust to missing waves. 
- `margot_plot_boxplot()` different colours for boxplots if a single variable is passed over multiple waves. 

# [2024-09-02] margot 0.2.1.41


## New features

- `margot_plot_individual_responses()`. New function to allow random plotting of responses in a subset of the sample, useful for investigating individual trajectories of change. 
- `margot_plot_boxplot()` Now user supplies `wave` values, allowing more flexible and precise plotting of intervals. Has optional prefixes. The coordinates of the graph may be optionally flipped.



# [2024-09-02] margot 0.2.1.40

## New features

- `margot_plot_categorical()` for visualising categorical data distributions.
- `margot_plot_shift()` for visualising shifts in data distributions with highlighted ranges.

## Deprecations

The following functions have been deprecated in favor of the new functions:

- `margot_plot_hist()` is deprecated. Use `margot_plot_histogram()` instead.
- `coloured_histogram()` is deprecated. Use `margot_plot_histogram()` instead.
- `coloured_histogram_shift()` is deprecated. Use `margot_plot_shift()` instead.
 `coloured_histogram_quantiles()` is deprecated. Use `margot_plot_categorical()` instead.

These deprecated functions will continue to work but will issue warnings. They will be removed in a future version of the package.



# [2024-09-02] margot 0.2.1.39

## Improved
- `margot_plot_slope()` allows faceting


# [2024-09-02] margot 0.2.1.38

## New 
- `margot_plot_histogram()` new histogram that's more informative and more robust than previous attempts. Can be used for multiple variables and multiple waves. 

## Improved
- `margot_plot_boxplot()` made robust to single outcome in single wave. 

# [2024-09-02] margot 0.2.1.37

- `margot_plot_discontinuity()`, `margot_plot_slope()`, `margot_plot_slope_covariate()` automatically print number of unique participants and unique number of observations in the title, if no title is passed. 


# [2024-09-02] margot 0.2.1.36

## Improved

- `margot_plot_discontinuity()` now being read to namespace.

# [2024-09-01] margot 0.2.1.35

## New
- `margot_plot_slope()` descriptive trends in continuous variables over time; user may pass historical events which are denoted by dashed vertical lines on the plot.
- `margot_plot_slope_covariate()` descriptive trends by covariates over time. 
- `margot_plot_bloxplot()` descriptive trends using boxplots + facets.
- `prepare_panel_data()`helper function to get panel data into shape for plotting response timelines for repeated measures studies.
-  `margot_response_timeline()` plot histogram of response timelines for repeated measures studies.

## Improved

- `here_save_qs()` and `here_read_qs()` report where and object was saved and how large it is. 
- `here_save()`and `here_read()`, ditto, and also ask users to specify a directory path, defaulting to `push_mods` if none is supplied


# [2024-09-01] margot 0.2.1.34

## New
- `margot_plot_discontinuity()` create longitudinal graphs

## Improved
-  `margot_size()` has cli alerts

# [2024-08-30] margot 0.2.1.33

## New
* `read_multiple_images()` utility function to read batchs of images, for presentations, articles etc. 

# [2024-08-30] margot 0.2.1.32

## New

* `margot_plot_batch_rate`, creates and save rate plots from multiple causal forests outputs.. 

# [2024-08-29] margot 0.2.1.31

## Improved

* `margot_batch_policy()` allows user to save plots automatically, with different sizes and resolutions. 

# [2024-08-29] margot 0.2.1.30

## Improved

* `margot_subset_model()` returns subset of results the user requests.


# [2024-08-29] margot 0.2.1.29

## New

* `margot_subset_model()` subsets causal forests for both categorial and binary exposures.


## Deprecated

* `margot_grf_subset_table()`, functions replaced by `margot_subset_model()`, use this new function instead.


# [2024-08-28] margot 0.2.1.28

## Improved

* `margot_qini_plot()` better labels for binary models.

## New

* `margot_plot_exposure()` - utility to plot change in the exposure variable from baseline. 
* `margot_size()` - utility to check size of object

## Improved

* `margot_summary_table()` - now provides optional graphs to show densities/distributions of the exposure and outcomes at baseline, exposure waves, and end of study. 


# [2024-08-27] margot 0.2.1.26

## Improved

* `margot_plot()` - consistent names for results table if these are modified using the new `label_mapping` option. 
* `here_save_qs()` and `here_read_qs()` minor tweaks.

# [2024-08-27] margot 0.2.1.25

* `margot_plot()` and `margot_plot_create_options()` now allow custom labels, which flexibly combine with defaults.

# [2024-08-27] margot 0.2.1.24

## Improved

* robust reporting/error handling in `margot_causal_forest()`, and its helper funciton `compute_qini_curves()`

# [2024-08-26] margot 0.2.1.23

## Deprecations

* `boilerplate_measures()` is deprecated. Use `boilerplate::boilerplate_report_variables()` instead.
* `boilerplate_methods_causal_interventions()` is deprecated. Use `boilerplate::boilerplate_report_causal_interventions()` instead.
* `boilerplate_methods_confounding_control()` is deprecated. Use `boilerplate::boilerplate_report_confounding_control()` instead.
* `boilerplate_methods()` is deprecated. Use `boilerplate::boilerplate_report_methods()` instead.
* `boilerplate_methods_eligibility_criteria()` is deprecated. Use `boilerplate::boilerplate_report_eligibility_criteria()` instead.
* `boilerplate_methods_identification_assumptions()` is deprecated. Use `boilerplate::boilerplate_report_identification_assumptions()` instead.
* `boilerplate_methods_missing_data()` is deprecated. Use `boilerplate::boilerplate_report_missing_data()` instead.
* `boilerplate_methods_sample()` is deprecated. Use `boilerplate::boilerplate_report_sample()` instead.
* `boilerplate_methods_statistical_estimator()` is deprecated. Use `boilerplate::boilerplate_report_statistical_estimator()` instead.
* `boilerplate_methods_target_population()` is deprecated. Use `boilerplate::boilerplate_report_target_population()` instead.
* `boilerplate_methods_variables()` is deprecated. Use `boilerplate::boilerplate_report_variables()` instead.
* `margot_create_database()` is deprecated. Use `boilerplate::boilerplate_manage_measures()` instead.
* `manager_boilerplate_measures()` is deprecated. Use `boilerplate::boilerplate_manage_measures()` instead.
* `margot_create_bibliography()` is deprecated. Use `boilerplate::boilerplate_report_measures()` instead.
* `margot_merge_databases()` is deprecated. Use `boilerplate::boilerplate_merge_databases()` instead. (Note the plural 'databases' in the new function name.)

* to obtain these new functions, use `devtools::install_github("go-bayes/boilerplate")`

# [2024-08-22] margot 0.2.1.22

## Improved

* `margot_create_bibliography` now prints binary tags correctly.  Improved presentation of items, with measures first. 

* `boilerplate_methods_variables` now accepts `margot_create_bibliography`.

# [2024-08-22] margot 0.2.1.21

## Improved

* `margot_create_database` further improved: deleted unnecessary prompts, back_up function in place. 


# [2024-08-21] margot 0.2.1.20

## Improved

* `boilerplate_measures` and `margot_create_bibliography` better printout.  However, again, must develop a package specifically for boilerplates. 


# [2024-08-21] margot 0.2.1.19

## Improved

* `margot_create_database` made more robust, menus work better, and you can copy information from other citations.  (It's clear this will need to be a separate package,... expect a move down the track)

# [2024-08-21] margot 0.2.1.18

## Improved

* `margot_lmtp` more informative reporting and progress bars using `cli`

# [2024-08-21] margot 0.2.1.17

## Improved

* `margot_plot_decision_tree` - user may remove 'Action" labels for cleaner graph, if desired (default is to remove them.)

# [2024-08-21] margot 0.2.1.16

## New

* `margot_plot_create_options` helper function to automate graph production.




# [2024-08-21] margot 0.2.1.15

## Improved

* `margot_plot_policy_tree()`, `margot_plot_qini`, `margot_plot_decision_tree` defaults to nice labels, with informative messages. 

# [2024-08-21] margot 0.2.1.14

## New

* `margot_interpret_marginal` has consistent syntax with `margot_plot`. 
* `transform_table_rownames` to allow for nicer tables with clear labels. 
* overhauled `margot_plot` function so that it produces nice labels, and so that it also generates an interpretation. 

## Improved

* `margot_create_bibliography` markdown output improved with `cli`
* considerably decluttered output of `boilerplate_methods`

## Deprecations

* `margot_interpret_table` replaced by `margot_interpret_marginal`


# [2024-08-20] margot 0.2.1.13

## Improved

* informative `cli` messages for `create_ordered_variable`, `margot_process_binary_vars`, `margot_summary_tables`, `margot_log_transform_vars`, `margot_propensity_model_and_plots`, `margot_process_longitudinal_data`, `margot_causal_forest`, `margot_multiarm_causal_forest`, `margot_batch_policy`

# [2024-08-19] margot 0.2.1.12

## Improved

* integrated `cli` messages for user experience, in `margot_create_database`, `margot_merge_databases`, and `boilerplate_methods` (more to follow.)



# [2024-08-19] margot 0.2.1.11

## New

* `margot_create_database`: allows for creation of bibliography databases. 
* `margot_merge_databases`: allows for merging of bibliography databases. 
* `margot_create_bibliography`: purpose-build for appendices in which all measures are reported. 


## Improved

- restored `coloured_histogram` to package (still useful)


## Deprecations

- `manager_biolerplate_measures` surpassed by `margot_create_database`

# [2024-08-19] margot 0.2.1.10

## Improved
* `boilerplate_measures` overhauled to allow bibliography by sections (for outcomewide studies)
* considerably improved reporting in `boilerplate_methods`, including: selective sections to report. 
* overhauled `boilerplate_methods` for simple and clear reporting
* simplified `biolerplate_methods_variables` to act mostly as a wrapper for `boilerplate_measures`

# [2024-08-19] margot 0.2.1.9

## New
* `boilerplate_methods_additional_sections`

## Improved

* baseline_missing_data_proportion passed to `boilerplate_methods_missing_data`.
* all boilerplate functions now accessible with package (not just internal), allowing for better selective use. 
* allow selective printing of `boilerplate_methods`

# [2024-08-18] margot 0.2.1.8

## New

* `boilerplate_methods` function allows first pass automated reporting. 
*  helper functions include: `boilerplate_methods_sample`, `boilerplate_methods_eligibility_criteria`, `boilerplate_methods_identification_assumptions`, `boilerplate_methods_statistical_estimator`, `boilerplate_methods_confounding_control`, `boilerplate_methods_missing_data`, `boilerplate_methods_causal_interventions`
* implemented 

## Improved

* `manager_boilerplate_measures` now flexibly handles strings for references when 'string_is ...' is used in the reference category that `manager_boilerplate_measures` creates.

* `manager_boilerplate_measures` has been streamlined.

# [2024-08-17] margot 0.2.1.7

## New

* added `batch_edit_measures` within `manager_boilerplate_measures` to allow batch editing of measures database.

## Improved

* `manager_boilerplate_measures` improved by allowing user-friendly navigation forward and backward, and more sensible defaults. requires `rlang`.

# [2024-08-16] margot 0.2.1.6

## New

* `boilerplate_measures` produces an appendix of measures and items uses from variable inputs (for Quarto manuscripts).
* `manager_boilerplate_measures` allows to you add / modify an existing database. 

## Improved

* `margot_plot` consistent syntax: user specifies save_plot as TRUE or FALSE (default = TRUE).

# [2024-08-15] margot 0.2.1.5

## Improved

* `margot_plot` has auto-save and numerous small improvements.

# [2024-08-14] margot 0.2.1.4

## New

* `margot_adjust_weights` to streamline creating inverse probability of censoring weights longitudinally, allowing for sample_weights at baseline. 

# [2024-08-14] margot 0.2.1.3

## New

* `margot_propensity_model_and_plots` a one stop shop for evaluating balance on the treatment: plots & diagnostics generated.


## Restored

* `coloured_histogram()` back by popular demand. 

## Improved

* tweaks to defaults for plots to make them more legible on small computer screens.

# [2024-08-13] margot 0.2.1.2

## Improved

* `margot_plot_policy_combo`, `margot_policy_tree`, `margot_batch_policy` and  now accepts arguments for `margot_plot_decision_tree` and `margot_plot_policy_tree`, leading to customisable policy visualisations. 

* Bigger defaults for x and y axis text in `margot_plot_policy_tree` for legibility.


# [2024-08-12] margot 0.2.1.1

## New

* `margot_batch_policy` runs `margot_policy_tree` for all outcomes in a model. 


# [2024-08-11] margot 0.2.1.0

## New

* `margot_plot_policy_combo`: creates a combination plot for `margot_plot_decision_tree` and `margot_plot_policy_tree()`, easing the burden of interpretation. 


## Improved

* `margot_plot_decision_tree`: policy action leafs different colours (user may specify palette). Defaults to `ggokabeito::scale_fill_okabe_ito()` to match `margot_plot_policy_tree()`
*  `margot_policy_tree` outputs a `margot_plot_policy_combo` in addition to the other otuputs.

## Fixed

* removed `split_vars` from the `margot_causal_forest` and `margot_mulit_arm_causal_forest`
* `margot_plot_decision_tree` correct tree arrangement

# [2024-08-11] margot 0.2.0.9

## Fixed

* `margot_plot_policy_tree` correctly renders decision tree, allows for individual plots for decision leafs, and collects guides.
* error in rendering of `margot_plot_decision_tree`, fixed: function now includes internal tests.
* removed `split_vars` from the `margot_causal_forest` and `margot_mulit_arm_causal_forest`

## Removed

* `debug_node_data_with_positions`, internal function - utility now included with `margot_plot_decision_tree`



# [2024-08-10] margot 0.2.0.8

## New

* `margot_plot_decision_tree` creates policy tree decision rule graphs in a ggplot2 format, so that the graph may be combined `margot_plot_policy` to more effectively communicate decision rules to policy makers. 

* `debug_node_data_with_positions` internal so that `margot_plot_decision_tree` works properly

## Fixed

* `margot_plot_decision_tree` requires `debug_node_data_with_positions`


## Improved

* Enhanced `margot_plot_policy` so that it produces a `margot_plot_decision_tree` graph, as well as its previous outputs.

# [2024-08-9] margot 0.2.0.7

## New

* `margot_interpret_policy_tree` interprets policy_tree outputs with outputs in either markdown or \LaTeX formats.
* `margot_policy_tree` wraps `margot_interpret_policy_tree`, `margot_plot_policy_tree`, `margot_qini_plot` and a decision tree visualisation within one function.

## Improved

* `margot_plot` will now work even if no title or subtitle is passed. 
* `margot_plot_policy_tree`: focus is not simply on plotting, rather than doing both plotting and interpreting. 
* `margot_interpret_table`: no longer requires specification of estimate. General explanation printed separately (as it is only used once).

# [2024-08-8] margot 0.2.0.6

## Improved
* Greatly enhanced the functionality of `margot_plot_policy_tree` so that it explains the result. 

## Fixed

* broken indexing when I changed `margot_causal_forest` and `margot_multi_arm_causal_forest` to use outcome names, rather than model names.

# [2024-08-8] margot 0.2.0.5

## Improved
* `margot_causal_forest` and `margot_multi_arm_causal_forest` now use outcome names, rather than model names, to name the outcomes


# [2024-08-7] margot 0.2.0.4

## New

* `margot_process_longitudinal_data` orders correctly for `lmtp` models by updating the censoring column `not_lost` such that it handles missing responses as well as attrition. The function additionally automatically dummy codes ordinal variables and standardises continuous variables. Presently it is only implemented for three waves, but in the future it will be expanded to handle arbitrarily many. 

## Fixed

* Functions `compute_qini_curves` and `extract_qini_data` to work with binary interventions

# [2024-08-7] margot 0.2.0.3

## New

* `margot_summary_tables` creates summary tables of baseline variables, exposure(s), and outcome(s) using the `gtsummary` package.

## Deprecations

* `margot_compute_gender_weights` is deprecated. Now use `margot_compute_gender_weights_by_wave`.


# [2024-08-7] margot 0.2.0.2

## Deprecations

* `coloured_histogram()` and `coloured_histogram_quantile()` are deprecated.
  Now use the new `margot_plot_hist()` instead.
* `create_ordered_variable_custom` is deprecated. Now use `created_ordered_variable`function with `custom_breaks = c(..)` to obtain custom breaks. 

## New
* `margot_plot_hist()` for plotting distributions of the exposure variable


# margot 0.2.0.1
* new table and plot functions for `grf` outputs

# margot 0.2.0

* improved subgroup comparison function 
* deprecated `compare_group_means` function, with `compare_group`allowing for contrasts of on both the causal difference and relative risk scales.
* new wrapper functions functions for `grf` plus visualising results.


# margot 0.1.2.1
* group_tab now works under the hood of margot_plot, so no need to specify explicitly
* group_tab now allows custom order for plot, not just by decreasing effect size
* numerous small enhancements to older parametric model options
* new logo

# margot 0.1.2

* minor fixes


# margot 0.1.1

* functions to interpret outputs of `lmtp` models
* functions to create tables
* functions to create graphs

# margot 0.1.0.1

* Minor bug fixes and performance improvements.


# margot 0.1.0

* Initial release: includes data exploration, model preparation, utility functions.
