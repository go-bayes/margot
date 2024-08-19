
# To do

* Develop automated reporting of all scales and measures for quarto documents.
* Begin writing tutorials for use cases.

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
