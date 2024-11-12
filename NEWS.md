
# To do

* Begin writing tutorials for use cases.
* fix decision tree layout
* progress [Develop automated reporting of all scales and measures for quarto documents.]
* [qini cost options] done [tick!].
* remove `color` package
* methods
* replace `stringr` and use `tools`
* get standard deviation units into reports
* batch process trend plots
* convert values in decision tree plots from standard deviation units to data scale in `margot_interpret_policy_tree`
* clean up require packages in  `margot_wide_machine()` and elsewhere [tick!].


# [2024-11-11] margot 0.3.0.2

## Fixed
`margot_plot()` label now reads "causal difference" rather than "causal risk difference".

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
