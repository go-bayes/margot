# CHANGELOG_DEV.md

## margot 1.0.212 (2025-07-30)

### Bug Fixes

- **Fixed transformation lookup for flipped outcomes in policy tree interpretations**:
  - Added detection for `_r` suffix indicating flipped models from `margot_flip_forests()`
  - Modified `get_outcome_transformation_info()` to remove `_r` suffix when searching in `original_df`
  - This fixes missing "original scale" output for flipped models like "(reduced) Depression"
  - The fix correctly preserves labels and only affects outcome value transformations
  - Split values (covariates) remain unchanged as they were never flipped

## margot 1.0.211 (2025-07-30)

### Major Changes

- **Corrected interpretation of log-transformed outcomes in policy trees**:
  - Implemented multiplicative interpretation for log-transformed outcomes (standard econometric practice)
  - Treatment effects on log scale now correctly translate to multiplicative effects on original scale
  - Updated calculation: delta_log = treatment_effect * log_sd, ratio = exp(delta_log), pct_change = (ratio - 1) * 100
  - Changed display format from "37% increase, from $32.7 to $44.8" to "47% multiplicative increase, ~$490 average increase"
  - Applied to both individual leaf CATEs and overall weighted average treatment effects
  - Added detection and correction for unrealistic population means from subset data
  - For charity donations: corrects mean from ~$92 to ~$1,048 when subset data is detected
  - This fixes dollar amounts being underestimated by factor of ~11x
  - Example: Leaf CATEs now correctly show ~$378-$553 instead of $33-$48

## margot 1.0.182 (2025-07-28)

### New Features

- **New margot_policy_tree() function**:
  - Provides direct computation of policy trees without re-running causal forests
  - Supports flexible covariate selection:
    - Exclude patterns (e.g., "_log" to remove log-transformed variables)
    - Custom covariate sets
    - Multiple modes: original, custom, add, all
  - User-configurable train_proportion parameter (default 0.5)
  - Depth selection: compute depth-1, depth-2, or both
  - Full compatibility with downstream functions:
    - margot_policy()
    - margot_plot_policy_tree()
    - margot_plot_policy_combo()
    - margot_interpret_policy_tree()
  - Structured output matching margot_causal_forest() format
  - Comprehensive metadata tracking for reproducibility
  - Output works directly with existing functions: margot_plot_policy_tree(), 
    margot_interpret_policy_tree(), and margot_plot_policy_combo()
  - No need for special wrapper functions - the design ensures full compatibility

### Improvements

- **Updated policy tree estimation defaults**:
  - Changed default train_proportion from 0.7 to 0.5 in margot_causal_forest(), 
    margot_flip_forests(), and margot_policy_tree()
  - Added seed parameter to margot_causal_forest() (default 12345) for reproducible 
    train/test splits
  - **BREAKING CHANGE**: Both depth-1 and depth-2 policy trees now use only top_n_vars
  - This ensures consistency between tree depths and reduces overfitting risk
  - Seeds are model-specific to maintain independence across outcomes

## margot 1.0.181 (2025-07-28)

### Improvements

- **Enhanced margot_rate_cv() result interpretation**:
  - Added warnings for models showing negative heterogeneity (negative t-statistics)
  - Only displays alpha level when an adjustment method is used (bonferroni)
  - Provides clearer messaging about statistical significance and practical implications
  - Helps users identify models where heterogeneity may be in opposite direction

## margot 1.0.141 (2025-07-25)

### Bug Fixes

- **Fixed scale parameter error in margot_interpret_qini**:
  - Added missing scale parameter to margot_interpret_qini_binary function
  - Fixed parameter passing from margot_interpret_qini to margot_interpret_qini_binary
  - Added validation to ensure scale is always a length-1 vector
  - Resolves "EXPR must be a length 1 vector" error in switch statement

### MAQ Parameter Updates

- **Added seed and sample.weights support to all maq calls**:
  - Updated `compute_qini_curves_binary()` to accept and pass weights and seed parameters
  - Updated `margot_generate_qini_data()` to accept and pass seed parameter
  - Updated `margot_recompute_qini_ipw()` and `margot_recompute_qini_aipw()` to pass seed
  - All maq::maq() calls now include seed = 42 for reproducibility
  - sample.weights parameter properly passed when weights are available
  - Ensures reproducible QINI curves and correct handling of weighted data

### QINI Scale Transformations

- **Added scale parameter to QINI plot functions**:
  - New `scale` parameter in `margot_plot_qini()` and `margot_plot_qini_batch()` with options:
    - "average" (default): Shows average policy effects per unit (maq default implementation)
    - "cumulative": Shows traditional cumulative gains (multiply by proportion)
    - "population": Shows total population impact (multiply by proportion and n_units)
  - Transforms both main curves and confidence intervals appropriately
  - Y-axis labels update automatically based on selected scale
  - Documentation explains difference between maq's average gains and traditional QINI cumulative gains

- **Created helper functions for scale interpretation**:
  - `margot_qini_scale_note()`: Generates explanatory text for what each scale represents
  - `margot_qini_scale_subtitle()`: Creates brief descriptions for plot subtitles
  - Helps users understand the meaning of different scale options

- **Updated margot_interpret_qini for scale awareness**:
  - Added `scale` parameter to affect interpretation text
  - Uses appropriate terminology ("average policy effects", "cumulative gains", or "total population impact")
  - Adds explanatory notes when using non-default scales
  - Ensures consistency between plots and interpretations

## margot 1.0.140 (2025-07-24)

### QINI Curve Improvements

- **Consistent baseline method across package**: 
  - Updated `margot_causal_forest()` to use "maq_no_covariates" as default baseline method
  - Previously used constant rewards ("maq_constant"), now matches visualization functions
  - Includes automatic fallback to constant rewards if maq_no_covariates fails
  - This makes the theoretically preferred method consistent throughout the package

- **Fixed QINI regeneration for flipped models**: Resolved issue where flipped models (with _r suffix) failed to regenerate QINI curves when baseline_method was changed
  - Removed conditional check that prevented regeneration when mc_result$data was NULL
  - Added fallback to existing QINI data when regeneration fails
  - Enhanced debugging for forest object retrieval
  
- **Added customizable QINI plot colors**: 
  - New parameters `cate_color` (default: gold #d8a739) and `ate_color` (default: dark gray #4d4d4d)
  - Available in both `margot_plot_qini()` and `margot_plot_qini_batch()`
  - Improves visual distinction between CATE-based targeting and no-priority assignment
  
- **Ensured QINI consistency with metadata storage**:
  - Store QINI generation metadata including test indices, baseline method, and data split info
  - `margot_summary_cate_difference_gain()` now uses stored metadata to ensure average gains use same data subset as original QINI curves
  - Added informative CLI messages throughout for transparency
  - Fixed baseline_method metadata to correctly reflect "maq_constant" for compute_qini_curves_binary
  
- **Fixed breaking change in QINI regeneration**: 
  - Added check to verify data availability before attempting QINI regeneration

- **Updated default spend levels across package**:
  - Changed default spend_levels from c(0.2, 0.5) to c(0.1, 0.4) in all functions
  - Affected functions: margot_plot_qini, margot_plot_qini_batch, margot_interpret_qini, 
    margot_interpret_heterogeneity, margot_policy, margot_qini, margot_qini_diagnostic, margot_batch_policy
  - Better reflects typical use cases where 10% and 40% spend levels are more informative than 20% and 50%

### Heterogeneity Interpretation Enhancements

- **Added cautiously selected models category**: `margot_interpret_heterogeneity()` now returns additional outputs:
  - `cautiously_selected_model_ids` and `cautiously_selected_model_names` for models with mixed evidence
  - `all_selected_model_ids` and `all_selected_model_names` combine both selected and cautiously selected models
  - These are models that show positive evidence in some tests but negative in others
  - Helps users identify models that may benefit from targeting but require careful validation
  - Example: models showing QINI curve benefits but negative RATE tests
  - When data is not available (e.g., save_data = FALSE), falls back to existing QINI curves with warning
  - Prevents errors when trying to change baseline_method without available data
  - Users now get clear warnings instead of cryptic errors

### QINI Baseline Method Consistency

- **Added baseline_method tracking**: `margot_policy()` and `margot_qini()` now store the baseline method used
  - Enables detection of mismatches between plots and interpretation summaries
  - Stored in output structure for each model

- **Enhanced margot_interpret_qini()**: Added baseline_method parameter
  - Accepts NULL (default) to use stored baseline method
  - Can specify alternative baseline methods: "maq_no_covariates", "auto", "simple", "maq_only", "none"
  - Warns when requested baseline method differs from stored method
  - Future: will support regeneration of summaries with different baseline methods

- **Fixed margot_qini() regeneration**: Now properly regenerates QINI curves when baseline method changes
  - Detects when stored baseline method differs from requested method
  - Forces regeneration and updates the original model objects
  - Ensures diff_gain_summaries use the new baseline method
  - Prevents mismatch between QINI plots and interpretation tables

## margot 1.0.130 (2025-07-24)

### New Features
- **margot_qini()**: New standalone function for generating QINI curves and difference gain summaries
  - Takes causal forest results as input (like margot_rate does)
  - Generates QINI curves using margot_generate_qini_data()
  - Computes difference gain summaries at specified spend levels
  - Returns structured output suitable for margot_interpret_qini()
  - Supports baseline_method parameter for flexibility
  - Includes label_mapping and formatting options
  - Provides direct access to QINI analysis without full policy analysis

- **margot_interpret_rate()**: Moved from _temp folder to main R package
  - Now properly exported and documented
  - Interprets RATE estimates from margot_rate()
  - Handles both single method (AUTOC or QINI) and comparison of both
  - Provides markdown-formatted summaries
  - Includes margot_interpret_rate_comparison() for comparing AUTOC vs QINI

### Improvements
- **margot_interpret_qini()**: Updated to accept output from both margot_policy() and margot_qini()
  - Added documentation about dual compatibility
  - No breaking changes to existing functionality

- **Consistent API design**:
  - margot_rate() → margot_interpret_rate() for RATE analysis
  - margot_qini() → margot_interpret_qini() for QINI analysis  
  - Both follow the same pattern for generation and interpretation

### Documentation
- Updated _pkgdown.yml to include margot_qini in "Interpret and Report Results" section
- All new functions properly documented with examples

### Default Changes
- Changed default baseline_method from "auto" to "maq_no_covariates" in all QINI functions:
  - margot_generate_qini_data()
  - margot_qini()
  - margot_plot_qini()
  - margot_plot_qini_batch()
  - margot_policy()
- This makes the default behavior more theoretically sound and predictable
- **Important**: "maq_no_covariates" now automatically falls back to simple baseline if maq fails
  - Provides robustness while maintaining theoretical preference
  - Clear warning messages when fallback occurs
  - Users always get a baseline curve with the default setting

### Bug Fixes
- Fixed outcome data retrieval for flipped models (those with _r suffix)
  - margot_plot_qini(), margot_qini(), and margot_policy() now prioritize getting data from forest objects for flipped models
  - Flipped models use the same outcome data as stored in their GRF forest objects (Y.orig)
  - This ensures QINI curves are computed with the correct data that was used to train the model
  - Resolves "Cannot find outcome data" errors for reversed/flipped models
  - **Note on flipped model data**: When models are created via margot_flip_forests(), the outcome
    is negated ONCE during model creation. The forest's Y.orig contains the already-negated values.
    QINI calculations correctly use this pre-negated data without additional flipping, ensuring
    consistency between model training and QINI curve generation.
  - **Fixed QINI regeneration for flipped models**: Previously, QINI regeneration with different baseline
    methods would fail for flipped models when mc_result$data was NULL. Now allows regeneration by
    accessing outcome data from forest objects (Y.orig) even when the main data object is unavailable.
  - **Improved margot_flip_forests() data handling**: Now properly merges flipped outcome data into the
    main data object, and removes original outcome data when remove_original=TRUE. This ensures
    downstream functions can find flipped outcome data in the expected locations.
  - **Enhanced QINI regeneration with better debugging and fallback**:
    - Added detailed debugging messages when forest objects can't be found for flipped models
    - Checks multiple locations for forest objects (model_result$model, model_result$full_model, mc_result$full_models)
    - Implements graceful fallback to existing QINI data when regeneration fails
    - Preserves QINI metadata (test indices, sample sizes) to ensure consistent regeneration
    - Uses stored test indices when regenerating to match original sample size
  - **Customizable QINI plot colors**: Added `cate_color` and `ate_color` parameters to margot_plot_qini() 
    and margot_plot_qini_batch() for full color customization. New defaults: gold (#d8a739) for CATE 
    (targeted treatment) and dark gray (#4d4d4d) for ATE (no-priority assignment)

## margot 1.0.125 (2025-07-24)

### Enhancements
- **margot_interpret_heterogeneity()**: Improved clarity of test results
  - Renamed "omnibus_test" column to "differential_prediction_test" for clarity
  - Added "mean_prediction_test" column to show calibration status (informational only)
  - Simplified main interpretation and enhanced extended report with complementary methods explanation
  - Updated omnibus calibration test description to match grf manual clarity
- **QINI ATE Baseline**: Improved reliability and consistency
  - ATE baseline now always generated as straight line using mean(tau_hat)
  - Ensures fair comparison between CATE and ATE curves
  - More robust than relying on maq output for constant rewards
  - Aligns with maq conceptual approach while being more predictable
- **margot_plot_qini() enhancements**:
  - Changed CATE curve color from blue (#4f88c6) to green (#009E73) to avoid "control" association
  - Added ylim parameter for manual y-axis control (defaults to automatic scaling)
  - ylim parameter also added to margot_plot_qini_batch() for consistency

### Major Architecture Change
- **QINI curve generation moved to on-demand**: 
  - Created margot_generate_qini_data() helper function for on-demand generation
  - margot_plot_qini() now generates QINI data when needed instead of requiring pre-computed data
  - margot_plot_qini_batch() updated to work with on-demand generation
  - margot_policy() and margot_summary_cate_difference_gain() generate QINI objects as needed
  - This approach is more robust and handles edge cases better
  - Follows maq's mathematical approach for ATE baselines (straight line)
  - Future: can remove QINI generation from margot_causal_forest() entirely
- **Robust baseline methods for QINI curves**:
  - Created margot_qini_simple_baseline() function that generates baseline curves that cannot fail
  - Implemented hybrid approach in margot_generate_qini_data() with options:
    - "auto" (default): Try maq with target.with.covariates = FALSE, fall back to simple baseline
    - "simple": Always use simple baseline (straight line from (0,0) to (1, mean(tau_hat)))
    - "maq_no_covariates": Use maq with target.with.covariates = FALSE (may fail)
    - "maq_only": Use standard maq with constant rewards (may fail)
    - "none": No baseline curve
  - Simple baseline implements average_gain() method for compatibility with diff_gain_summaries
  - Updated margot_summary_cate_difference_gain() to handle both maq and simple baseline objects
  - Added integrated_difference_simple() function for computing differences with simple baselines
  - Enhanced data extraction to find Y and W data in grf forest objects when mc_result$data is NULL
  - Baseline method parameter added to margot_plot_qini(), margot_plot_qini_batch(), and margot_policy()
  - Simple baseline represents expected gain under random allocation: gain(B) = B * E[tau]
  - Smart fallback logic in margot_plot_qini(): when data regeneration fails but simple baseline is requested,
    adds baseline to existing QINI data by extracting ATE from multiple sources
  - Improved regeneration logic: only regenerates when data is available or when changing baseline types
  - Better error messages showing available fields when baseline generation fails

### Breaking Changes
- **Removed save_plot parameters**: 
  - Removed save_plot and related parameters (dpi, width, height, output_dir) from:
    - margot_plot_qini_batch()
    - margot_policy()
    - margot_plot_rate_batch()
  - Users should save plots manually using ggplot2::ggsave() if needed
  - Simplifies functions and reduces maintenance burden

### Bug Fixes
- Fixed omnibus test matching for flipped models from margot_flip_forests()
- Improved matching logic using original outcome names for reliability

## margot 1.0.115 (2025-07-23)

### New Features
- **margot_interpret_heterogeneity()**: Comprehensive function to combine heterogeneity evidence
  - Integrates RATE AUTOC, RATE QINI, QINI curves, and omnibus calibration tests
  - Provides unified model selection based on multiple evidence sources
  - Includes concordance analysis showing which methods agree/disagree
  - Generates interpretation text with specific recommendations
  - Flexible selection criteria with `require_any_positive` and `exclude_negative_any` parameters
  - Returns detailed evidence summary table for transparency
  - Simplified interface: takes models directly instead of requiring pre-computed interpretations
  - New evidence categorisation: "evidence_for_heterogeneity", "targeting_opportunity", "statistical_only", etc.
  - Added `include_extended_report` parameter for detailed academic-style report with full statistics
  - Extended report includes confidence intervals, p-values, and detailed method descriptions
  - Uses proper LaTeX escaping for mathematical notation (e.g., $\\hat{\\tau}(x)$)
  - New Zealand English spelling throughout extended report
  - Evidence summary table now includes model_id column for reliable internal matching
  - Fixed omnibus test computation to include flipped models from margot_flip_forests()
  - Improved omnibus test matching logic to use original outcome names for reliable matching
  - Simplified main interpretation and enhanced extended report with complementary methods explanation
  - Updated omnibus calibration test description to match grf manual clarity
  - Renamed "omnibus_test" column to "differential_prediction_test" for clarity
  - Added "mean_prediction_test" column to show calibration status (informational only, not used in scoring)

- **margot_plot_qini_batch()**: Batch processing for QINI plots
  - Process multiple models in one function call
  - Supports all parameters from margot_plot_qini()
  - Automatically saves plots to specified directory
  - Returns list of ggplot objects for further customization

### Enhancements
- **margot_interpret_rate()**: Added excluded model lists to output
  - New fields: `excluded_both` (models negative in both AUTOC and QINI)
  - New fields: `excluded_either` (models negative in either AUTOC or QINI)

- **margot_interpret_qini()**: Enhanced output structure
  - Added `harmful_model_ids` and `harmful_model_names` for negative QINI gains
  - Added `no_effect_model_ids` and `no_effect_model_names` for inconclusive results
  - Parallel structure to margot_interpret_rate() for easier integration

## margot 1.0.112 (2025-07-23)

### New Features
- **margot_flip_forests() GRF parameter support**: 
  - Added `grf_defaults` parameter to allow users to specify GRF model parameters
  - When NULL (default), automatically extracts parameters from original fitted models
  - Ensures consistency between original and flipped models
  - Extracts common parameters: num.trees, honesty, honesty.fraction, alpha, min.node.size, mtry, etc.
  - Provides verbose output about which parameters are being used
  - Example usage: `margot_flip_forests(results, flip_outcomes = c("anxiety"), grf_defaults = list(num.trees = 4000, honesty = TRUE))`

## margot 1.0.80 (2025-07-19)

### New Features
- **Conditional Means Support**: Added experimental support for conditional means computation via `policytree::conditional_means()`
  - New parameter `compute_conditional_means` in `margot_causal_forest()` (default TRUE)
  - Conditional means are computed for each model and stored in results
  - Works with both regular and parallel versions of causal forest functions
  - Provides expected outcomes under each treatment arm for enhanced interpretability

### Technical Details
- Conditional means computation requires the `policytree` package
- Computation is wrapped in error handling to ensure graceful degradation
- The feature is enabled by default but can be disabled by setting `compute_conditional_means = FALSE`

### Policy Tree Interpretation Enhancements
- Enhanced `margot_interpret_policy_tree()` with conditional means interpretation
  - New `include_conditional_means` parameter (default TRUE)
  - Shows expected outcomes under each treatment within policy tree leaves
  - Computes average treatment effects within each leaf
  - Provides N units in each leaf
  - Works seamlessly with `margot_interpret_policy_batch()`
  - Enhanced depth-2 tree interpretation to show all 4 leaves with conditional means
  - Added percentage of units assigned to recommended action in each leaf
  - Maintains overall test set allocation summary
  - Added leaf summary showing size and percentage of each leaf
  - Added percentages to overall allocation summary
  - Shows total test set size for clarity
- Made output more user-friendly and quarto-compatible:
  - Added interpretation guide with callout tips
  - Uses markdown headers (###, ####) for better structure
  - Added effect size interpretations (small/moderate/large)
  - Added outcome interpretations (below/near/above average)
  - Uses formatted treatment effects with + sign for clarity
  - Added key takeaway summary in callout note
  - Ready to drop directly into quarto documents
- Improved output clarity and flexibility:
  - Changed default output to clear, simple language (science should be clear!)
  - Added `use_math_notation` parameter (default FALSE) for optional mathematical notation
  - Added `output_format` parameter with "bullet" (default) and "prose" options
  - Prose format creates flowing narrative text suitable for direct inclusion in papers
  - Removed uncertainty quantification disclaimer (just report point estimates)
  - Enhanced leaf output with sample sizes and percentages
  - Added interpretation arrows showing effect direction
  - Suitable for both academic papers and policy documents

### Refactoring Completed
- `margot_flip_forests()` completely rewritten to create new models with "_r" suffix
  - No longer modifies models in place
  - Properly recomputes all statistics (ATE, RATE, E-values)
  - Handles conditional means by swapping columns for binary treatment
  - Automatically rebuilds combined_table
  - Fixed QINI recalculation to properly access outcome data and treatment assignment
  - Added multiple fallback strategies to find outcome data with different key formats
  - Now copies original QINI data as fallback when recalculation fails

### New Functions Added
- Created `margot_recalculate_policy_trees()` - exported function for customizing policy tree covariates
  - Supports four covariate modes: "original", "custom", "add", "all"
  - Allows exclusion of covariates via regex patterns
  - Handles automatic expansion for depth-2 trees when needed
  - Includes parallel processing support
  - Comprehensive documentation and examples

### Function Updates
- Updated `margot_reversed_labels()` to handle new "_r" suffix convention
  - New parameter `use_r_suffix` (default TRUE) creates entries with "_r" suffix
  - Old behavior available by setting `use_r_suffix = FALSE`
  - New parameter `remove_original` to optionally remove non-flipped entries
  - Maintains backward compatibility

### Bug Fixes
- Fixed "subscript out of bounds" error in depth-2 policy tree interpretation
  - Added checks for valid indices before subsetting
  - Handles cases where no units fall in left or right branches
- Fixed "non-character argument" error in `margot_interpret_policy_batch`
  - Added validation for NULL interpretations
  - Gracefully handles cases where all interpretations fail
- Added error handling around conditional means computation
  - Prevents entire interpretation from failing if conditional means error
- Fixed parameter naming error in `margot_flip_forests_parallel`
  - Changed `n_cores_policy` to `n_cores` to match `margot_recalculate_policy_trees` signature
- Fixed propensity plot title in `margot_assess_overlap`
  - Now correctly shows exposure name instead of undefined outcome_name

### Cleanup Completed
- Removed `margot_flip_forests_dev()` and its artifacts since functionality exists in `margot_recalculate_policy_trees()`

### New Diagnostic Function: margot_assess_overlap
- Added `margot_assess_overlap()` function for evaluating propensity score overlap
  - Analyzes common support regions for treatment validity
  - Provides comprehensive diagnostics for causal estimate reliability
  - Generates propensity score distribution plots by treatment group
  - Includes covariate balance tables within propensity score strata
  - Reports test calibration p-values from grf
  - Provides trimming summaries for poor overlap regions
  - Added `exposure_name` parameter to specify the treatment variable
  - Added `label_mapping` parameter for custom exposure labels
  - **Refactored to focus on exposure overlap rather than outcome overlap**
  - Now correctly calculates overlap statistics once for the exposure variable
  - Fixed conceptual issue where overlap was incorrectly assessed per outcome
  - Now uses `transform_label` helper for automatic label formatting
  - Applies label transformation to both exposure and outcome names when no custom mapping provided
  - Added `text_summary` return value with prose description suitable for dropping into documents
  - Text summary can be used with `cat(overlap_results$text_summary)` for easy report inclusion
  - Added `theme` parameter to allow customization of ggplot2 theme (default: "classic")
  - Updated propensity plot colors to use package-consistent colors: treatment (#d8a739) and control (#4f88c6)
  - Enhanced text summary with interpretation of what overlap means in causal inference context
  - Increased color opacity (0.85) and line weights for clearer, more vibrant visualizations

### Enhanced Qini Analysis Functions
- **margot_interpret_qini()** major improvements:
  - Added `spend_levels` parameter to analyze custom spend levels (default: c(0.2, 0.5))
  - Added `include_intro` parameter to control explanatory text about CATE and Qini curves
  - New `concise_summary` output provides brief summary grouping outcomes by benefit/harm/no effect
  - Enhanced explanatory text describes how Qini curves work for beneficial vs detrimental exposures
  - Dynamic handling of any number of spend levels
  - Automatic detection of available spend levels with warnings when requested levels don't exist
  - Function will use available spend levels if requested ones are not found in the data
  - Backward compatible with existing code

- **margot_plot_qini()** enhanced visualization:
  - Added vertical dashed lines at spend levels (customizable via `show_spend_lines`, `spend_line_color`, `spend_line_alpha`)
  - Replaced multiple label parameters with single `label_mapping` using `transform_label` helper
  - Added `theme` parameter for ggplot2 theme selection (matches margot_assess_overlap)
  - Spend levels shown in plot match those analyzed in margot_interpret_qini
  - Cleaner function signature with fewer parameters
  - Added text annotations to label spend level lines (e.g., "20% spend", "50% spend")
  - Labels positioned to avoid overlap when multiple spend levels are shown

### API Consistency Improvements
- Renamed `spend` parameter to `spend_levels` in both `margot_policy()` and `margot_batch_policy()`
  - This creates consistency with `margot_interpret_qini()` which uses `spend_levels`
  - Makes the API more intuitive and predictable
  - Backward compatibility note: users need to update their code to use `spend_levels` instead of `spend`

### Bug Fixes (Additional)
- Fixed `margot_policy()` and `margot_batch_policy()` to pass `spend_levels` to `margot_plot_qini()`
  - Previously, Qini plots always showed vertical lines at 20% and 50% regardless of the spend_levels parameter
  - Now the vertical lines in Qini plots correctly match the spend levels used for diff_gain_summaries
- Fixed `margot_plot_qini()` to properly use custom label mappings in plot titles
  - Now tries exact match first before falling back to transform_label pattern matching
  - Handles both "model_outcome" and "outcome" key formats in label_mapping

### Minor Improvements
- Updated `margot_reversed_labels()` to use "(reduced)" instead of "(reversed)" suffix
  - Makes interpretation clearer that the outcome direction has been reduced/flipped
  - Affects all flipped outcome labels throughout the package
- Enhanced `margot_interpret_rate_comparison()` output with richer information
  - Now includes RATE estimates and 95% CIs for all reported outcomes
  - Added section for negative RATE estimates with explicit caution against CATE prioritisation
  - Added summary of unreliable outcomes where neither method found significant heterogeneity
  - More informative comparison text suitable for academic reporting
  - Reformatted output to use flowing paragraph text instead of bullets and choppy sentences
  - All information now presented as cohesive paragraphs suitable for direct inclusion in academic papers

### Policy Tree Interpretation Updates (2025-07-20)
- Modified `margot_interpret_policy_batch()` output formatting:
  - Removed "Policy tree analysis results:" line after the heading
  - Now displays clean output with just the heading followed by findings
- Enhanced `margot_interpret_policy_tree()` clarity:
  - Added "at the end of study" to all Findings headings for temporal clarity
  - Added "baseline" prefix to all variable names in tree splits and leaf descriptions
  - This disambiguation makes it clear that splits are based on baseline characteristics, not outcomes
- Simplified CATE reporting in leaf descriptions:
  - Removed confusing interpretations about treatment "improving" or "worsening" outcomes
  - Now simply states the CATE value without subjective interpretation
  - More appropriate for standardized outcomes where negative values are common
  - Overall policy advice remains in the summary section

### Policy Tree Plotting Updates (2025-07-20)
- Enhanced `margot_plot_policy_tree()` to clarify baseline variables:
  - Added "(baseline)" to axis labels on all policy tree plots
  - Added "(baseline)" to plot titles and subtitles
  - Updated both depth-1 and depth-2 plots for consistency
- Enhanced `margot_plot_decision_tree()` to clarify temporal context:
  - Now appends "Outcome" to outcome labels in titles
  - Example: "Neuroticism" now displays as "Neuroticism Outcome"
- Updated `transform_var_name()` helper function:
  - Now removes "_l" suffix from variable names
  - Capitalizes NZSEI and NZDEP acronyms properly
  - Example: "t0_nzsei_13_l" now displays as "NZSEI 13"

### Minor Updates (2025-07-20)
- Updated `margot_reversed_labels()` formatting:
  - Changed from "{outcome} (reduced)" to "(reduced) {outcome}"
  - Example: "Anxiety (reduced)" now displays as "(reduced) Anxiety"
  - Improves readability and understanding of reduced/flipped outcomes

### New Functions (2025-07-21)
- Added `margot_recompute_ate()` function for recomputing ATEs with different target samples:
  - Allows recomputation without refitting expensive causal forest models
  - Supports target_sample options: "all", "treated", "control", "overlap"
  - Recomputes E-values with the new ATE estimates
  - Preserves all original model structure while updating estimates
  - Includes metadata about recomputation parameters
  - Uses margot-style parameter naming (target_sample not target.sample)
  - Renames effect columns to ATE/ATT/ATC/ATO based on target sample
- Added `margot_recompute_ate_batch()` for comparing ATEs across multiple target samples:
  - Convenience function to compute ATEs for all target sample types
  - Creates comparison table showing estimates and E-values side by side
  - Uses clear column names: ATE, ATT, ATC, ATO with corresponding E-values
  - Useful for sensitivity analysis and exploring treatment effect heterogeneity

### margot_plot() Enhancements (2025-07-21)
- **New Column Support**: margot_plot() now accepts ATE/ATT/ATC/ATO column names in addition to traditional E[Y(1)]-E[Y(0)] format
  - Automatically detects and uses the appropriate effect column
  - Maintains full backwards compatibility with existing code
- **Enhanced rename_ate Parameter**: 
  - Now accepts boolean (TRUE uses appropriate label based on column type)
  - Accepts custom string for complete control
  - Auto-detects appropriate label (ATE/ATT/ATC/ATO) when set to TRUE
- **New rename_evalue Parameter**: 
  - When TRUE, renames E_Value to "E-Value" and E_Val_bound to "E-Value Bound"
  - Makes column names more publication-ready
- **Updated Supporting Functions**:
  - margot_interpret_marginal() now correctly describes treatment effect type
  - back_transform_estimates() handles new column types for original scale transformations
  - group_tab() detects and processes new effect column types
  - All changes maintain backwards compatibility

### margot_interpret_marginal() Improvements (2025-07-21)
- **Clearer messaging when no reliable effects found**:
  - Now explicitly states "No reliable effects are evident." when no outcomes meet reliability threshold
  - Previously returned only adjustment notes without clear indication of no effects
- **Updated language for presenting effects**:
  - Changed from "showed reliable causal evidence" to "present reliable causal evidence"
  - More direct and present-tense wording improves clarity

### Qini Plot Enhancements (2025-07-21)
- **margot_plot_qini()**: Added confidence interval support
  - New parameters: show_ci, ci_alpha, ci_n_points, ci_ribbon_alpha, ci_ribbon_color
  - Confidence intervals computed using maq::average_gain()
  - Visualized with ggplot2::geom_ribbon()
  - Maintains backwards compatibility (CI display off by default)
  - Fixed geom_ribbon aesthetic inheritance issue with inherit.aes = FALSE
  
- **margot_policy()**: Added qini_args parameter
  - Allows passing additional arguments to margot_plot_qini()
  - Enables confidence interval display in policy batch processing
  - User-provided args override defaults via modifyList()

### New AIPW Enhancement (2025-07-21)
- **margot_recompute_qini_aipw()**: New function to recompute QINI curves using AIPW scores
  - Provides doubly robust estimates (consistent if either propensity or outcome model is correct)
  - Especially valuable for observational data with potential confounding
  - Implements regression forest method for estimating conditional means (mu.hat)
  - Trains separate forests on control/treated units to estimate E[Y|W=0,X] and E[Y|W=1,X]
  - Fully compatible with existing plotting and interpretation functions
  - Adds metadata tracking (qini_method = "AIPW", timestamp, details)
  - Created example script in inst/examples/test_aipw_qini.R

### AIPW Implementation Updates (2025-07-22) - v1.0.110
- **Fixed maq parameter names**: Changed from tau.hat/evaluation.scores to reward/DR.scores
- **Enhanced data extraction**: Function now handles multiple data storage patterns:
  - Models stored in result$model or full_models list
  - Data stored at top level (data, covariates, not_missing)
  - Automatic reconstruction of X, Y, W from saved components
- **Smart treatment detection**: 
  - Added treatment_var parameter for manual specification
  - Auto-detects binary treatment variables when not specified
  - Provides informative messages about detected variables
- **Propensity score handling**:
  - Uses model's W.hat when available
  - Estimates via regression forest when missing
  - Trims extreme values (0.01, 0.99) for stability
- **Overlap diagnostics**: Warns when propensity scores < 0.05 or > 0.95

### QINI API Modernization (2025-07-22) - v1.0.110
- **Fixed compute_qini_curves_binary()**: Updated to use modern maq API
  - Changed from positional args: `maq(tau_hat, 1, IPW_scores, R = 200)`
  - To named parameters: `maq(reward = ..., cost = ..., DR.scores = ..., R = 200)`
  - Affects all QINI computations in margot_causal_forest and margot_flip_forests
  - Resolves inconsistencies between different QINI computations
- **Created margot_recompute_qini_ipw()**: IPW-only version
  - Simpler alternative to AIPW for debugging and speed
  - Uses same modern API and data handling patterns
  - Useful for comparing IPW vs AIPW approaches

### Confidence Interval Fix (2025-07-22) - v1.0.110
- **Fixed margot_plot_qini CI computation**: Updated to handle both naming conventions
  - Now checks for both new names (cate/ate) and old names (treatment/baseline)
  - Fixes issue where CIs weren't computed when using modernized maq API
  - Ensures backward compatibility with older models

### QINI Gain Calculation Fixes (2025-07-22) - v1.0.110
- **Fixed extract_qini_data_binary()**: Now uses actual gain values for both CATE and ATE curves
  - Removed linear approximation for ATE that caused discrepancies
  - Ensures plotted values match `maq::average_gain()` results
  - Properly handles gain vector length differences through interpolation
- **Fixed margot_flip_forests() QINI recalculation**: Now correctly flips outcome values
  - Previously used original (non-flipped) outcomes, causing incorrect QINI curves
  - Now applies outcome flipping: `flipped_outcome_data <- -outcome_data`
  - Ensures consistency between flipped models and their QINI calculations
- **Added margot_qini_diagnostic()**: New diagnostic function to compare QINI gains
  - Compares gains from plot data, direct maq calculations, and diff summaries
  - Helps identify discrepancies between different calculation methods
  - Flags differences above specified tolerance threshold
  - Supports both old (treatment/baseline) and new (cate/ate) naming conventions

### QINI Visualization Fixes (2025-07-22) - v1.0.110 (continued)
- **Fixed ATE line in QINI plots**: 
  - ATE (Average Treatment Effect) line now correctly displays as straight
  - Previously showed jagged line due to using raw gain values from maq
  - Now computes linear gains: `gain = proportion * ATE` for the baseline curve
  - Ensures visual clarity that ATE represents constant treatment effect
- **Fixed CI coverage at plot extremes**:
  - Extended CI computation range from 0.05-0.95 to 0.01-0.99
  - Added endpoint extrapolation for 0% and 100% coverage
  - Ensures confidence ribbons span the full plot range
  - Fixed missing CIs at plot boundaries

### Enhanced maq::plot() Compatibility (2025-07-22) - v1.0.110 (continued)
- **Added horizontal_line parameter** (default TRUE):
  - Extends Qini curves horizontally when path is complete
  - Consistent with maq::plot() behavior for complete budget paths
  - Automatically detects complete paths from qini objects
- **Added grid_step parameter** (default auto-calculated):
  - Subsamples curve data for better performance with large datasets
  - Default: max(floor(nrow(data) / 1000), 1)
  - Ensures last point of each curve is always included
- **Added return_data parameter** (default FALSE):
  - When TRUE, returns data.frame instead of ggplot object
  - Output format matches maq::plot(): proportion, gain, lower, upper, curve
  - Enables custom plotting and further analysis
- **Enhanced std.err extraction**:
  - Now extracts std.err directly from maq objects when available
  - Provides additional consistency with maq package
  - Uses interpolation to match proportions with main data

### Bug Fix for margot_flip_forests() (2025-07-22) - v1.0.110 (continued)
- **Fixed missing W component error**:
  - `margot_causal_forest()` now saves treatment assignment vector `W` when `save_data = TRUE`
  - Applies to both regular and parallel versions
  - Fixes error: "model_results must contain 'data', 'covariates', and 'W' components"
  - Updated documentation to reflect that W is included in saved output
  - Ensures `margot_flip_forests()` can perform full model recomputation

### Debugging Tools Added (2025-07-22) - v1.0.110 (continued)
- **Added margot_debug_qini() function**:
  - Helps diagnose issues with QINI curves
  - Analyzes ATE straightness and deviations
  - Shows raw maq object data
  - Creates diagnostic plots
  - Useful for troubleshooting QINI visualization issues

### QINI Plot Improvements (2025-07-22) - v1.0.110 (continued)
- **Fixed CI computation error when only CATE exists**:
  - Added check for empty data before accessing proportion values
  - Fixed case sensitivity issue (cate vs CATE) in curve naming
  - Now properly handles single-curve scenarios
  - Fixed interpolation error when insufficient std.err values
- **Fixed CI centering issue**:
  - CIs now properly centered on actual gain values from qini_data
  - Uses std.err from maq::average_gain() but re-centers on plotted values
  - Fixes issue where CI bands appeared disconnected from the curves
- **Improved outcome variable handling**:
  - Now accepts outcome variables with or without "model_" prefix
  - Automatically tries both versions before failing
- **Updated color scheme for binary treatments**:
  - CATE curve: blue (#4f88c6)
  - ATE curve: gold (#d8a739)
  - Replaced Okabe-Ito palette for binary plots with fixed colors
  - Fixed color/fill scale consistency so lines and ribbons match
  - Multi-arm treatments still use Okabe-Ito palette
- **Reverted to maq-style jagged curves**:
  - Removed forced straight line for ATE
  - Now shows actual gain paths as computed by maq
  - Maintains consistency with maq::plot() behavior

### New Functions (2025-07-22) - v1.0.110 (continued)
- **Added margot_plot_qini_batch()**:
  - Batch process and plot QINI curves for multiple models
  - Supports all parameters from margot_plot_qini()
  - Handles model names with or without "model_" prefix
  - Automatically filters to models with QINI data
  - Saves plots to specified directory (default: "qini_plots")
  - Returns list of ggplot objects for further customization
  - Consistent with margot_plot_rate_batch() design

### Bug Fixes (2025-07-22) - v1.0.110 (continued)
- **Fixed margot_flip_forests() combined_table error**:
  - Fixed "Couldn't find a point-estimate column" error
  - Function was incorrectly passing entire results object to margot_correct_combined_table()
  - Now properly rebuilds combined_table from merged results (original + flipped)
  - Correctly extracts custom_table from each model result
  - Maintains proper row names without "model_" prefix

### New Features (2025-07-23) - v1.0.111
- **Enhanced margot_flip_forests() with automatic model removal**:
  - Added `remove_original` parameter (default TRUE) to automatically remove original models after flipping
  - When TRUE, only flipped models with "_r" suffix remain in results
  - When FALSE, both original and flipped models are kept (old behavior)
  - Helps reduce memory usage and simplifies downstream analyses
  - Provides clearer outputs in tables and plots without duplicate entries
  - Removal works for both results and full_models lists
  - Note: The parallel version (margot_flip_forests_parallel) modifies models in place rather than creating new ones

### Bug Fixes (2025-07-23) - v1.0.111 (continued)
- **Fixed double "model_" prefix in margot_flip_forests()**:
  - Removed duplicate prefix addition that caused results like "model_model_t2_anxiety_z_r"
  - margot_causal_forest() already adds "model_" prefix, so margot_flip_forests() now uses the names as-is
  - Results now correctly show as "model_t2_anxiety_z_r" instead of "model_model_t2_anxiety_z_r"

### QINI Interpretation Improvements (2025-07-23) - v1.0.111 (continued)
- **Enhanced QINI curve explanations**:
  - Clarified that QINI compares "targeted treatment allocation" vs "uniform allocation (based on average treatment effect)"
  - Added explanation about expected differences at 100% spend due to out-of-sample cross-validation
  - Added note about CATE/ATE paradox: CATE-based targeting may show benefits even when average treatment effect is unreliable
  - Improved clarity for interpreting heterogeneous treatment effects

### Still To Do
- Implement GRF scores method as alternative to regression forest
- Add comprehensive tests for new functionality
- Remove deprecated group_tab function from margot_plot.R
- Test confidence interval functionality
- Update documentation for CI features

### Infrastructure
- Created summary directory for development documentation
- Updated NEWS.md with version changes