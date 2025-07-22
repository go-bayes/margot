# CHANGELOG_DEV.md

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

### Still To Do
- Implement GRF scores method as alternative to regression forest
- Add comprehensive tests for new functionality
- Remove deprecated group_tab function from margot_plot.R
- Test confidence interval functionality
- Update documentation for CI features

### Infrastructure
- Created summary directory for development documentation
- Updated NEWS.md with version changes