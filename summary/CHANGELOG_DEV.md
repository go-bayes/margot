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

### Cleanup Completed
- Removed `margot_flip_forests_dev()` and its artifacts since functionality exists in `margot_recalculate_policy_trees()`

### Still To Do
- Add comprehensive tests for new functionality

### Infrastructure
- Created summary directory for development documentation
- Updated NEWS.md with version changes