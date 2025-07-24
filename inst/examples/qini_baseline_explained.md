# Understanding QINI Baseline Methods in margot

## Overview

QINI curves visualize treatment prioritization strategies by showing the cumulative gain from treating individuals based on their conditional average treatment effects (CATE). The baseline curve represents a "no prioritization" strategy - treating individuals at random.

## Baseline Method Options

### 1. "auto" (default)
- Tries `maq` with `target.with.covariates = FALSE` first
- Falls back to simple baseline if maq fails
- Best for most use cases

### 2. "simple"
- Always generates a straight line from (0,0) to (1, mean(tau_hat))
- Represents expected gain under random allocation: gain(B) = B × E[τ]
- **Always succeeds** - cannot fail
- Ideal when you need guaranteed results

### 3. "maq_no_covariates"
- Uses `maq` with `target.with.covariates = FALSE`
- Creates baseline using maq's optimization without covariate information
- May fail if maq encounters numerical issues

### 4. "maq_only"
- Uses standard `maq` with constant rewards
- Traditional approach but prone to failure with degenerate optimization
- May fail when all rewards are identical

### 5. "none"
- Shows only the CATE curve without any baseline
- Useful for focused visualization of treatment effects

## Handling Missing Data

When `mc_result$data` is NULL (common with saved results), margot intelligently handles baseline generation:

1. **For "simple" baseline**: Extracts ATE from multiple sources:
   - `model_result$tau_hat` (takes mean)
   - `model_result$ATE` or `model_result$ate`
   - `model_result$estimate`
   - `model_result$custom_table["E[Y(1)]-E[Y(0)]", "Estimate"]`

2. **Fallback behavior**: Adds baseline to existing CATE curves without full regeneration

3. **Error messages**: Shows available fields when ATE cannot be found

## Examples

```r
# Robust approach for saved results without data
margot_plot_qini(
  saved_results,
  outcome_var = "model_outcome",
  baseline_method = "simple"  # Guaranteed to work
)

# Try maq first, fall back if needed
margot_plot_qini(
  results,
  outcome_var = "model_outcome",
  baseline_method = "auto"
)

# Focus on CATE only
margot_plot_qini(
  results,
  outcome_var = "model_outcome",
  baseline_method = "none"
)
```

## Technical Details

The simple baseline represents the theoretical gain under random treatment allocation. At any budget B (proportion treated), the expected gain is:

**Gain(B) = B × E[τ]**

Where E[τ] is the average treatment effect. This creates a straight line from origin to (1, ATE), providing a clear benchmark for evaluating whether CATE-based prioritization improves upon random allocation.