# PLANNING.md

## Log Transformation Back-calculation for Policy Trees

### Current Issue
When displaying CATEs on the original scale for log-transformed outcomes, the current approach transforms control and treatment means separately, which can give misleading results. Additionally, when `original_df` contains subset data rather than full population data, the statistics used for back-transformation may not represent the true population parameters.

### Proposed Solution

For log-transformed outcomes, use the following approach:

```r
# Given:
# - treatment_effect: standardized effect size (e.g., 0.14)
# - sd_log: standard deviation of log-transformed variable
# - mean_dollars: mean of back-transformed values

# Calculate:
delta_log <- treatment_effect * sd_log  # Effect on log scale
ratio <- exp(delta_log)                 # Multiplicative effect
pct_change <- (ratio - 1) * 100         # Percentage change
abs_change <- mean_dollars * (ratio - 1) # Absolute dollar change
```

### Why This Approach is Better

1. **Statistically Appropriate**: Treats the effect as additive on log scale, which translates to multiplicative on original scale
2. **Standard Practice**: Aligns with econometric and epidemiological interpretation of log-transformed outcomes
3. **Avoids Group-Specific Bias**: Doesn't rely on separate transformations for control/treatment groups
4. **More Reasonable Results**: Gives plausible percentage changes (e.g., 47% for charity donations)

### Implementation Steps

1. **Phase 1: Update `compute_leaf_means()` function**
   - For log-transformed outcomes, calculate effect on standardized scale
   - Apply the multiplicative interpretation
   - Show both percentage change and absolute change

2. **Phase 2: Handle Data Subsetting Issues**
   - Modify `margot_log_transform_vars()` to store population statistics as attributes
   - Update `get_outcome_transformation_info()` to use stored attributes when available
   - Add warnings when subset statistics differ substantially from expected population values

3. **Phase 3: Consider Weighted Statistics**
   - Account for sample weights and IPTWs in transformation statistics
   - Allow users to specify whether to use weighted or unweighted statistics

### Example Output

Instead of:
```
CATE of 0.112 (37% increase, from $32.7 to $44.8)
```

Should show:
```
CATE of 0.112 (47% multiplicative increase, ~$490 average increase)
```

### Notes

- The differences may not be dramatic in all cases, but the interpretation is more correct
- This approach better reflects the log-linear nature of the model
- Aligns with standard practice in causal inference with log outcomes