# Build Depth Comparison Report

Internal helper to generate a transparent depth comparison report
showing both depth-1 and depth-2 policy values for all outcomes.

## Usage

``` r
.build_depth_comparison_report(
  depth_summary_df,
  threshold,
  label_mapping = NULL
)
```

## Arguments

- depth_summary_df:

  Data frame from margot_policy_summary_compare_depths

- threshold:

  The min_gain_for_depth_switch threshold used

- label_mapping:

  Optional label mapping

## Value

A list with text (formatted report) and data (summary data frame)
