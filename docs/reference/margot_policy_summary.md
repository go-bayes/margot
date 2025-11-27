# Collect and adjust policy-value tests, focusing on desired depths

Combines \[margot_collect_policy_values()\] and
\[margot_adjust_policy_p()\] into a single summary step. Keep only
outcomes where the learned policy improves on treat-all

## Usage

``` r
margot_policy_summary(cf_out, depths = 2L, adjust = "bonferroni", alpha = 0.05)
```

## Arguments

- cf_out:

  margot result list (after running batch helper).

- depths:

  integer vector. Depths to include. Default 2.

- adjust:

  character. Multiplicity adjustment method. Default "bonferroni"; other
  options include "holm" or any supported by \[stats::p.adjust()\].

- alpha:

  numeric. Significance threshold after adjustment. Default 0.05.

## Value

Adjusted summary \`tibble\` filtered to requested depths.
