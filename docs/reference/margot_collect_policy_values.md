# Collect policy-value summaries

Collect policy-value summaries

## Usage

``` r
margot_collect_policy_values(cf_out, depths = c(1L, 2L))
```

## Arguments

- cf_out:

  list. \*\*Normalised\*\* margot result with \`policy_value_depth\_\*\`
  slots present.

- depths:

  integer vector. Depths to include.

## Value

A \`tibble\` with one row per outcome × depth.
