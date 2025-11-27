# Batch Recompute ATEs for Multiple Target Samples

Convenience function to recompute ATEs for multiple target samples at
once, returning a comparison table.

## Usage

``` r
margot_recompute_ate_batch(
  causal_forest_output,
  target_samples = c("all", "treated", "control", "overlap"),
  ...
)
```

## Arguments

- causal_forest_output:

  Output from \`margot_causal_forest()\` with saved models

- target_samples:

  Character vector of target samples to compute. Default is all four:
  c("all", "treated", "control", "overlap")

- ...:

  Additional arguments passed to \`margot_recompute_ate()\`

## Value

A list containing:

- \`results\` - Named list of recomputed outputs for each target sample

- \`comparison_table\` - Data frame comparing ATEs across target samples

- \`parameters\` - Parameters used for recomputation
