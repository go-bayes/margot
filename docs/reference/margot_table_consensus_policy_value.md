# Manuscript-ready table for consensus policy value

Produces a compact table (data.frame) summarizing the consensus policy
value for each model and selected depth, relative to both treat-all and
control-all baselines, with 95

## Usage

``` r
margot_table_consensus_policy_value(
  object,
  model_names = NULL,
  depth = 2L,
  R = 499L,
  seed = 42L,
  ci_level = 0.95,
  report_df = NULL,
  label_mapping = NULL
)
```

## Arguments

- object:

  Stability result from \[margot_policy_tree_stability()\].

- model_names:

  Optional outcomes to include (with or without \`model\_\`).

- depth:

  Integer; default 2.

- R, seed, ci_level:

  Passed to \[margot_report_consensus_policy_value()\].
