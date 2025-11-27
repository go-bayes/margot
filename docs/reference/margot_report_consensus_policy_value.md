# Report Policy Value for Consensus Trees

Computes and reports how much better the consensus policy trees perform
than (a) treat-all (ATE baseline) and (b) treat-none (universal
control), with 95

## Usage

``` r
margot_report_consensus_policy_value(
  object,
  model_names = NULL,
  depths = c(1L, 2L),
  R = 499L,
  seed = 42L,
  ci_level = 0.95,
  include_treated_only = FALSE,
  label_mapping = NULL,
  verbose = TRUE
)
```

## Arguments

- object:

  Object of class "margot_stability_policy_tree" produced by
  \[margot_policy_tree_stability()\]. Must contain consensus policy
  trees in \`policy_tree_depth_1\` and/or \`policy_tree_depth_2\`, plus
  \`plot_data\` and \`dr_scores\`/\`dr_scores_flipped\`.

- model_names:

  Optional character vector of model names to include (with or without
  \`model\_\` prefix). Default: all.

- depths:

  Integer vector of depths to evaluate (default \`c(1, 2)\`).

- R:

  Integer ≥ 199; number of bootstrap replicates (default 499).

- seed:

  Integer or NULL; RNG seed for reproducibility (default 42).

- ci_level:

  Confidence level for intervals (default 0.95).

- verbose:

  Logical; print progress (default TRUE).

## Value

A data.frame with one row per model × depth × contrast containing
estimate, standard error, and confidence interval.
