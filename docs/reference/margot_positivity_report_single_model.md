# Assemble a full LMTP positivity report for a single model

Convenience wrapper that accepts a single LMTP fit (exposing
\`\$density_ratios\`) and forwards it to \[margot_positivity_report()\]
after coercing it into the expected nested structure.

## Usage

``` r
margot_positivity_report_single_model(
  x,
  outcome,
  shift = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  test_thresholds = list(prod_log10 = -1, prod_frac_warn = 0.1, near_zero_median = 0.001,
    near_zero_cv = 0.05),
  include_policy_rates = FALSE,
  effect_table = NULL,
  digits = 3,
  trim_right = 0.999,
  thresholds = c(5, 10, 25, 50, 100),
  summary_compact = TRUE,
  include_plot = TRUE,
  plot_args = list(),
  interpret_args = list()
)
```

## Arguments

- x:

  Result of \`margot_lmtp()\` (with \`\$models\`) or another object
  accepted by the underlying helpers.

- outcome:

  Character outcome name.

- shift:

  Optional name for the single shift/policy. Defaults to \`x\$shift\`
  when present, otherwise \`"(shift)"\`.

- label_mapping:

  Optional label map passed through to downstream helpers.

- waves:

  Optional integer vector selecting waves.

- remove_waves:

  Optional integer vector of waves to drop after subsetting.

- test_thresholds:

  Named list of thresholds passed to \`margot_positivity_summary()\` and
  \`margot_interpret_lmtp_positivity()\`.

- include_policy_rates:

  Logical; whether to compute policy-rate columns in the summary table
  and narrative.

- effect_table:

  Optional effect table merged into the summary.

- digits:

  Integer rounding applied to numeric outputs.

- trim_right:

  Numeric in \`(0, 1\]\`; right-tail winsorisation level for diagnostics
  supplied to \`margot_lmtp_weight_diag_from_fit()\`.

- thresholds:

  Numeric vector of ratio thresholds forwarded to
  \`margot_lmtp_weight_diag_from_fit()\`.

- summary_compact:

  Logical; whether to request the compact summary table.

- include_plot:

  Logical; if \`TRUE\`, returns a \`ggplot2\`/patchwork object from
  \`margot_plot_lmtp_overlap_grid()\`.

- plot_args:

  Optional named list overriding defaults passed to
  \`margot_plot_lmtp_overlap_grid()\`.

- interpret_args:

  Optional named list overriding defaults passed to
  \`margot_interpret_lmtp_positivity()\` (e.g., \`include_tests =
  FALSE\`).
