# Assemble a full LMTP positivity report (table, diagnostics, text, plot, methods)

Bundles the per-shift diagnostics, compact summary table, interpretive
text, overlap plot, and a ready-to-drop-in methods paragraph into a
single object. Designed to streamline Quarto sections where you want
consistent reporting across multiple shift functions.

Convenience wrapper that accepts a single LMTP fit (exposing
\`\$density_ratios\`) and forwards it to \[margot_positivity_report()\]
after coercing it into the expected nested structure.

## Usage

``` r
margot_positivity_report(
  x,
  outcome = NULL,
  shifts = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  test_thresholds = list(prod_log10 = -1, prod_frac_ok = 0.05, prod_frac_warn = 0.2,
    near_zero_median = 0.001, near_zero_cv = 0.05),
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

margot_positivity_report_single_model(
  x,
  outcome,
  shift = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  test_thresholds = list(prod_log10 = -1, prod_frac_ok = 0.05, prod_frac_warn = 0.2,
    near_zero_median = 0.001, near_zero_cv = 0.05),
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

  Optional character outcome name. When \`NULL\`, the first stored
  outcome is used.

- shifts:

  Optional character vector of shifts to include (full or cleaned
  names). If \`NULL\`, all available shifts are used.

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

- shift:

  Optional name for the single shift/policy. Defaults to \`x\$shift\`
  when present, otherwise \`"(shift)"\`.

## Value

A named list with elements: - \`summary_table\`: tibble/data.frame from
\`margot_positivity_summary()\`. - \`diagnostics\`: list returned by
\`margot_lmtp_weight_diag_from_fit()\`. - \`overlap_plot\`: plot object
(or \`NULL\` when \`include_plot = FALSE\`). - \`narrative\`: structured
list from \`margot_interpret_lmtp_positivity(return = "list")\`. -
\`method_statement\`: single character string describing the analytic
approach. - \`metadata\`: list of context (outcome, shifts, waves,
thresholds).
