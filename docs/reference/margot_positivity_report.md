# Assemble a full LMTP positivity report

Bundles the LMTP positivity summary table, density-ratio diagnostics,
interpretive text, overlap plot, and a ready-to-drop-in methods
paragraph into a single object for Quarto or reporting workflows.

## Usage

``` r
margot_positivity_report(
  x,
  outcome,
  shifts = NULL,
  label_mapping = NULL,
  waves = NULL,
  remove_waves = NULL,
  test_thresholds = list(prod_log10 = -1, prod_frac_warn = 0.1, near_zero_median = 0.001,
    near_zero_cv = 0.05),
  include_policy_rates = TRUE,
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
  accepted by the positivity helpers.

- outcome:

  Character outcome name.

- shifts:

  Optional character vector of shifts to include (full or cleaned
  names). If \`NULL\`, all shifts are used.

- label_mapping:

  Optional label map passed to downstream helpers.

- waves:

  Optional integer vector selecting waves.

- remove_waves:

  Optional integer vector of waves to drop after subsetting.

- test_thresholds:

  Named list of thresholds forwarded to the summary and narrative
  functions.

- include_policy_rates:

  Logical; whether to compute policy-rate columns and include the
  policy-rate narrative.

- effect_table:

  Optional effect table merged into the summary.

- digits:

  Integer rounding applied to numeric outputs.

- trim_right:

  Numeric in \`(0, 1\]\`; passed to
  \`margot_lmtp_weight_diag_from_fit()\`.

- thresholds:

  Numeric vector of ratio thresholds for the diagnostics.

- summary_compact:

  Logical; request the compact summary table.

- include_plot:

  Logical; whether to build the overlap plot.

- plot_args:

  Named list overriding defaults in \`margot_plot_lmtp_overlap_grid()\`.

- interpret_args:

  Named list overriding defaults in
  \`margot_interpret_lmtp_positivity()\`.

## Value

A named list containing the summary table, diagnostics list, optional
plot, structured narrative, methods paragraph, per-wave summary table,
and metadata (including a censoring summary) describing the request.
