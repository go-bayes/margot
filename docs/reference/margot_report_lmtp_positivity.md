# One-stop LMTP positivity/overlap reporting for an analysis

Builds manuscript-ready positivity diagnostics (overall and by-wave) and
an optional density-ratio grid for a selected outcome and set of shifts.

## Usage

``` r
margot_report_lmtp_positivity(
  x,
  outcome,
  shifts = NULL,
  label_mapping = NULL,
  digits = 2,
  include_plots = TRUE,
  ymax = NULL
)
```

## Arguments

- x:

  LMTP run output with \`\$models\` (e.g., result from
  \`margot_lmtp()\`), or a list compatible with
  \`margot_lmtp_positivity()\`.

- outcome:

  Character; outcome name to report (required).

- shifts:

  Optional character vector of shifts/policies to include; accepts full
  names (\`t5_pwi_z_shift_up\`) or cleaned (\`shift_up\`). If NULL,
  includes all available shifts for the outcome.

- label_mapping:

  Optional named list to prettify estimand labels via
  \`transform_label()\`; if missing, minimal cleaning is applied.

- digits:

  Integer rounding for numeric outputs.

- include_plots:

  Logical; if TRUE, returns \`overlap_grid\`.

- ymax:

  Optional numeric; if supplied, sets common y-limit for histogram
  counts in the density-ratio grid; otherwise a common limit is
  inferred.

## Value

A list with elements: - overall: tibble with Estimand, N, Prop_zero,
ESS, ESS/N - by_wave_ess_frac: tibble wide (Wave x Estimand) with
ESS/N - by_wave_ess: tibble wide (Wave x Estimand) with ESS - flags:
tibble of positivity flags (ESS+/(N+) removed) - overlap_grid: patchwork
object (if include_plots = TRUE) - text_summary: brief prose summary

## Details

\- Computes summaries via \`margot_lmtp_positivity()\` - Produces
overall table (Estimand, N, Prop_zero, ESS, ESS/N) - Produces combined
by-wave tables (ESS/N by wave; ESS by wave) - Optionally returns a
wave-by-shift density-ratio grid using \`margot_lmtp_overlap()\` and
\`margot_lmtp_overlap_plot_grid()\` with harmonised or user-specified
histogram heights
