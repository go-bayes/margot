# Tidy per-shift LMTP positivity summary

Builds a single, tidy table per shift with practical-positivity
diagnostics and optional effect columns (ATT, CI, E-value) when
supplied. Designed to compare incremental propensity score interventions
(IPSI) such as \`ipsi_02\`, \`ipsi_05\`, \`ipsi_10\` without refitting,
but works for any set of shifts available in an LMTP run object.

## Usage

``` r
margot_positivity_summary(
  x,
  outcome = NULL,
  shifts = NULL,
  waves = NULL,
  test_thresholds = list(prod_log10 = -1, prod_frac_ok = 0.05, prod_frac_warn = 0.2,
    near_zero_median = 0.001, near_zero_cv = 0.05),
  include_policy_rates = FALSE,
  effect_table = NULL,
  digits = 3,
  compact = TRUE,
  include_explanation = TRUE
)

margot_ipsi_summary(...)
```

## Arguments

- x:

  Result of \`margot_lmtp()\` (list with \`\$models\`) or a list of
  models each exposing \`\$density_ratios\` (matrix with waves in
  columns). Optionally models may include \`\$exposure_by_wave\` to
  enable policy-rate summaries.

- outcome:

  Optional character outcome name. When \`NULL\`, the first stored
  outcome is used.

- shifts:

  Character vector of shifts to include (full names or cleaned
  suffixes). If NULL, includes all available for the outcome. When
  supplied, the output preserves the order provided here.

- waves:

  Optional integer vector of waves to include; defaults to all.

- test_thresholds:

  Named list controlling tests. Recognised names: - \`prod_log10\`
  (default -1) defining the central support band \`\[10^prod_log10,
  10^-prod_log10\]\`; \`-1\` corresponds to \`\[0.1, 10\]\`. -
  \`prod_frac_ok\` (default 0.05) and \`prod_frac_warn\` (default 0.20),
  defining the graded support screen from the combined fraction outside
  the band. - \`near_zero_median\` (default 1e-3) and \`near_zero_cv\`
  (default 0.05) are computed but not included in the support screen;
  they are returned as counts of flagged waves per shift.

- include_policy_rates:

  Logical; if TRUE and exposure-by-wave aligned with density ratios is
  available, returns policy-implied Pr(A_t=1) by wave (uncensored rows).

- effect_table:

  Optional data.frame with effect columns to merge by (outcome, shift).
  Expected columns (case-insensitive, flexible names): \`outcome\`,
  \`shift\` (either full or cleaned), and any of \`att\`, \`ci_low\`,
  \`ci_high\`, \`e_value\`, \`e_value_bound\`.

- digits:

  Integer; rounding for numeric outputs.

## Value

A tibble/data.frame with one row per shift containing: \`outcome\`,
\`shift_full\`, \`shift_clean\`, cumulative density-ratio metrics,
support screen, ESS metrics (including final cumulative ESS), optional
policy rates (per-wave \`p_hat_wave_k\` and \`p_hat_overall\`), optional
effect columns if provided.

## Details

Metrics include: - Cumulative density-ratio support across selected
waves, summarised as the fraction of uncensored rows falling below and
above a user-defined central band, plus a graded support screen, percent
collapsing to zero including censoring (IPCW zeros), and the final
cumulative ESS. - ESS on uncensored rows overall and relative to
person-time (ESS+/(N_pt)). - Policy-implied exposure rates by wave (and
overall) on uncensored rows when \`exposure_by_wave\` is attached to
models (best-effort from \`margot_lmtp()\`). - Optional effect columns
merged from a user-supplied \`effect_table\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Given an LMTP run with IPSI shifts
tbl <- margot_positivity_summary(
  x = fit_ipsi,
  outcome = "t5_meaning_purpose_z",
  shifts = c("null", "ipsi_02", "ipsi_05", "ipsi_10"),
  test_thresholds = list(prod_log10 = -1, prod_frac_warn = 0.10),
  include_policy_rates = TRUE,
  digits = 3
)

# Compact table for reporting (kable-friendly headers)
tbl_compact <- margot_positivity_summary(
  x = fit_ipsi,
  outcome = "t5_meaning_purpose_z",
  shifts = c("null", "ipsi_02", "ipsi_05", "ipsi_10"),
  test_thresholds = list(prod_log10 = -1, prod_frac_warn = 0.10),
  include_policy_rates = FALSE,
  digits = 3,
  compact = TRUE,
  include_explanation = TRUE
)
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::kable(tbl_compact, format = "markdown")
}
# Print non-specialist explanation
cat(attr(tbl_compact, "explanation"), "\n")
} # }
```
