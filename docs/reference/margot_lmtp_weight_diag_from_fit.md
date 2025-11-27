# Density-ratio diagnostics from an LMTP fit

Constructs per-wave density-ratio summaries and cumulative effective
sample sizes (ESS) directly from an LMTP model object. For incremental
propensity score interventions (IPSI) with \\\delta \> 1\\, zeros in the
density ratios arise solely from dropout, so the helper reconstructs the
observation mask via \`density_ratios \> 0\`, computes per-wave
quantiles/tail probabilities, applies optional right-tail winsorisation,
and reports ESS for the cumulative weight trajectories (raw and
trimmed).

## Usage

``` r
margot_lmtp_weight_diag_from_fit(
  fit,
  outcome = NULL,
  shift = NULL,
  shifts = NULL,
  trim_right = 0.999,
  thresholds = c(5, 10, 25, 50, 100),
  label_mapping = NULL
)
```

## Arguments

- fit:

  Either a single LMTP model (anything exposing \`\$density_ratios\`) or
  a full \`margot_lmtp()\` result.

- outcome:

  Optional outcome name (required when \`fit\` is a full
  \`margot_lmtp()\` run).

- shift:

  Optional shift name (clean suffix or full). Required when \`fit\` is a
  full \`margot_lmtp()\` run with multiple shifts. Ignored when
  \`shifts\` is supplied.

- shifts:

  Optional character vector of shifts to summarise in a single call.
  When supplied, diagnostics are returned in the same order. Defaults to
  all available shifts only when a single shift exists in the fit.

- trim_right:

  Numeric in \`(0, 1\]\`; optional right-tail winsorisation level
  applied per wave before forming cumulative products (default
  \`0.999\`).

- thresholds:

  Numeric vector of ratio thresholds used when computing tail mass
  (\`Pr(r_t \> a)\`).

- label_mapping:

  Optional named list for relabelling wave indices (e.g., via
  \`label_mapping\$wave_labels\` or \`label_mapping\$wave_1 =
  "Baseline"\`). Mirrors the conventions used by
  \`margot_interpret_lmtp_positivity()\`.

## Value

For a single shift, a list with \`wave_table\` (per-wave diagnostics),
\`mask_from_fit\` (logical observation mask), \`density_ratios\`,
cumulative weights (\`w_cum_raw\`, \`w_cum_trim\`), and shift metadata.
When multiple shifts are requested, a named list of such objects (one
per shift) is returned.

## Examples

``` r
dr <- matrix(c(2, 0, 1,
               1.5, 0.5, 2), nrow = 2, byrow = TRUE)
fit <- list(density_ratios = dr)
diagnostics <- margot_lmtp_weight_diag_from_fit(fit)
diagnostics$wave_table
#>   wave wave_label n_obs prop_censored ess_pos ess_pos_frac ess_pos_frac_pt
#> 1    1     Wave 1     2           0.0    1.96         0.98            0.98
#> 2    2     Wave 2     1           0.5    1.00         1.00            0.50
#> 3    3     Wave 3     2           0.0    1.80         0.90            0.90
#>   r_q50 r_q90 r_q95 r_q99 r_q999 rtrim_q99 rtrim_q999 frac_gt_5 frac_gt_10
#> 1  1.75  1.95 1.975 1.995 1.9995  1.994505   1.999001         0          0
#> 2  0.50  0.50 0.500 0.500 0.5000  0.500000   0.500000         0          0
#> 3  1.50  1.90 1.950 1.990 1.9990  1.989010   1.998001         0          0
#>   frac_gt_25 frac_gt_50 frac_gt_100 ess_cum_raw ess_cum_trim
#> 1          0          0           0        1.96     1.960067
#> 2          0          0           0        1.00     1.000000
#> 3          0          0           0        1.00     1.000000
```
