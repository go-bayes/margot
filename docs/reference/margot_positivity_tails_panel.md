# Detailed quantiles and tails (all vs positive-only)

For a given outcome/shift and wave (or overall), return a compact table
of quantiles and tail probabilities from \`margot_lmtp_positivity()\`
side-by-side for all weights vs positive-only weights.

## Usage

``` r
margot_positivity_tails_panel(
  pos,
  outcome,
  shift,
  wave = "overall",
  digits = 3
)
```

## Arguments

- pos:

  A result from \`margot_lmtp_positivity()\`.

- outcome:

  Character. Outcome name to filter.

- shift:

  Character. Shift/policy name. Can be the full shift name (e.g.,
  "t5_pwi_z_shift_up") or the cleaned suffix (e.g., "shift_up").

- wave:

  Character or numeric. A specific wave index or "overall".

- digits:

  Optional integer; if provided, round numeric columns.

## Value

A list with two elements: - header: one-row tibble with outcome, shift,
wave, n, n_pos, prop_zero, prop_nonzero, ess, ess_frac, ess_pos,
ess_pos_frac - panel: tibble with columns metric, all, pos, diff (pos -
all)
