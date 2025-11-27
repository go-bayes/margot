# Compact positivity panel (all vs positive-only)

Produces a compact table of positivity diagnostics for a selected
outcome/shift from \`margot_lmtp_positivity()\`, showing both the
all-weights summary and the positive-only (uncensored) summary.

## Usage

``` r
margot_positivity_panel(
  pos,
  outcome = NULL,
  shift = NULL,
  include_overall = TRUE,
  digits = 3
)
```

## Arguments

- pos:

  A result from \`margot_lmtp_positivity()\` (list with by_wave,
  overall, flags).

- outcome:

  Optional character. If provided, filter to this outcome.

- shift:

  Optional character. If provided, filter to this shift/policy.

- include_overall:

  Logical; if TRUE, include pooled "overall" rows.

- digits:

  Optional integer; if provided, round numeric columns.

## Value

A data.frame (tibble if available) with a compact positivity panel.

## Details

The panel includes, when available: censoring mass, ESS and ESS/N,
positive-only counts and ESS+/N+, and tail probabilities for chosen
thresholds (both all and positive-only).
