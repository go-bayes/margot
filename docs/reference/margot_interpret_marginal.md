# Make Interpretation of ATE Results

helper that assembles a concise markdown‑style interpretation of the
results. when \`include_adjust_note = FALSE\` (the default for a
single‑outcome call from \`margot_plot()\`), statements about
multiplicity correction are suppressed to avoid unnecessary noise.

## Usage

``` r
margot_interpret_marginal(
  df,
  type = c("RD", "RR"),
  order = c("alphabetical", "magnitude_desc", "magnitude_asc", "evaluebound_desc",
    "evaluebound_asc", "custom", "default"),
  original_df = NULL,
  e_val_bound_threshold = 1,
  adjust = c("none", "bonferroni"),
  alpha = 0.05,
  include_adjust_note = TRUE,
  effect_type = "ATE"
)
```

## Arguments

- include_adjust_note:

  logical; if \`FALSE\`, any reference to adjustment methods is omitted.
  default \`TRUE\`.

## Value

list with one element, \`interpretation\` (a character string).
