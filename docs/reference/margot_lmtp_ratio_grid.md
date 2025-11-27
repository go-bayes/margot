# LMTP density-ratio panel (per-wave grid)

Build a grid of density-ratio histograms across waves for a selected
outcome and shift/policy from an LMTP run. Plots use either log10 or
linear scale on the x-axis and annotate the proportion of zeros.

## Usage

``` r
margot_lmtp_ratio_grid(
  x,
  outcome,
  shift,
  waves = NULL,
  scale = "log10",
  theme = "classic",
  bins = 40,
  ncol = 3,
  verbose = TRUE
)
```

## Arguments

- x:

  LMTP run output with \`\$models\` (e.g., result from
  \`margot_lmtp()\`), or a single LMTP model (has \`\$density_ratios\`),
  or a list of such models.

- outcome:

  Character. Outcome name to plot.

- shift:

  Character. Shift/policy name. May be the full name (e.g.,
  \`t5_pwi_z_shift_up\`) or the cleaned suffix (e.g., \`shift_up\`).

- waves:

  Optional integer vector of wave indices to include. If NULL, includes
  all available waves.

- scale:

  Character; \`"log10"\` (default) or \`"linear"\` for the x-axis.

- theme:

  ggplot2 theme keyword: \`"classic"\` (default), \`"minimal"\`,
  \`"bw"\`, \`"gray"\`, \`"light"\`, \`"dark"\`, \`"void"\`.

- bins:

  Integer; histogram bins (default 40).

- ncol:

  Integer; number of columns in the grid (default 3).

- verbose:

  Logical; emit informative messages.

## Value

A patchwork plot object (ggplot) composing per-wave histograms.
