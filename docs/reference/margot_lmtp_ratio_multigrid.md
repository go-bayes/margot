# LMTP density-ratio multi-shift grid (wave x shift)

Build a facet grid of density-ratio histograms across waves (rows) and
shifts (columns) for a selected outcome from an LMTP run. Each facet
annotates the proportion of zero ratios (censoring mass).

## Usage

``` r
margot_lmtp_ratio_multigrid(
  x,
  outcome,
  shifts = NULL,
  waves = NULL,
  scale = "log10",
  theme = "classic",
  bins = 40,
  verbose = TRUE,
  label_mapping = NULL
)
```

## Arguments

- x:

  LMTP run output with \`\$models\` (e.g., result from
  \`margot_lmtp()\`), or a single LMTP model (has \`\$density_ratios\`),
  or a list of such models.

- outcome:

  Character. Outcome name to plot.

- shifts:

  Optional character vector of shifts/policies to include. Accepts full
  names (e.g., \`t5_pwi_z_shift_up\`) or cleaned suffixes (e.g.,
  \`shift_up\`, \`shift_down\`, \`null\`). If NULL, includes all shifts.

- waves:

  Optional integer vector of waves to include. If NULL, includes all.

- scale:

  Character; \`"log10"\` (default) or \`"linear"\` for the x-axis.

- theme:

  ggplot2 theme keyword: \`"classic"\` (default), \`"minimal"\`,
  \`"bw"\`, \`"gray"\`, \`"light"\`, \`"dark"\`, \`"void"\`.

- bins:

  Integer; histogram bins (default 40).

- verbose:

  Logical; emit informative messages.

- label_mapping:

  Optional named list for human-friendly labels. When provided, used via
  \`transform_label()\` if available.

## Value

A ggplot object with \`facet_grid(rows = vars(wave), cols =
vars(shift))\`.
