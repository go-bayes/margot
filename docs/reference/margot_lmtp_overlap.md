# Assess Overlap/Positivity from LMTP Models via Density Ratios

Summarises density-ratio overlap and effective sample size (ESS) for
LMTP models using \`margot_lmtp_positivity()\`, and optionally creates
diagnostic plots of the density-ratio distributions by wave and shift.

## Usage

``` r
margot_lmtp_overlap(
  x,
  outcomes = NULL,
  shifts = NULL,
  plot = TRUE,
  show_censored = FALSE,
  theme = "classic",
  scale = "log10",
  digits = 3,
  verbose = TRUE,
  color_by = c("wave", "shift", "constant"),
  color_by_wave = NULL,
  fill_palette = NULL,
  bins = 40,
  binwidth = NULL,
  xlim = NULL,
  ...
)
```

## Arguments

- x:

  LMTP run output with \`\$models\` (e.g., result from margot_lmtp()),
  or a list compatible with \`margot_lmtp_positivity()\`.

- outcomes:

  Optional character vector of outcome names to include.

- shifts:

  Optional character vector of shifts/policies to include (accepts full
  names like \`t5_pwi_z_shift_up\` or cleaned names like \`shift_up\`).

- plot:

  Logical; if TRUE, create ratio distribution plots.

- show_censored:

  Logical; if FALSE (default), histograms exclude zeros (r = 0) to focus
  on uncensored observations. If TRUE, includes zeros in the histogram.
  Censoring rate is always reported in plot titles.

- theme:

  ggplot2 theme keyword: "classic" (default), "minimal", "bw", "gray",
  "light", "dark", "void".

- scale:

  Character; x-axis scale for plotted ratios: "log10" (default) shows
  log10(density ratio) for w\>0, or "linear" shows the raw ratio.

- digits:

  Integer rounding for summaries (passed to positivity helper).

- verbose:

  Logical; emit informative messages.

- color_by:

  Character; how to colour histogram fills: \`"wave"\` (default),
  \`"shift"\` (one colour per shift), or \`"constant"\` (single colour).

- color_by_wave:

  Legacy logical alias for \`color_by\` (\`TRUE\` = \`"wave"\`,
  \`FALSE\` = \`"constant"\`).

- fill_palette:

  Optional vector of colours (named or unnamed) used when colouring
  histograms. Character aliases include \`"lab"\` (blue/red/grey with a
  constant fallback) and \`"classic"\` (the default qualitative
  palette).

- ...:

  Optional named arguments recognised by
  \`margot_interpret_lmtp_positivity()\` when assembling the text
  summary (e.g., \`label_mapping\`, \`waves\`, \`remove_waves\`,
  \`include_methods\`, \`include_diagnostics\`). Unrecognised entries
  are silently ignored for backward compatibility with deprecated
  arguments such as \`save_plots\`.

## Value

A list with: - overlap_summary: tibble combining by-wave and overall
positivity/overlap metrics - ratio_plots: list of ggplot objects (if
plot = TRUE) - flags: tibble of positivity flags - text_summary:
markdown-ready prose from \`margot_interpret_lmtp_positivity()\`

## Details

\*\*Censoring vs. Treatment Positivity:\*\* By default, histograms show
only \*\*uncensored observations\*\* (density ratios r \> 0), as zeros
primarily reflect dropout/censoring rather than treatment positivity
violations. The censoring rate (proportion r = 0) is reported in plot
titles and summaries. Use \`show_censored = TRUE\` to include zeros in
histograms (shown as a bar at r = 0).
