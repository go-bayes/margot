# Plot LMTP density-ratio grid (waves x shifts)

Thin wrapper around \`margot_lmtp_overlap()\` and
\`margot_lmtp_overlap_plot_grid()\` that returns a panel grid with
optional harmonised or user-specified y-axis limits for histogram
counts.

## Usage

``` r
margot_plot_lmtp_overlap_grid(
  x,
  outcome,
  shifts = NULL,
  label_mapping = NULL,
  show_censored = FALSE,
  scale = "linear",
  theme = "empty",
  ymax = NULL,
  digits = 3,
  annotate_graph = c("none", "waves", "shifts"),
  annotate_zeros = FALSE,
  waves = NULL,
  xlim = NULL,
  layout = c("waves_by_shifts", "shifts_by_waves"),
  ymax_harmonize = "none",
  xlim_harmonize = "none",
  headroom = 0.12,
  color_by = c("wave", "shift", "constant"),
  color_by_wave = NULL,
  fill_palette = NULL,
  text_size = 3,
  annotate_wave_size = NULL,
  annotate_shift_size = NULL,
  annotate_zero_size = NULL,
  bins = 40,
  binwidth = NULL
)
```

## Arguments

- x:

  LMTP run output with \`\$models\` or any input accepted by
  \`margot_lmtp_overlap()\`.

- outcome:

  Character outcome to plot (required).

- shifts:

  Optional character vector of shifts to include (full or cleaned
  names).

- label_mapping:

  Optional label map for pretty titles.

- show_censored:

  Logical; if FALSE (default), histograms exclude zeros (r = 0). If
  TRUE, includes zeros. Censoring rate always shown in titles.

- scale:

  Character, "log10" or "linear" for the ratio scale (passed to
  \`margot_lmtp_overlap()\`).

- theme:

  Character ggplot theme keyword passed to \`margot_lmtp_overlap()\`.

- ymax:

  Optional numeric y-axis maximum for histogram counts across panels
  (passed to \`margot_lmtp_overlap_plot_grid()\`).

- digits:

  Integer rounding for summaries (not used in plot aesthetics).

- annotate_graph:

  Deprecated. Shift labels are always annotated along rows and wave
  labels appear as column titles; the supplied value is ignored.

- annotate_zeros:

  Logical; if TRUE, adds "zeros: X Default is FALSE.

- waves:

  Optional integer vector specifying which waves to include (e.g.,
  \`c(1, 2, 3)\`). If NULL, includes all waves found for the outcome.

- ymax_harmonize:

  Character or named vector; controls y-axis harmonization: \`"none"\`
  (default) gives each plot independent y-scale, \`"row"\` harmonizes
  within rows, \`"column"\` harmonizes within columns, \`"global"\`
  harmonizes all plots. Can also be a named vector with custom values
  (e.g., \`c(wave_1 = 1000)\`).

- xlim_harmonize:

  Character or named vector; controls x-axis harmonization: \`"none"\`
  (default) gives each plot independent x-scale, \`"row"\` harmonizes
  within rows, \`"column"\` harmonizes within columns, \`"global"\`
  harmonizes all plots. Can also be a named vector with custom values.

- color_by:

  Character; how histogram fills are coloured (\`"wave"\`, \`"shift"\`,
  or \`"constant"\`).

- color_by_wave:

  Legacy logical alias for \`color_by\` (\`TRUE\` = \`"wave"\`,
  \`FALSE\` = \`"constant"\`).

- fill_palette:

  Optional vector of colours (named or unnamed) used when colouring
  histograms.

- text_size:

  Numeric size for facet annotations (wave/shift/zeros labels). Defaults
  to 3.

- annotate_wave_size:

  Numeric; overrides the size of wave titles across columns. When
  \`NULL\`, inherits the default determined inside
  \[margot_lmtp_overlap_plot_grid()\].

- annotate_shift_size:

  Numeric; controls the size of shift annotations inside each panel.

- annotate_zero_size:

  Numeric; controls the size of zero-percentage annotations.

## Value

A patchwork grid object.

## Details

By default, histograms show only \*\*uncensored observations\*\* (r \>
0) as zeros primarily reflect dropout/censoring. Use \`show_censored =
TRUE\` to include zeros.
