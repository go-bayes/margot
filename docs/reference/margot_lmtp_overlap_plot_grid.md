# Arrange LMTP overlap ratio_plots into a grid (waves x shifts)

Given the \`ratio_plots\` returned from \`margot_lmtp_overlap()\`, build
a patchwork grid organized by waves (rows) and shifts (columns) for a
single outcome. Accepts either the full result list (with
\`\$ratio_plots\`) or the plot list itself. Shift names may be provided
as full names or cleaned suffixes; missing cells are filled with blank
panels.

## Usage

``` r
margot_lmtp_overlap_plot_grid(
  x,
  outcome = NULL,
  shifts = NULL,
  waves = NULL,
  ncol = NULL,
  drop_titles = TRUE,
  title = NULL,
  label_mapping = NULL,
  annotate_zeros = FALSE,
  ymax = NULL,
  annotate_graph = c("none", "waves", "shifts"),
  xlim = NULL,
  layout = c("waves_by_shifts", "shifts_by_waves"),
  ymax_harmonize = "none",
  xlim_harmonize = "none",
  headroom = 0.12,
  text_size = 3,
  annotate_wave_size = NULL,
  annotate_shift_size = NULL,
  annotate_zero_size = NULL
)
```

## Arguments

- x:

  A list as returned by \`margot_lmtp_overlap()\` or a named list of
  ggplot objects keyed as \`"outcome::shift::wave"\`.

- outcome:

  Optional character. If not supplied, inferred when only a single
  outcome is present in \`x\`. Required when multiple outcomes exist.

- shifts:

  Optional character vector of shifts to include; accepts full names
  (e.g., \`t5_pwi_z_shift_up\`) or cleaned (e.g., \`shift_up\`). If
  NULL, includes all available shifts for the outcome.

- waves:

  Optional integer vector specifying waves (e.g., \`1:5\`). If NULL,
  includes all waves found for the outcome.

- ncol:

  Optional integer for number of columns. Defaults to number of selected
  shifts.

- drop_titles:

  Logical; if TRUE (default), strip titles from individual plots and add
  a single overall title.

- title:

  Optional overall title; if NULL, a default is constructed.

- label_mapping:

  Optional named list to prettify column (shift) labels via
  \`transform_label()\` when available.

- annotate_zeros:

  Logical; if TRUE, adds "zeros: X Default is FALSE.

- annotate_graph:

  Deprecated. Shift labels are always annotated along rows and wave
  labels appear as column titles; the supplied value is ignored.

- ymax_harmonize:

  Character or named vector; controls y-axis harmonization: \`"none"\`
  (default) gives each plot independent y-scale, \`"row"\` harmonizes
  within rows, \`"column"\` harmonizes within columns, \`"global"\`
  harmonizes all plots. Can also be a named vector with custom values.

- xlim_harmonize:

  Character or named vector; controls x-axis harmonization: \`"none"\`
  (default) gives each plot independent x-scale, \`"row"\` harmonizes
  within rows, \`"column"\` harmonizes within columns, \`"global"\`
  harmonizes all plots. Can also be a named vector with custom values.

- text_size:

  Numeric size for facet annotations (wave/shift/zeros labels). Defaults
  to 3.

- annotate_wave_size:

  Numeric; overrides the size of the column wave titles when
  \`drop_titles = TRUE\`. When \`NULL\`, defaults to a slightly larger
  value than \`text_size\` for readability.

- annotate_shift_size:

  Numeric; controls the size of shift annotations inside each panel.

- annotate_zero_size:

  Numeric; controls the size of zero-percentage annotations.

## Value

A patchwork object combining the selected plots.
