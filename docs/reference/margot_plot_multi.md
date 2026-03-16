# Create Multi-panel Margot Plots with Shared Axes

Create Multi-panel Margot Plots with Shared Axes

## Usage

``` r
margot_plot_multi(
  tables,
  panel_titles = names(tables),
  panel_subtitles = NULL,
  ncol = 1,
  tag_levels = "A",
  collect_guides = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  shared_x_limits = NULL,
  options = list(),
  per_panel_options = NULL,
  ...
)
```

## Arguments

- tables:

  A named list of combined tables to plot.

- panel_titles:

  Optional character vector of per-panel titles.

- panel_subtitles:

  Optional character vector of per-panel subtitles.

- ncol:

  Number of columns in the patchwork layout.

- tag_levels:

  Tag levels passed to \`patchwork::plot_annotation()\`.

- collect_guides:

  Logical; if \`TRUE\`, collect legends at the bottom.

- title:

  Optional overall patchwork title.

- subtitle:

  Optional overall patchwork subtitle.

- caption:

  Optional overall patchwork caption.

- shared_x_limits:

  Optional numeric vector of length two. If omitted, limits are computed
  across all panels.

- options:

  Base options passed to each \`margot_plot()\` call.

- per_panel_options:

  Optional list of option lists, one per panel.

- ...:

  Additional arguments passed to \`margot_plot()\`.

## Value

An invisible list containing the combined \`plot\`, the per-panel
\`panels\`, and the computed \`shared_x_limits\`.
