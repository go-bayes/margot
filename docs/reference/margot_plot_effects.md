# Publication forest figure of estimated effects

Draws estimated effects and their confidence intervals as a horizontal
forest figure built to a journal column width: a label column, one
interval panel per contrast, optional decimal-aligned numeric columns,
and a footer with one axis title and a key. Every dimension is derived
from the content and the target width, so the same call works across
studies with different numbers of outcomes, label lengths, contrasts and
scales.

## Usage

``` r
margot_plot_effects(
  data,
  xlab,
  plot = c("estimate", "raw"),
  columns = character(),
  column_titles = list(),
  family_title = NULL,
  group_titles = NULL,
  order = c("estimate", "given"),
  contrast_titles = NULL,
  key_labels = NULL,
  width_mm = 120,
  size_pt = 6.5,
  label_max_mm = 45,
  stripe = TRUE,
  digits = NULL,
  margin_mm = 1,
  family = NULL
)
```

## Arguments

- data:

  Tidy effects table, usually from
  [`margot_lmtp_effects()`](https://go-bayes.github.io/margot/reference/margot_lmtp_effects.md).
  Required columns: `outcome`, `estimate`, `lower`, `upper` and
  `conf_level`. Optional columns: `label`, `contrast`, `type`
  (`"difference"`, the default, or `"ratio"`), `group`, `raw_estimate`,
  `raw_lower`, `raw_upper` (differences in original units),
  `mean_reference`, `mean_intervention`, `evalue` and `evalue_bound`.
  Tables from other estimators can be supplied in the same format.

- xlab:

  Axis title for the interval panels. A line break in the string starts
  a second line.

- plot:

  Plotted quantity: `"estimate"` or `"raw"` (the original-unit columns).

- columns:

  Numeric columns to the right of the panels, in order: any of
  `"plotted"` (the plotted quantity), `"estimate"`, `"raw"`, `"means"`
  (reference and intervention means) and `"evalue"` (point and interval
  E-values).

- column_titles:

  List with one `c(title, subtitle)` per column; `NULL` entries use the
  defaults. `"{ci}"` is replaced by the interval label (for example
  `"99.58% CI"`), and a subtitle written `"a|b"` puts one sub-heading
  over each number.

- family_title:

  Title above the label column, for example `"Outcome"`.

- group_titles:

  Named character vector mapping values of `group` to display titles, in
  display order.

- order:

  `"estimate"` (descending within each group, by the first contrast's
  plotted estimate) or `"given"` (row order of `data`).

- contrast_titles:

  Named character vector of titles for the contrasts, used when there is
  more than one.

- key_labels:

  Labels for the filled and open points. Default `"Interval excludes 0"`
  and `"Interval includes 0"` (1 for ratios); `NULL` omits the key.

- width_mm:

  Final figure width in millimetres, margin included. Nature uses 89,
  120 and 183 mm; Science and PNAS use similar column widths.

- size_pt:

  Type size in points at print size.

- label_max_mm:

  Widest label column before labels wrap.

- stripe:

  Shade alternate rows.

- digits:

  Decimals for the estimate and original-unit columns; `NULL` chooses
  them as described above.

- margin_mm:

  White margin inside `width_mm`.

- family:

  Font family; `NULL` chooses as described above.

## Value

A patchwork object. Attributes: `size_mm` (final width and height),
`margin_mm`, `layout_mm` (widths of the label column, each interval
panel and each numeric column), `axis` (breaks, data range, limits and
smallest gap between tick labels in mm), `font` (family, font file and
its MD5 checksum) and `plotted` (the drawn estimate and interval for
each outcome and contrast, top to bottom, with `excludes_null`, which
sets the filled point). Save it with
[`margot_save_effects_plot()`](https://go-bayes.github.io/margot/reference/margot_save_effects_plot.md),
which uses `size_mm`.

## Details

Points are estimates and horizontal lines are the intervals in `data`. A
filled point marks an interval that excludes the null (0 for
differences, 1 for ratios) and an open point one that includes it, so
status does not depend on colour. Ratios are drawn on a log axis. The
axis breaks bracket every interval and the null, are chosen so that
adjacent tick labels stay at least 2.5 mm apart at the stated width, and
the call stops rather than draw a figure that would cut off part of an
interval.

Numbers in the columns are rounded for display only. The number of
decimals is the fewest, from two, at which no limit of an interval that
excludes the null prints as the null itself; `digits` overrides it. Each
character is placed from its unkerned advance width, so decimal points
align exactly down each column, and the text stays live in the PDF.

Type is set at `size_pt` (default 6.5 pt) in Arial when it is installed,
falling back to Helvetica, Liberation Sans or DejaVu Sans with a
warning. Text widths, and so the layout, depend on the font file, which
is recorded on the returned object.

## See also

[`margot_lmtp_effects()`](https://go-bayes.github.io/margot/reference/margot_lmtp_effects.md),
[`margot_save_effects_plot()`](https://go-bayes.github.io/margot/reference/margot_save_effects_plot.md)

## Examples

``` r
effects <- data.frame(
  outcome = c("y_warmth", "y_trust", "y_belonging"),
  label = c("Warmth", "Trust in others", "Sense of belonging"),
  estimate = c(0.12, -0.03, 0.05),
  lower = c(0.07, -0.08, -0.01),
  upper = c(0.17, 0.02, 0.11),
  conf_level = 1 - 0.05 / 3
)
fig <- margot_plot_effects(effects, xlab = "Difference in mean outcome, SD units",
  columns = "estimate", family_title = "Outcome", width_mm = 120)
attr(fig, "size_mm")
#>     width    height
#> 120.00000  28.96041
```
