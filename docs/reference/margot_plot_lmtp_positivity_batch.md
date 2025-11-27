# Batch plot/report LMTP positivity for multiple analyses

Convenience wrapper to run \`margot_report_lmtp_positivity()\` and
\`margot_plot_lmtp_positivity()\` across a list of analyses.

## Usage

``` r
margot_plot_lmtp_positivity_batch(
  analyses,
  label_mapping = NULL,
  digits = 2,
  include_plots = TRUE,
  ymax = NULL
)
```

## Arguments

- analyses:

  A list of spec lists, each with \`x\`, \`outcome\`, optional
  \`shifts\`, and optional \`name\`.

- label_mapping:

  Optional label map passed through to reporters/plots.

- digits:

  Integer rounding for numeric outputs.

- include_plots:

  Logical; include overlap grid in the report.

- ymax:

  Optional numeric for histogram y-axis alignment in overlap grid.

## Value

A named list; each element contains \`report\`, \`plots\`.
