# Plot LMTP positivity (overall and by-wave ESS/N)

Generates two ggplots from \`margot_report_lmtp_positivity()\` results:
a bar plot of overall ESS/N by estimand, and a line plot of ESS/N by
wave.

## Usage

``` r
margot_plot_lmtp_positivity(
  x,
  outcome = NULL,
  shifts = NULL,
  label_mapping = NULL,
  digits = 2,
  ymax = NULL
)
```

## Arguments

- x:

  Either a report list from \`margot_report_lmtp_positivity()\` or a
  model object acceptable to \`margot_report_lmtp_positivity()\` when
  \`outcome\` is provided.

- outcome:

  Character outcome (required if \`x\` is a model object).

- shifts:

  Optional character vector of shifts/policies (full or cleaned).

- label_mapping:

  Optional label mapping passed to the reporter.

- digits:

  Integer rounding for the reporter.

- ymax:

  Unused here; exists for interface consistency (use
  \`margot_plot_lmtp_overlap_grid()\` to adjust histogram heights).

## Value

A list with \`overall_plot\`, \`by_wave_plot\`, and the underlying
\`report\`.
