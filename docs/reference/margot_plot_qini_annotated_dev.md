# Create Annotated QINI Plot with Gain Information

Enhanced version that adds gain annotations at specified spend levels

## Usage

``` r
margot_plot_qini_annotated_dev(
  qini_results,
  outcome,
  annotate_spends = c(0.2),
  ...
)
```

## Arguments

- qini_results:

  Results from margot_qini_dev()

- outcome:

  Single outcome to plot with annotations

- annotate_spends:

  Numeric vector of spend levels to annotate

- ...:

  Additional arguments passed to margot_plot_qini_dev()

## Value

A ggplot2 object with annotations
