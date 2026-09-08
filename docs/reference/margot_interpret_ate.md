# Interpret Average Treatment Effect Estimates

Return the prose produced by \[margot_plot()\], using the same
estimates, intervals, ordering and transformation metadata as its figure
and table.

## Usage

``` r
margot_interpret_ate(.data, ..., scale_info = NULL)
```

## Arguments

- .data:

  A combined estimate table accepted by \[margot_plot()\].

- ...:

  Additional arguments to \[margot_plot()\], including ordering, labels,
  multiplicity adjustment and plotting options.

- scale_info:

  Outcome transformation metadata accepted by \[margot_plot()\]. Supply
  the constants used during preparation when reporting standardised
  outcomes in measurement units.

## Value

A character string containing the formatted interpretation.

## Details

Arguments in \`...\` retain their \[margot_plot()\] behaviour, including
the evidence threshold controlling which estimates enter the prose. If
\`save_output = TRUE\`, the complete reporting list is saved.

## See also

\[margot_plot_ate()\], \[margot_table_ate()\], \[margot_plot()\]
