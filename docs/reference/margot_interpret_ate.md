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

Arguments in \`...\` retain their \[margot_plot()\] behaviour. By
default the prose lists every outcome whose (adjusted) confidence
interval excludes the null, and \`e_val_bound_threshold\` only sets the
E-value flag reported beside each estimate; with \`category_rule =
"interval_and_evalue"\`, the earlier rule, the threshold also decides
which estimates enter the prose. If \`save_output = TRUE\`, the complete
reporting list is saved.

## See also

\[margot_plot_ate()\], \[margot_table_ate()\], \[margot_plot()\]
