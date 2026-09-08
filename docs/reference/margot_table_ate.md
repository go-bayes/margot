# Tabulate Average Treatment Effect Estimates

Return the table produced by \[margot_plot()\], retaining numerical
precision for subsequent formatting.

## Usage

``` r
margot_table_ate(.data, ..., scale_info = NULL)
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

A data frame corresponding to the \`transformed_table\` element of
\[margot_plot()\].

## Details

The table retains model-scale estimates and includes separately labelled
reported quantities when transformation metadata is supplied. Arguments
in \`...\` retain their \[margot_plot()\] behaviour, including column
renaming, outcome ordering and multiplicity adjustment. If \`save_output
= TRUE\`, the complete reporting list is saved.

## See also

\[margot_plot_ate()\], \[margot_interpret_ate()\], \[margot_plot()\]
