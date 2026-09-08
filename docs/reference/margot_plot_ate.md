# Plot Average Treatment Effect Estimates

Return the figure produced by \[margot_plot()\]. The plotting, table and
interpretation interfaces use the same reporting calculations and scale
metadata.

## Usage

``` r
margot_plot_ate(.data, ..., scale_info = NULL)
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

A \`ggplot\` object.

## Details

This interface returns a figure directly. \[margot_table_ate()\] returns
the corresponding table and \[margot_interpret_ate()\] returns the
corresponding prose. The existing \[margot_plot()\] interface retains
its list containing all three outputs.

All arguments in \`...\` retain their \[margot_plot()\] behaviour. In
particular, \`save_output = TRUE\` saves the complete reporting list
before this function returns the figure.

## See also

\[margot_table_ate()\], \[margot_interpret_ate()\], \[margot_plot()\]
