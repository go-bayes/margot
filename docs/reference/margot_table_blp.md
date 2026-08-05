# Reporting table of best linear projection coefficients

Formats a \[margot_blp()\] result as a reporting table of estimates with
95 confidence intervals. No significance stars and no p-values are
produced, by design.

## Usage

``` r
margot_table_blp(blp, outcomes = NULL, terms = NULL, digits = 2)
```

## Arguments

- blp:

  A \`margot_blp\` data frame returned by \[margot_blp()\].

- outcomes:

  Optional character vector of outcomes to retain, in the order given.
  Defaults to \`NULL\`, meaning every outcome.

- terms:

  Optional character vector of coefficient terms to retain, in the order
  given. Defaults to \`NULL\`, meaning every term.

- digits:

  Integer; decimal places for the reported estimate and interval.
  Default 2.

## Value

A data frame of class \`margot_blp_table\` with columns \`outcome\`,
\`term\`, \`estimate\`, \`conf_low\`, \`conf_high\`, \`estimate_ci\`,
and \`status\`, carrying the mandatory relativity sentence as its
\`caption\` attribute.

## Details

Every rendered table must carry the relativity sentence attached as the
\`caption\` attribute of the returned object. It states that the
coefficients are descriptive projections interpretable only relative to
the other covariates in the registered set.

## See also

\[margot_blp()\], \[margot_plot_blp()\]

## Examples

``` r
if (FALSE) { # \dontrun{
blp <- margot_blp(cf)
tbl <- margot_table_blp(blp, digits = 2)
attr(tbl, "caption")
} # }
```
