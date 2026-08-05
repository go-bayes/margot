# Plot best linear projection coefficients

Draws a forest-style plot of \[margot_blp()\] coefficients with 95
intervals and a zero reference line, one facet per outcome. Nothing is
coloured or annotated by significance, by design.

## Usage

``` r
margot_plot_blp(
  blp,
  outcomes = NULL,
  terms = NULL,
  title = NULL,
  caption = .margot_blp_relativity_note()
)
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

- title:

  Optional plot title. Defaults to \`NULL\`.

- caption:

  Plot caption. Defaults to the mandatory relativity sentence.

## Value

A \`ggplot\` object.

## See also

\[margot_blp()\], \[margot_table_blp()\]

## Examples

``` r
if (FALSE) { # \dontrun{
blp <- margot_blp(cf)
margot_plot_blp(blp)
} # }
```
