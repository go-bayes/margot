# Overview bullets for multiple LMTP positivity analyses

Creates concise, supplement-ready overview bullets and an optional table
summarising worst-case positivity diagnostics across multiple analyses
(e.g., different outcome/shift sets).

## Usage

``` r
margot_interpret_lmtp_positivity_overview(
  reports,
  labels = names(reports),
  digits = 2,
  include_table = TRUE
)
```

## Arguments

- reports:

  A named list where each element is the result of
  \`margot_report_lmtp_positivity()\` for one analysis. The list names
  or the \`labels\` vector will be used as the analysis labels.

- labels:

  Optional character vector of labels (same length/order as
  \`reports\`). If NULL, uses \`names(reports)\`.

- digits:

  Integer for rounding ESS/N in bullets and table (default 2).

- include_table:

  Logical; if TRUE, returns a data.frame \`overview_table\`.

## Value

A list with - bullets: character vector of bullet lines -
overview_table: data.frame (if include_table = TRUE) with worst-zero and
worst ESS/N per analysis
