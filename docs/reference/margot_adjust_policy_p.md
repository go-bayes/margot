# adjust policy-value p-values for multiplicity

adds adjusted p-values. the caller can flag a result as significant
using this metric.

## Usage

``` r
margot_adjust_policy_p(tbl, method = "bonferroni", alpha = 0.05)
```

## Arguments

- tbl:

  data frame returned by \[margot_collect_policy_values()\]. must
  include columns \`p_value\` (raw two-sided) and \`estimate\`.

- method:

  character. multiplicity correction for the \`p_adj\` column: any
  option of \[stats::p.adjust()\]. default "bonferroni".

- alpha:

  numeric. threshold that defines the \`significant\` flag. default
  0.05.

## Value

\`tbl\` with two extra columns: \* \`p_adj\` – adjusted p-value \*
\`significant\` – logical; TRUE if p_adj \< \`alpha\`.
