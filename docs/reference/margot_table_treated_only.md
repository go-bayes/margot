# Condensed table: treated-only summary

Produces a one-row-per outcome × depth table focused on the treated-only
signal: average uplift among treated with 95

## Usage

``` r
margot_table_treated_only(report_df, label_mapping = NULL, digits = 3)
```

## Arguments

- report_df:

  Data frame returned by \[margot_report_consensus_policy_value()\],
  called with \`include_treated_only = TRUE\`.

- label_mapping:

  Optional named list for outcome label mapping.

- digits:

  Integer; rounding digits for numeric columns (default 3).

## Value

A condensed data frame with columns: outcome, depth, avg_uplift_treated,
(optional) avg_uplift_treated_ci, coverage_treated\_
