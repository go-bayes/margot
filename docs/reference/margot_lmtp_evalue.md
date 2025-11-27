# Combine LMTP Summary and E-Value Calculation

This function first creates a summary table from the output of
\`lmtp::lmtp_contrast()\` using \`margot_tab_lmtp\`, specifying the
desired scale (RD or RR) and a new name for the row. It then calculates
E-values for the estimates in the table to assess the potential impact
of unmeasured confounding, appending these values to the summary table.

## Usage

``` r
margot_lmtp_evalue(
  lmtp_output,
  scale = c("RD", "RR"),
  new_name = "character_string",
  delta = 1,
  sd = 1
)
```

## Arguments

- lmtp_output:

  The output from \`lmtp::lmtp_contrast()\`, to be summarized and
  analyzed for E-values.

- scale:

  Character string specifying the scale of the estimate to be used in
  the summary table and E-value calculation. Valid options are "RD"
  (risk difference) or "RR" (risk ratio). Default is "RD".

- new_name:

  Character string to name the row in the output summary table,
  representing the treatment contrast. This name will be applied to the
  first row of the summary table.

- delta:

  The hypothesized increase in outcome for RD scale calculations. Used
  only when \`scale\` is "RD". Default value is 1.

- sd:

  The standard deviation of the outcome for RD scale calculations. Used
  only when \`scale\` is "RD". Default value is 1.

## Value

A data frame with the original estimates and their E-values. The table
includes columns for the estimate (either RD or RR), its E-Value, and
the E-Value lower bound, excluding the 'standard_error' column.

## See also

`margot_tab_lmtp`,
[`lmtp_evalue_tab`](https://go-bayes.github.io/margot/reference/lmtp_evalue_tab.md)
for the underlying functions used.

## Examples

``` r
if (FALSE) { # \dontrun{
# assuming `contrast_output` is the result from `lmtp::lmtp_contrast()`
summary_evalues <- margot_lmtp_evalue(
  lmtp_output = contrast_output,
  scale = "RD",
  new_name = "Treatment Effect"
)
print(summary_evalues)
} # }
```
