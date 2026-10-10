# Print an LMTP full analysis-weight report

Prints the distribution of the full analysis weight (baseline weight
times the cumulative joint ratio) at every node and stage, the
concentration at the final node, the causes of zero or unavailable final
weights, and the regularisation comparison when one is stored.

## Usage

``` r
# S3 method for class 'margot_lmtp_analysis_weight_report'
print(x, digits = 2, ...)
```

## Arguments

- x:

  A \`margot_lmtp_analysis_weight_report\` object.

- digits:

  Decimal places for summaries.

- ...:

  Ignored.

## Value

\`x\`, invisibly.
