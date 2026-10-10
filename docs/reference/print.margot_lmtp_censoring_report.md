# Print an LMTP censoring report

Prints observed retention by transition and, when the report holds them,
the fitted continued-observation probabilities, censoring factors, joint
ratios and exact-zero causes. The printed summary repeats the stored
aggregates and adds no classification.

## Usage

``` r
# S3 method for class 'margot_lmtp_censoring_report'
print(x, digits = 2, ...)
```

## Arguments

- x:

  A \`margot_lmtp_censoring_report\` object.

- digits:

  Decimal places for percentages and summaries.

- ...:

  Ignored.

## Value

\`x\`, invisibly.
