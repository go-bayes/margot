# Print a target-population projection report

Prints the source, projected-source and target values for every
registered projection variable, names target strata without source
representation, and summarises the projection weights. Categorical rows
are shown as percentages with differences in percentage points; numeric
rows as means with standardised differences.

## Usage

``` r
# S3 method for class 'margot_target_projection_report'
print(x, digits = 2, ...)
```

## Arguments

- x:

  A \`margot_target_projection_report\` object.

- digits:

  Decimal places for percentages and summaries.

- ...:

  Ignored.

## Value

\`x\`, invisibly.
