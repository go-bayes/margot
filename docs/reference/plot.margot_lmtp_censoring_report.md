# Plot an LMTP censoring report

Draws observed retention by transition, unweighted and weighted, and
adds a range panel for the fitted continued-observation probabilities
and for the joint ratios when the report holds them.

## Usage

``` r
# S3 method for class 'margot_lmtp_censoring_report'
plot(x, ...)
```

## Arguments

- x:

  A \`margot_lmtp_censoring_report\` object.

- ...:

  Ignored.

## Value

A \`ggplot\` object, or a \`patchwork\` object when more than one panel
is drawn.
