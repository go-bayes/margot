# Plot a target-population projection report

Draws the difference between the source sample and the target before and
after projection weighting, one facet per registered variable
(percentage points for categorical levels; standardised differences for
numeric variables), and the share of the projection weight held by the
largest weights beside the share under equal weights.

## Usage

``` r
# S3 method for class 'margot_target_projection_report'
plot(x, what = c("balance", "weights"), ...)
```

## Arguments

- x:

  A \`margot_target_projection_report\` object.

- what:

  Which panels to draw: \`"balance"\`, \`"weights"\`, or both (the
  default).

- ...:

  Ignored.

## Value

A \`ggplot\` object for one panel, or a \`patchwork\` object for both.
