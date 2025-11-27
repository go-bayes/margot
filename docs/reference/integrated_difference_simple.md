# Compute Integrated Difference for Simple Baseline

When comparing against a simple baseline, this computes the integrated
difference between the comparison curve and the simple baseline.

## Usage

``` r
integrated_difference_simple(object, baseline, spend, ...)
```

## Arguments

- object:

  A maq object (the comparison curve)

- baseline:

  A qini_simple_baseline object

- spend:

  The spend level up to which to integrate

- ...:

  Additional arguments (ignored)

## Value

A list with estimate and std.err
