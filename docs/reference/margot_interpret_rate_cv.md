# Interpret CV RATE Results

Produces a summary of which outcomes show positive, negative, or
inconclusive heterogeneous treatment effects based on cross-validation
RATE analysis, similar to margot_interpret_rate() but for CV results.

## Usage

``` r
margot_interpret_rate_cv(cv_results, flipped_outcomes = NULL, target = NULL)
```

## Arguments

- cv_results:

  A margot_cv_results object from margot_rate_cv()

- flipped_outcomes:

  Character vector of outcomes that were inverted during preprocessing.
  Optional.

- target:

  Character. "AUTOC" or "QINI" (ignored when cv_results contains both).

## Value

A list containing:

- summary_table: Formatted table with CV results

- interpretation: Markdown-formatted character string summarizing the
  results

- autoc_results: AUTOC interpretation (if available)

- qini_results: QINI interpretation (if available)

- comparison: Comparison between AUTOC and QINI (if both available)
