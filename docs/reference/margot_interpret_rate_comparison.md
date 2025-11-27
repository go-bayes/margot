# Compare AUTOC and QINI RATE Tables

Internal function that compares AUTOC and QINI RATE results to identify
where the two weighting schemes agree or disagree on heterogeneity
evidence.

## Usage

``` r
margot_interpret_rate_comparison(
  autoc_df,
  qini_df,
  flipped_outcomes = NULL,
  adjust_positives_only = FALSE
)

margot_interpret_rate_comparison(
  autoc_df,
  qini_df,
  flipped_outcomes = NULL,
  adjust_positives_only = FALSE
)
```

## Arguments

- autoc_df:

  Data frame of AUTOC results from margot_rate().

- qini_df:

  Data frame of QINI results from margot_rate().

- flipped_outcomes:

  Character vector of outcomes inverted during preprocessing.

- adjust_positives_only:

  Logical; if TRUE, apply multiple testing correction only to positive
  RATE estimates (negative outcomes use unadjusted CIs). Default FALSE.

## Value

A list containing:

- comparison: Markdown summary comparing AUTOC and QINI

- autoc_results: Interpretation of AUTOC results

- qini_results: Interpretation of QINI results

- autoc_model_names: Models with positive RATE in AUTOC

- qini_model_names: Models with positive RATE in QINI

- both_model_names: Models with positive RATE in both

A list with elements: \* comparison: Markdown comparison text \*
autoc_results: Output of margot_interpret_rate() for AUTOC \*
qini_results: Output of margot_interpret_rate() for QINI \*
autoc_model_names: Models with positive AUTOC \* qini_model_names:
Models with positive QINI \* both_model_names: Models positive in both
\* either_model_names: Models positive in either \*
not_excluded_autoc_model_names: AUTOC models not reliably negative \*
not_excluded_qini_model_names: QINI models not reliably negative \*
not_excluded_both: Models not excluded by both AUTOC and QINI \*
not_excluded_either: Models not excluded by either AUTOC or QINI
