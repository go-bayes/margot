# Compute Difference in Gains and Integrated Difference Between Reference and Comparison Curves

This function computes the difference in average gains and the
integrated difference between a reference curve (maq object) and a
comparison curve at a specified spend level. It returns a list of
formatted strings for easy use in Quarto markdown text.

## Usage

``` r
margot_summary_cate_difference_gain(
  mc_result,
  outcome_var,
  reference_curve,
  comparison_curve,
  spend,
  digits = 3
)
```

## Arguments

- mc_result:

  A list containing the results from margot_multi_arm_causal_forest().

- outcome_var:

  A character string specifying the name of the outcome variable.

- reference_curve:

  A character string specifying the name of the reference Qini curve
  (e.g., "baseline").

- comparison_curve:

  A character string specifying the name of the comparison Qini curve
  (e.g., "arm2").

- spend:

  A numeric value specifying the spend level (between 0 and 1).

- digits:

  An integer specifying the number of decimal places to round the
  output. Default is 3.

## Value

A list containing formatted strings for use in Quarto markdown text:

- diff_gain:

  Formatted string for difference in gains

- int_diff:

  Formatted string for integrated difference

- summary:

  A summary sentence of the comparison

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming mc_result is the result of margot_multi_arm_causal_forest()
result <- margot_summary_cate_difference_gain(mc_result,
  outcome_var = "model_Y",
  reference_curve = "baseline",
  comparison_curve = "arm2",
  spend = 0.3
)
# Use in text
glue::glue("The difference in gains is {result$diff_gain}. {result$summary}")
} # }
```
