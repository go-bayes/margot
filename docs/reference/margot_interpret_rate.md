# Interpret RATE Estimates

Produces a concise summary of which outcomes show positive, negative, or
inconclusive heterogeneous treatment effects based on RATE (Rank Average
Treatment Effect) analysis.

Produce a compact Markdown summary describing which outcomes show
positive, negative, or inconclusive heterogeneous treatment effects.

## Usage

``` r
margot_interpret_rate(
  rate_df,
  flipped_outcomes = NULL,
  target = "AUTOC",
  adjust_positives_only = FALSE
)

margot_interpret_rate(
  rate_df,
  flipped_outcomes = NULL,
  target = "AUTOC",
  adjust_positives_only = FALSE
)
```

## Arguments

- rate_df:

  A data frame from margot_rate() or a list containing rate_autoc and
  rate_qini.

- flipped_outcomes:

  Character vector of outcomes inverted during preprocessing.

- target:

  Character; either "AUTOC" or "QINI" (ignored when rate_df is a list).

- adjust_positives_only:

  Logical; if TRUE, apply multiple testing correction only to positive
  RATEs in comparison output. Default FALSE.

## Value

For a single table: a markdown-formatted character string summarizing
the RATE results. For a list input: a list containing comparison results
and individual interpretations for both AUTOC and QINI.

If rate_df is a data frame, a Markdown string. If rate_df is a list,
returns a list produced by margot_interpret_rate_comparison().

## Details

RATE quantifies how much better CATE-based targeting performs compared
to uniform treatment (ATE). The interpretation differs by weighting
scheme:

- AUTOC: Uses logarithmic weighting, emphasizing top responders

- QINI: Uses linear weighting, balancing effect size and prevalence

Positive RATE values indicate heterogeneity can be exploited for better
targeting. Negative values suggest CATE targeting would underperform
uniform treatment.

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute RATE for causal forest results
rate_results <- margot_rate(causal_forest_results)

# Interpret the results (handles both AUTOC and QINI)
interpretation <- margot_interpret_rate(rate_results)
cat(interpretation$comparison)

# Or interpret just AUTOC results
autoc_interpretation <- margot_interpret_rate(
  rate_results$rate_autoc,
  target = "AUTOC"
)
} # }
```
