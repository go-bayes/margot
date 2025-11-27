# Interpret Qini Results

Interprets Qini results for binary and multi-arm treatments,
automatically detecting treatment type from input data.

## Usage

``` r
margot_interpret_qini(
  multi_batch,
  label_mapping = NULL,
  alpha = 0.05,
  decimal_places = 2,
  model_names = NULL,
  spend_levels = c(0.1, 0.4),
  include_intro = TRUE,
  baseline_method = NULL,
  scale = "average"
)
```

## Arguments

- multi_batch:

  List from margot_policy() or margot_qini() with diff_gain_summaries

- label_mapping:

  Named list mapping model names to readable labels

- alpha:

  Significance level for confidence intervals (default: 0.05)

- decimal_places:

  Decimal places for rounding (default: 2)

- model_names:

  Character vector of models to process (optional)

- spend_levels:

  Numeric vector of spend levels to analyze (default: 0.1). If requested
  levels don't exist in the data, the function will use available levels
  instead.

- include_intro:

  Logical whether to include explanatory text about CATE and Qini curves
  (default: TRUE)

- baseline_method:

  Method for generating baseline when regenerating summaries:
  "maq_no_covariates" (default if NULL), "auto", "simple", "maq_only",
  or "none". If NULL, uses the baseline method from the original QINI
  generation.

- scale:

  Character string specifying the scale for gains: "average" (default),
  "cumulative", or "population". This affects how gains are interpreted
  in the summary.

## Value

List with summary_table, qini_explanation, concise_summary,
reliable_model_names, reliable_model_ids

## Details

The function automatically detects available spend levels in the data.
If requested spend levels are not available, it will use the closest
available levels and warn the user. To see what spend levels are
available, check the names of
\`multi_batch\[\[1\]\]\$diff_gain_summaries\`.

This function accepts output from either:

- margot_policy() - which includes policy trees and Qini results

- margot_qini() - which focuses solely on Qini curves and gains
