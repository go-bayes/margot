# Assess Covariate Overlap from Causal Forest Models

This function analyzes propensity score overlap and covariate balance
from the outputs of margot_causal_forest() or margot_flip_forests(). It
provides diagnostics for assessing the validity of causal estimates.

## Usage

``` r
margot_assess_overlap(
  model_results,
  model_names = NULL,
  exposure_name = NULL,
  label_mapping = NULL,
  plot = TRUE,
  save_plots = FALSE,
  output_dir = NULL,
  theme = "classic",
  verbose = TRUE
)
```

## Arguments

- model_results:

  Output from margot_causal_forest() or margot_flip_forests()

- model_names:

  Character vector of model names to assess. If NULL, all models.

- exposure_name:

  Character string naming the exposure/treatment variable. If NULL,
  defaults to "Treatment".

- label_mapping:

  Named list mapping variable names to custom labels for the exposure.

- plot:

  Logical indicating whether to create overlap plots. Default is TRUE.

- save_plots:

  Logical indicating whether to save plots. Default is FALSE.

- output_dir:

  Directory to save plots if save_plots is TRUE.

- theme:

  Character string specifying the ggplot2 theme. Default is "classic".
  Options include "classic", "minimal", "bw", "gray", "light", "dark",
  "void".

- verbose:

  Logical for detailed messages. Default is TRUE.

## Value

A list containing:

- overlap_summaryData frame with overlap statistics for each model

- propensity_plotsList of ggplot objects (if plot = TRUE)

- balance_tablesCovariate balance tables for each model

- trimming_summarySummary of observations trimmed due to poor overlap

- text_summaryCharacter string with a prose summary suitable for reports

## Details

The function assesses overlap using several metrics:

- Propensity score distributions by treatment group

- Common support region statistics

- Test calibration from grf (differential prediction test)

- Covariate balance within propensity score strata

Poor overlap (propensity scores near 0 or 1) indicates limited
comparability between treatment groups and may lead to unreliable causal
estimates.

## Examples

``` r
if (FALSE) { # \dontrun{
# assess overlap for all models
overlap_results <- margot_assess_overlap(model_results)

# assess specific models only
overlap_results <- margot_assess_overlap(
  model_results,
  model_names = c("model_outcome1", "model_outcome2")
)

# save plots
overlap_results <- margot_assess_overlap(
  model_results,
  save_plots = TRUE,
  output_dir = "output/overlap_diagnostics"
)

# use text summary in a report
cat(overlap_results$text_summary)

# use different theme
overlap_results <- margot_assess_overlap(
  model_results,
  theme = "minimal"
)
} # }
```
