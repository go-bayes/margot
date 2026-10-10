# Perform Naive Cross-Sectional Regressions

This function performs naive cross-sectional linear regressions of a
single exposure variable on multiple outcome variables, ignoring
potential confounders. It produces output compatible with margot_plot()
to demonstrate what happens when proper causal inference methods are not
used. The results should be interpreted as misspecified models that do
not account for confounding.

## Usage

``` r
margot_naive_regressions(
  data,
  exposure_var,
  outcome_vars,
  baseline_vars = NULL,
  scale = c("RD", "RR"),
  delta = 1,
  sd = 1,
  coefficient_scale = 1,
  save_output = FALSE,
  save_path = here::here("push_mods"),
  base_filename = "naive_regressions_output",
  use_timestamp = FALSE,
  prefix = NULL,
  flip_outcomes = NULL,
  flip_method = "zscore",
  flip_scale_bounds = NULL
)
```

## Arguments

- data:

  A data frame containing all necessary variables.

- exposure_var:

  A character string specifying the exposure variable name.

- outcome_vars:

  A character vector of outcome variable names to be modeled.

- baseline_vars:

  Optional character vector of baseline variables to include as
  covariates in the regression models. Default is NULL (no additional
  covariates).

- scale:

  Character string specifying the scale for E-value calculation. Options
  are "RD" (risk difference, default) or "RR" (risk ratio).

- delta:

  The hypothesised increase in outcome for RD scale E-value
  calculations. Default value is 1.

- sd:

  The standard deviation of the outcome for RD scale E-value
  calculations. Default value is 1.

- coefficient_scale:

  Numeric value to scale coefficients by. Default is 1 (no scaling). Use
  this to interpret effects for multi-unit changes (e.g., set to 4 to
  get effects for a 4-unit change in the exposure variable).

- save_output:

  Logical, whether to save the complete output. Default is FALSE.

- save_path:

  The directory path to save the output. Default is "push_mods" in the
  current working directory.

- base_filename:

  The base filename for saving the output. Default is
  "naive_regressions_output".

- use_timestamp:

  Logical, whether to include a timestamp in the filename. Default is
  FALSE.

- prefix:

  Optional prefix to add to the saved output filename. Default is NULL.

- flip_outcomes:

  Optional character vector or named list specifying outcomes to
  reverse-score. Behaves like the \`flip_outcomes\` argument in
  \`margot_causal_forest()\` and appends an \`\_r\` suffix to flipped
  outcomes.

- flip_method:

  Default inversion method when \`flip_outcomes\` is supplied. One of
  "zscore" (simple negation, default) or "ordinal" (bounded scale
  inversion).

- flip_scale_bounds:

  Numeric vector of length 2 \[min, max\] or named list supplying bounds
  for ordinal flipping. Required when \`flip_method = "ordinal"\` unless
  bounds should be inferred from the data.

## Value

A list containing:

- models:

  A list of lm() model objects for each outcome.

- combined_table:

  A data frame with columns E\[Y\|A\], 2.5 and E_Val_bound, compatible
  with margot_plot().

- individual_results:

  A list of individual regression summaries for each outcome.

## Details

This function fits simple linear regressions of the form: outcome ~
exposure + baseline_vars. It calculates confidence intervals and
E-values for each regression coefficient. The output uses "E\[Y\|A\]"
notation to indicate these are conditional expectations from naive
regressions, not causal effects. The E-values calculated are technically
incorrect since they assume causal interpretation of the coefficients.

This function is intended for educational purposes to demonstrate the
difference between naive associations and properly estimated causal
effects.

## Examples

``` r
if (FALSE) { # \dontrun{
# perform naive regressions
naive_results <- margot_naive_regressions(
  data = my_data,
  exposure_var = "treatment",
  outcome_vars = c("outcome1_z", "outcome2_z", "outcome3_z")
)

# perform naive regressions with baseline covariates
naive_results_adjusted <- margot_naive_regressions(
  data = my_data,
  exposure_var = "treatment",
  outcome_vars = c("outcome1_z", "outcome2_z", "outcome3_z"),
  baseline_vars = c("age", "gender", "baseline_outcome")
)

# perform naive regressions scaled for 4-unit change
naive_results_scaled <- margot_naive_regressions(
  data = my_data,
  exposure_var = "treatment", 
  outcome_vars = c("outcome1_z", "outcome2_z", "outcome3_z"),
  coefficient_scale = 4
)

# plot results with misspecified label
margot_plot(naive_results$combined_table, rename_ate = "Naive Association")
} # }
```
