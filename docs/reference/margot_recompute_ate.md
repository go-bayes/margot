# Recompute Average Treatment Effects with Different Target Samples

This function allows recomputation of average treatment effects (ATEs)
from fitted causal forest models using different target samples or
estimation methods without refitting the models. This is useful for
sensitivity analysis and exploring treatment effects in different
subpopulations.

## Usage

``` r
margot_recompute_ate(
  causal_forest_output,
  target_sample = c("all", "treated", "control", "overlap"),
  method = "AIPW",
  scale = c("RD", "RR"),
  delta = 1,
  sd = 1,
  subset = NULL,
  respect_train_test_split = TRUE
)
```

## Arguments

- causal_forest_output:

  Output from \`margot_causal_forest()\` or
  \`margot_causal_forest_parallel()\`. Must have been run with
  \`save_models = TRUE\`.

- target_sample:

  Character string specifying the target sample for ATE estimation.
  Options are:

  - "all" - All units (default in original models)

  - "treated" - Only treated units (ATT)

  - "control" - Only control units (ATC)

  - "overlap" - Units with propensity scores away from 0 and 1

- method:

  Character string specifying the estimation method. Currently only
  "AIPW" (Augmented Inverse Probability Weighting) is supported by grf.

- scale:

  Character string specifying the scale of the estimate. Either "RD"
  (risk difference) or "RR" (risk ratio). Default is "RD".

- delta:

  The hypothesized increase in outcome for RD scale E-value
  calculations. Default is 1.

- sd:

  The standard deviation of the outcome for RD scale E-value
  calculations. Default is 1.

- subset:

  Optional logical vector for subsetting the data. Default is NULL.

- respect_train_test_split:

  Logical. If TRUE and the original analysis used
  use_train_test_split=TRUE, computes treatment effects on the test set
  only for consistency with policy evaluation. Default is TRUE.

## Value

A list with the same structure as the original causal forest output, but
with:

- Updated \`ate\` values in each model's results

- Recomputed \`custom_table\` with new E-values and renamed effect
  columns (ATE/ATT/ATC/ATO)

- Updated \`combined_table\` with all new estimates

- A new element \`ate_params\` documenting the parameters used

## Details

This function requires that the original causal forest models were saved
(using \`save_models = TRUE\`). It extracts the fitted models and
recomputes ATEs using the specified target sample.

The different target samples represent different estimands: - "all":
Average Treatment Effect (ATE) for the entire population - "treated":
Average Treatment Effect on the Treated (ATT) - "control": Average
Treatment Effect on the Control (ATC) - "overlap": ATE for units with
good overlap in propensity scores

## Examples

``` r
if (FALSE) { # \dontrun{
# First fit causal forest models with saved models
cf_results <- margot_causal_forest(
  data = mydata,
  outcome_vars = outcomes,
  covariates = X,
  save_models = TRUE
)

# Later, get ATEs for different target populations
ate_overlap <- margot_recompute_ate(cf_results, target_sample = "overlap")
ate_treated <- margot_recompute_ate(cf_results, target_sample = "treated")

# Compare estimates
cf_results$combined_table # Original (all units)
ate_overlap$combined_table # Overlap units only
ate_treated$combined_table # Treated units only
} # }
```
