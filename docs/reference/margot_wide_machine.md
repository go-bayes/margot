# Transform longitudinal data to wide format with baseline imputation and optional NA indicators

This function transforms longitudinal data from long format to wide
format, ensuring that baseline measurements are correctly labeled and
included. It handles multiple observations per subject across an
indefinite number of waves, and allows for the specification of baseline
variables, exposure variables, outcome variables, and time-varying
confounders.

## Usage

``` r
margot_wide_machine(
  .data,
  id = "id",
  wave = "wave",
  baseline_vars,
  exposure_var,
  outcome_vars,
  confounder_vars = NULL,
  imputation_method = c("median", "mice", "none"),
  include_exposure_var_baseline = TRUE,
  include_outcome_vars_baseline = TRUE,
  extend_baseline = FALSE,
  include_na_indicators = TRUE
)
```

## Arguments

- .data:

  A data frame containing the longitudinal data in long format.

- id:

  The name of the ID column identifying subjects (default is "id").

- wave:

  The name of the wave/time column (default is "wave").

- baseline_vars:

  A character vector of baseline variable names to be included at t0.

- exposure_var:

  A character string specifying the name of the exposure variable to be
  tracked across time.

- outcome_vars:

  A character vector of outcome variable names to be tracked across
  time.

- confounder_vars:

  An optional character vector of time-varying confounder variable names
  to include without imputation (default is NULL).

- imputation_method:

  A character string specifying the imputation method to use for
  baseline variables. Options are 'median' (default), 'mice', or 'none'.

- include_exposure_var_baseline:

  Logical indicating whether to include the exposure variable at
  baseline (t0).

- include_outcome_vars_baseline:

  Logical indicating whether to include outcome variables at baseline
  (t0).

- extend_baseline:

  Logical indicating whether to include baseline_vars in all subsequent
  waves (default FALSE).

- include_na_indicators:

  Logical indicating whether to generate NA indicator columns for
  baseline variables (default TRUE).

## Value

A wide-format data frame with each subject's observations across time
points represented in a single row. Baseline variables, exposure
variables at baseline, and outcome variables at baseline have missing
values imputed as specified. NA indicators are created for variables at
baseline only if include_na_indicators is TRUE. Exposure variables are
tracked across waves but are not imputed beyond baseline. Outcome
variables are included only at the final wave unless
include_outcome_vars_baseline is TRUE. Confounders (if any) are included
without imputation.
