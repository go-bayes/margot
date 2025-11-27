# Development Version of QINI Analysis with Integrated Interpretation

Computes QINI curves and related metrics using the test set from
margot_causal_forest_dev(). Integrates functionality from margot_qini()
and margot_interpret_qini() into a single coherent function.

## Usage

``` r
margot_qini_dev(
  cf_results,
  outcome_vars = NULL,
  baseline_method = "maq_no_covariates",
  use_evaluation_forest = TRUE,
  spend_levels = c(0.05, 0.1, 0.2, 0.4),
  n_bootstrap = 200,
  confidence_level = 0.95,
  seed = 12345,
  verbose = TRUE
)
```

## Arguments

- cf_results:

  Results from margot_causal_forest_dev()

- outcome_vars:

  Character vector of outcomes to analyze. NULL = all.

- baseline_method:

  Method for baseline curve: "maq_no_covariates" (default), "simple",
  "maq_only", "auto", or "none"

- use_evaluation_forest:

  Logical. Use evaluation forest for DR scores if available (default:
  TRUE)

- spend_levels:

  Numeric vector of budget levels for gain summaries (default: c(0.05,
  0.1, 0.2, 0.4))

- n_bootstrap:

  Integer. Bootstrap iterations for inference (default: 200)

- confidence_level:

  Numeric. Confidence level (default: 0.95)

- seed:

  Integer. Random seed (default: NULL)

- verbose:

  Logical. Print progress (default: TRUE)

## Value

List of class "margot_qini_dev" containing:

- qini_curves:

  Data frame with QINI curve data for all outcomes

- qini_objects:

  List of maq objects by outcome

- gain_summaries:

  Gain summaries at specified spend levels

- qini_metrics:

  Summary metrics (peak gain, area under curve, etc.)

- metadata:

  Analysis parameters and data info

## Details

This function: - Uses the test set from the causal forest for honest
evaluation - Can use evaluation forests for computing DR scores if
available - Provides multiple baseline methods for comparison - Computes
gain summaries at specified budget levels - Includes bootstrap inference
for uncertainty quantification

The baseline methods are: - "maq_no_covariates": Uses maq with
target.with.covariates = FALSE - "simple": Constant treatment effect
assumption - "maq_only": Standard maq baseline - "auto": Tries methods
in order until one succeeds - "none": No baseline curve

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate test data and fit forests
test_data <- margot_simulate_test_data()
cf_results <- margot_causal_forest_dev(
  data = test_data$data,
  outcome_vars = c("Y1", "Y2", "Y3", "Y4"),
  treatment = "A"
)

# Compute QINI curves
qini_results <- margot_qini_dev(cf_results)

# Analyze specific outcomes with custom parameters
qini_results_custom <- margot_qini_dev(
  cf_results,
  outcome_vars = c("Y1", "Y4"),
  baseline_method = "simple",
  spend_levels = c(0.1, 0.25, 0.5)
)
} # }
```
