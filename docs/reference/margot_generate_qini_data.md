# Generate QINI Data for Visualization

This function generates QINI curve data on-demand from a model result.
It extracts tau_hat from the model, uses maq to generate the CATE curve,
and creates a baseline following various methods.

## Usage

``` r
margot_generate_qini_data(
  model_result,
  outcome_data,
  treatment,
  weights = NULL,
  baseline_method = c("maq_no_covariates", "auto", "simple", "maq_only", "none"),
  seed = 12345,
  verbose = FALSE,
  treatment_cost = 1,
  x_axis = c("proportion", "budget")
)
```

## Arguments

- model_result:

  A single model result from margot_causal_forest output

- outcome_data:

  The outcome data (Y) for the model

- treatment:

  The treatment assignment vector (W)

- weights:

  Optional weights vector

- baseline_method:

  Method for generating baseline: "maq_no_covariates" (default), "auto",
  "simple", "maq_only", or "none". See details.

- verbose:

  Logical for verbose output

- treatment_cost:

  Scalar treatment cost per unit. Default 1.

- x_axis:

  Type of x-axis for QINI curves: "proportion" (default) or "budget".
  "proportion" shows proportion of population treated (0 to 1). "budget"
  shows budget per unit (0 to treatment_cost), matching maq's
  visualization.

## Value

A list with components: - qini_data: data.frame with columns proportion
(or budget), gain, curve - qini_objects: list with cate and ate maq
objects (ate may be simplified) - x_axis: the x-axis type used
("proportion" or "budget")

## Details

The baseline_method parameter controls how the no-prioritization
baseline is generated:

- "maq_no_covariates": Use maq with target.with.covariates = FALSE
  (default). Automatically falls back to simple baseline if maq fails.

- "auto": Try maq with target.with.covariates = FALSE first, fall back
  to simple baseline if it fails

- "simple": Always use simple baseline (straight line from (0,0) to (1,
  mean(tau_hat)))

- "maq_only": Use standard maq with constant rewards (may fail with no
  fallback)

- "none": No baseline curve

The simple baseline represents the expected gain under random
allocation, where treating proportion p of units yields gain p \*
E\[tau\]. This method always succeeds and provides a robust fallback
when maq fails.
