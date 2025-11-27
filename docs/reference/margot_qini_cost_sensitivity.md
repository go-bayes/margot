# Perform Cost Sensitivity Analysis for QINI Curves

Generates QINI curves across multiple treatment cost scenarios to
understand how optimal treatment allocation changes with cost. This
allows exploration of policy recommendations under different budget
constraints without rerunning the causal forest models.

## Usage

``` r
margot_qini_cost_sensitivity(
  models,
  costs = c(0.2, 0.5, 1, 2, 5),
  model_names = NULL,
  spend_levels = 0.1,
  baseline_method = "maq_no_covariates",
  verbose = TRUE,
  seed = 12345
)
```

## Arguments

- models:

  List returned by margot_causal_forest(), containing results and
  optionally full_models.

- costs:

  Numeric vector of treatment costs to evaluate. Default is c(0.2, 0.5,
  1, 2, 5) representing a range from cheap to expensive treatments.

- model_names:

  Optional character vector specifying which models to process. Default
  NULL (all models).

- spend_levels:

  Numeric vector of spend levels for difference gain summaries. Default
  is 0.1.

- baseline_method:

  Method for generating baseline. See margot_qini() for details.

- verbose:

  Logical; print progress messages (default TRUE).

- seed:

  Integer; base seed for reproducible computations (default 12345).

## Value

A list with class "margot_qini_cost_sensitivity" containing:

- `results`: Named list where each element is a cost scenario containing
  QINI results from margot_qini()

- `costs`: The cost values used

- `models_processed`: Names of models that were processed

- `summary`: Data frame summarizing key metrics across costs

## Details

This function systematically varies the treatment_cost parameter in
margot_qini() to show how QINI curves and optimal treatment fractions
change with cost.

Lower costs (e.g., 0.2) represent scenarios where treatment is cheap
relative to budget, resulting in steeper QINI curves and larger optimal
treatment fractions. Higher costs (e.g., 5) represent expensive
treatments where only the highest-effect individuals justify treatment.

The summary table includes the optimal treatment fraction and expected
gain at key spend levels for each cost scenario, facilitating comparison
across different budget constraints.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic cost sensitivity analysis
cost_sens <- margot_qini_cost_sensitivity(
  causal_forest_results,
  costs = c(0.5, 1, 2)
)

# View summary of optimal treatment fractions
print(cost_sens$summary)

# Plot results for a specific model across costs
margot_plot_qini_cost_sensitivity(cost_sens, model_name = "model_anxiety")

# Detailed analysis for specific models
cost_sens_detailed <- margot_qini_cost_sensitivity(
  causal_forest_results,
  costs = seq(0.2, 3, by = 0.2), # Fine-grained cost grid
  model_names = c("model_anxiety", "model_depression"),
  spend_levels = c(0.05, 0.1, 0.2, 0.4) # More spend levels
)
} # }
```
