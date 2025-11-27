# Diagnose Policy Tree Stability

Comprehensive stability diagnostics for policy trees, combining
correlation analysis with stability results to provide actionable
insights about tree instability.

## Usage

``` r
margot_stability_diagnostics(
  stability_results,
  model_results,
  model_name,
  correlation_threshold = 0.5
)
```

## Arguments

- stability_results:

  Output from margot_policy_tree_stability()

- model_results:

  Original causal forest results

- model_name:

  Model to analyze

- correlation_threshold:

  Threshold for correlation analysis (default 0.5)

## Value

List containing diagnostic results and recommendations
