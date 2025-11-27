# Inspect and Debug Model Result Structure

This utility function inspects the structure of a specific model result
or all models in the results list, helping to debug issues with flipping
estimates.

## Usage

``` r
margot_inspect_model(model_results, model_name = NULL, details = FALSE)
```

## Arguments

- model_results:

  A list containing the model results from margot_causal_forest().

- model_name:

  Name of a specific model to inspect. If NULL, summarizes all models.

- details:

  Logical indicating whether to show detailed structure of objects.
  Default is FALSE.

## Value

Invisibly returns a summary of the model structure.
