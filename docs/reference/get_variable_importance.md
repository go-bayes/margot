# Extract variable importance from bootstrap results

Extract variable importance from bootstrap results

## Usage

``` r
get_variable_importance(object, model_name = NULL, depth = NULL)
```

## Arguments

- object:

  Object of class "margot_bootstrap_policy_tree"

- model_name:

  Optional model name to extract. If NULL, returns all.

- depth:

  Tree depth (1 or 2). If NULL, returns average across depths.

## Value

Data frame of variable importance scores
