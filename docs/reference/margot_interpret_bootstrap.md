# Interpret Bootstrap Policy Tree Results

Provides a narrative interpretation of bootstrap policy tree results
suitable for inclusion in scientific manuscripts. The interpretation
acknowledges the inherent instability of decision trees while focusing
on robust patterns that emerge across bootstrap iterations.

This function is deprecated. Please use
[`margot_interpret_stability`](https://go-bayes.github.io/margot/reference/margot_interpret_stability.md)
instead.

## Usage

``` r
margot_interpret_bootstrap(...)

margot_interpret_bootstrap(...)
```

## Arguments

- ...:

  Arguments passed to margot_interpret_stability

- object:

  Object of class "margot_bootstrap_policy_tree"

- model_name:

  Model name to interpret

- depth:

  Tree depth to interpret (1, 2, or "both")

- stability_threshold:

  Minimum frequency to consider a split "stable" (default 0.7)

- format:

  Output format: "text" for narrative prose or "technical" for detailed
  statistics

- decimal_places:

  Number of decimal places for statistics (default 1)

- include_theory:

  Logical: Include theoretical context about tree instability (default
  TRUE)

- label_mapping:

  Optional named list mapping variable names to labels. If NULL, uses
  automatic transformation via transform_var_name()

## Value

Character string containing the interpretation

Result from margot_interpret_stability
