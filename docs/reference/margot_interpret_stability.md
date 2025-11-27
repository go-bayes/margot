# Interpret Policy Tree Stability Results

Provides a narrative interpretation of policy tree stability results
suitable for inclusion in scientific manuscripts. The interpretation
acknowledges the inherent instability of decision trees while focusing
on robust patterns that emerge across iterations.

## Usage

``` r
margot_interpret_stability(
  object,
  model_name,
  depth = 2,
  stability_threshold = 0.7,
  format = c("text", "technical"),
  decimal_places = 1,
  include_theory = TRUE,
  label_mapping = NULL,
  include_ci = FALSE,
  ci_level = 0.95
)
```

## Arguments

- object:

  Object of class "margot_stability_policy_tree"

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

- include_ci:

  Logical: If TRUE and format = "technical", include simple confidence
  intervals for selection frequencies (Wilson interval) and, when
  available, for threshold means (normal approx). These quantify
  iteration-level variability; for sample uncertainty prefer vary_type =
  "bootstrap".

- ci_level:

  Numeric: Confidence level for intervals (default 0.95)

## Value

Character string containing the interpretation
