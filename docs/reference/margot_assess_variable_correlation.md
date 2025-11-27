# Assess Variable Correlations for Policy Tree Analysis

Analyzes correlations among covariates used in policy tree analysis.
This helps identify groups of correlated variables that may substitute
for each other in tree splits, explaining apparent instability.

## Usage

``` r
margot_assess_variable_correlation(
  model_results,
  model_name,
  correlation_threshold = 0.5,
  method = c("pearson", "spearman", "kendall"),
  plot = TRUE,
  label_mapping = NULL
)
```

## Arguments

- model_results:

  List returned by margot_causal_forest() (not bootstrap results). Must
  have been run with save_data = TRUE to access covariate data.

- model_name:

  Character string specifying which model to analyze

- correlation_threshold:

  Numeric threshold for considering variables correlated (default 0.5)

- method:

  Correlation method: "pearson", "spearman", or "kendall" (default
  "pearson")

- plot:

  Logical: Create correlation heatmap (default TRUE)

- label_mapping:

  Optional named list mapping variable names to labels. If NULL, uses
  automatic transformation via transform_var_name()

## Value

List containing:

- correlation_matrix: Full correlation matrix (with labelled row/column
  names)

- high_correlations: Pairs of variables with \|r\| \> threshold (with
  labels)

- correlation_clusters: Groups of intercorrelated variables (with
  labels)

- summary_text: Narrative summary of findings (using labels)
