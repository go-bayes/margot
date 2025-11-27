# Identify Variable Clusters for Policy Tree Analysis

Groups variables into clusters based on their correlations, helping to
understand which variables might substitute for each other in policy
tree splits.

## Usage

``` r
margot_identify_variable_clusters(
  correlation_assessment,
  min_cluster_size = 2,
  label_mapping = NULL
)
```

## Arguments

- correlation_assessment:

  Output from margot_assess_variable_correlation()

- min_cluster_size:

  Minimum number of variables to form a cluster (default 2)

- label_mapping:

  Optional named list mapping variable names to labels. If NULL and
  correlation_assessment contains label_mapping, uses that; otherwise
  uses automatic transformation

## Value

Data frame with cluster assignments and representative variables (using
labels)
