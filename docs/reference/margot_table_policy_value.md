# Create a held-out policy-value reporting table

Summarises cross-validated policy-tree values against all-control,
all-treatment, and best-constant baselines.

## Usage

``` r
margot_table_policy_value(object, model_name = NULL, depth = NULL, digits = 3L)
```

## Arguments

- object:

  A `margot_policy_tree_cv` object.

- model_name:

  Optional model name, with or without the `model_` prefix.

- depth:

  Optional tree depth. If `NULL`, all depths are returned.

- digits:

  Integer; rounding used in formatted columns.

## Value

A tibble with held-out policy-value summaries.
