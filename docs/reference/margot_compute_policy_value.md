# Bootstrap test-set policy value

Non-parametric bootstrap of the doubly-robust gain for a stored
\`policy_tree\`. Computes the difference between the mean gain under the
policy and the average treatment effect in the evaluation fold.

## Usage

``` r
margot_compute_policy_value(model, depth = 2L, R = 499L, seed = NULL)
```

## Arguments

- model:

  List; one outcome entry from a \`margot\` result.

- depth:

  Integer; depth of the stored \`policy_tree\` (1 or 2). Default: 2.

- R:

  Integer ≥ 199; number of bootstrap replicates. Default: 499.

- seed:

  Integer or \`NULL\`; RNG seed for reproducibility.

## Value

Object of class \`policy_value_test\` with components:

- estimate:

  Numeric; bootstrap estimate of policy gain minus ATE.

- std.err:

  Numeric; standard error of bootstrap replicates.

- p.value:

  Numeric; two-sided p-value.

- n_eval:

  Integer; number of evaluation observations.
