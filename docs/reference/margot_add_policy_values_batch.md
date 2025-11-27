# Attach policy-value tests to a batch of models

Runs \`margot_compute_policy_value()\` at specified depths for each
outcome, optionally in parallel via \*\*future\*\*/\*\*furrr\*\*.

runs \`margot_compute_policy_value()\` at the requested depths for each
outcome, in parallel (via \*\*future\*\*/\*\*furrr\*\*) or sequentially.

## Usage

``` r
margot_add_policy_values_batch(
  cf_out,
  outcomes = NULL,
  depths = c(1L, 2L),
  R = 499L,
  seed = 42L,
  parallel = FALSE
)

margot_add_policy_values_batch(
  cf_out,
  outcomes = NULL,
  depths = c(1L, 2L),
  R = 499L,
  seed = 42L,
  parallel = FALSE
)
```

## Arguments

- cf_out:

  list. a margot result containing \`\$results\` and \`\$outcome_vars\`.

- outcomes:

  character or NULL; outcome names without the \`model\_\` prefix.
  \`NULL\` → all outcomes.

- depths:

  integer vector; tree depths to evaluate. default \`c(1L, 2L)\`.

- R:

  integer ≥ 199; bootstrap replicates. default \`499L\`.

- seed:

  integer; rng seed. default \`42L\`.

- parallel:

  logical; run in parallel via \*\*furrr\*\*? default \`FALSE\`.

## Value

invisibly returns modified \`cf_out\` with added policy-value tests

invisibly returns the modified \`cf_out\`.
