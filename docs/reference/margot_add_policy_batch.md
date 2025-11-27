# attach policy-value tests to a subset of outcomes

this is the pared-back replacement for the earlier, more complex batch
helper. it simply iterates over the outcomes you pass in \`keep\`, calls
\`margot_add_policy_p()\` on each model, and invisibly returns the
updated \`cf_out\` so you can keep piping.

## Usage

``` r
margot_add_policy_batch(cf_out, keep, depth = 2L, R = 999L, seed = 2025L)
```

## Arguments

- cf_out:

  list. full \*\*margot\*\* result.

- keep:

  character. outcome names to update (without the \`model\_\` prefix).

- depth:

  integer. policy-tree depth. default 2.

- R, seed:

  bootstrap settings for the underlying test (defaults 999 / 2025).

## Value

the modified \`cf_out\` (invisibly).
