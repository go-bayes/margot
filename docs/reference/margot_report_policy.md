# summarise policy tests (add if absent, adjust, return tidy table)

this convenience wrapper makes a complete, end-to-end policy report in
one line. it: 1. optionally runs the bootstrap test for the selected
outcomes (via \`margot_add_policy_batch()\`), 2. pulls the
depth‑specific results with \`margot_policy_summary()\`, and 3. returns
the tidy summary.

## Usage

``` r
margot_report_policy(
  cf_out,
  keep = NULL,
  depth = 2L,
  adjust = "bonferroni",
  alpha = 0.05,
  R = 999L,
  seed = 2025L
)
```

## Arguments

- cf_out:

  \*\*margot\*\* result list.

- keep:

  character. which outcomes to include; \`NULL\` keeps all.

- depth:

  integer. policy-tree depth. default 2.

- adjust:

  character. multiplicity adjustment method for \*p\*-values. default
  "bonferroni".

- alpha:

  numeric. significance level for the pass/fail flag. default 0.05.

- R, seed:

  bootstrap settings if tests need to be (re)run.

## Value

tibble. one row per outcome with adjusted \*p\*-values and pass flag.

## Details

nothing is printed; you decide what to do with the tibble (view, knit,
etc.).
