# summarise ate, rate and policy tests across all outcomes

pared‑back copy of the original (pre‑refactor) helper that pulls key
diagnostics – average treatment effect, two rate metrics (AUTOC & QINI)
and the depth‑specific policy \*p\*‑value – for every outcome in the
object. returns a tibble ready for downstream plotting or reporting.

## Usage

``` r
margot_summarise_all(
  cf_out,
  target = c("AUTOC", "QINI", "both"),
  adjust = c("bonferroni", "holm", "BH", "none"),
  alpha = 0.05
)
```

## Arguments

- cf_out:

  margot result list.

- target:

  character. keep both rate metrics (\`"both"\`, default) or drop one:
  \`"AUTOC"\` or \`"QINI"\`.

- adjust:

  character. multiplicity adjustment method (see \[stats::p.adjust()\]).
  default "bonferroni".

- alpha:

  numeric. confidence level for rate intervals (default 0.05).

## Value

tibble. one row per outcome.
