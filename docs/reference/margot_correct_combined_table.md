# Correct a "combined table" for multiplicity \*\*and\*\* recompute \*E\*-values

\`margot_correct_combined_table()\` takes the \*\*combined_table\*\*
produced by the various \*margot\* models (or by your own code) and

1.  widens the confidence interval according to the chosen
    family–wise-error correction, \*\*and\*\*

2.  recalculates \*E\*-values (and their lower bounds) so they match the
    new interval.

By default it implements the single–step \*\*Bonferroni\*\* correction
at \\\alpha = 0.05\\ as advocated by VanderWeele & Mathur (2019).

## Usage

``` r
margot_correct_combined_table(
  combined_table,
  adjust = c("bonferroni", "holm", "BH"),
  alpha = 0.05,
  scale = c("RD", "RR"),
  delta = 1,
  sd = 1
)
```

## Arguments

- combined_table:

  A data frame with \*at least\* the columns

  - \`E\[Y(1)\]-E\[Y(0)\]\` \*\*or\*\* \`E\[Y(1)\]/E\[Y(0)\]\`

  - \`2.5

  Extra columns (e.g. the original \*E\*-values) are carried through.

- adjust:

  Multiplicity method: \`"bonferroni"\` (default), \`"holm"\`, or
  \`"BH"\`. Bonferroni and Holm provide strong FWER control; BH provides
  FDR control.

- alpha:

  Family-wise error-rate (for bonferroni/holm) or false discovery rate
  (for BH) to control. Default \`0.05\`.

- scale:

  Scale to use when recomputing the \*E\*-value. \`"RD"\` (risk
  difference / ATE, \*\*default\*\*) or \`"RR"\` (risk ratio).

- delta, sd:

  Arguments passed to \[EValue::evalues.OLS()\] when \`scale = "RD"\`.
  Ignored for \`"RR"\`.

## Value

A data frame with the same rows (and order) as \`combined_table\`, but
with

- updated \`2.5

- freshly computed \`E_Value\` and \`E_Val_bound\`.

## How the correction is applied

Let \\m\\ be the number of rows (tests).

- \*\*Bonferroni\*\* uses \$\$ z^\* =
  \Phi^{-1}\\\bigl(1-\alpha/(2m)\bigr) \$\$ and rescales the original
  half-width.

- \*\*Holm\*\* first step-down adjusts the (two-sided) \*p\*-value for
  each test, then back-calculates a \*symmetric\* CI whose coverage
  matches the adjusted \*p\*. Point estimates \*\*never\*\* change.

- \*\*BH\*\* (Benjamini-Hochberg) applies FDR correction to
  \*p\*-values, then back-calculates symmetric CIs. Controls false
  discovery rate rather than family-wise error rate.

## References

VanderWeele TJ, Mathur MB (2019). \*Some desirable properties of the
Bonferroni correction: Is the Bonferroni correction really so bad?\*
\*\*Am J Epidemiol\*\* 188(3): 617–618.
