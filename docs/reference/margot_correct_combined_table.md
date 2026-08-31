# Correct a "combined table" for multiplicity \*\*and\*\* recompute \*E\*-values

\`margot_correct_combined_table()\` takes the \*\*combined_table\*\*
produced by the various \*margot\* models (or by your own code) and

1.  applies the chosen confidence-interval adjustment, \*\*and\*\*

2.  recalculates \*E\*-values (and their lower bounds) so they match the
    new interval.

By default it implements the single–step \*\*Bonferroni\*\* correction
at \\\alpha = 0.05\\ as advocated by VanderWeele & Mathur (2019).

## Usage

``` r
margot_correct_combined_table(
  combined_table,
  adjust = c("bonferroni", "holm", "BH", "none"),
  alpha = 0.05,
  scale = c("RD", "RR"),
  delta = 1,
  sd = 1,
  m = NULL
)
```

## Arguments

- combined_table:

  A data frame with \*at least\* the columns

  - \`E\[Y(1)\]-E\[Y(0)\]\` \*\*or\*\* \`E\[Y(1)\]/E\[Y(0)\]\`

  - \`2.5

  Extra columns (e.g. the original \*E\*-values) are carried through.

- adjust:

  Multiplicity method: \`"bonferroni"\` (default), \`"holm"\`, \`"BH"\`,
  or \`"none"\`. Bonferroni and Holm provide strong FWER control; BH
  provides FDR control. \`"none"\` retains the supplied confidence
  limits.

- alpha:

  Family-wise error-rate (for bonferroni/holm) or false discovery rate
  (for BH) to control. Default \`0.05\`.

- scale:

  Scale used to recompute the \*E\*-value. \`"RD"\` is the legacy label
  for the standardised-continuous-outcome approximation from an
  outcome-mean difference or ATE; \`"RR"\` treats the estimate as a risk
  ratio.

- delta:

  Exposure contrast represented by an outcome-mean difference, used only
  when \`scale = "RD"\`.

- sd:

  Outcome standard deviation used to standardise an outcome-mean
  difference, used only when \`scale = "RD"\`.

- m:

  Positive whole number giving the total number of tests in the
  Bonferroni family. It must be at least the number of table rows. When
  \`NULL\`, Margot uses the number of rows. Holm and BH continue to use
  the rows supplied in \`combined_table\` as their adjustment family.

## Value

A data frame with the same rows (and order) as \`combined_table\`, but
with

- updated \`2.5

- freshly computed \`E_Value\` and \`E_Val_bound\`.

Numeric columns retain their computational precision. Apply display
rounding only when formatting the returned table for presentation.

## E-value calculation

For a risk ratio \\r\\, let \\r^\* = r\\ when \\r \ge 1\\ and \\r^\* =
1/r\\ otherwise. Margot computes the null E-value as \$\$r^\* +
\sqrt{r^\*(r^\*-1)}.\$\$ The confidence-bound E-value uses the
confidence limit closest to 1 and equals 1 when the interval includes 1.

For an outcome-mean difference \\b\\, exposure contrast \\\delta\\, and
outcome standard deviation \\s\\, Margot first forms the standardised
difference \\d = b\|\delta\|/s\\. It approximates the risk ratio as
\\\exp(0.91d)\\ and the risk-ratio confidence limits as \\\exp(0.91d \pm
1.78\\\mathrm{SE}(d))\\, then applies the same null E-value equation.
This calculation treats \\s\\ as known.

## How the correction is applied

For Bonferroni, let \\m\\ be the total number of tests in the
multiplicity family.

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

VanderWeele TJ, Ding P (2017). Sensitivity analysis in observational
research: introducing the E-value. \*Annals of Internal Medicine\*
167(4): 268–274.
[doi:10.7326/M16-2607](https://doi.org/10.7326/M16-2607) .

Chinn S (2000). A simple method for converting an odds ratio to effect
size for use in meta-analysis. \*Statistics in Medicine\* 19(22):
3127–3131.

VanderWeele TJ (2017). On a square-root transformation of the odds ratio
for a common outcome. \*Epidemiology\* 28(6): e58.
