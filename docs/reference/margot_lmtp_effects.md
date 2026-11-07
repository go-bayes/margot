# Tidy effects table from margot_lmtp() output

Converts the output of
[`margot_lmtp()`](https://go-bayes.github.io/margot/reference/margot_lmtp.md),
or one of its combined tables, into the tidy effects table that
[`margot_plot_effects()`](https://go-bayes.github.io/margot/reference/margot_plot_effects.md)
draws: one row per outcome and contrast, with the estimate, interval
limits, confidence level and scale. Intervals are rebuilt from standard
errors when a multiplicity correction or a different `alpha` is
requested, or when registered standard errors are supplied. The function
checks that the inputs agree before it returns anything.

## Usage

``` r
margot_lmtp_effects(
  x,
  contrast = NULL,
  outcomes = NULL,
  labels = NULL,
  adjust = c("none", "bonferroni"),
  alpha = 0.05,
  family_size = NULL,
  se_source = c("native", "supplied"),
  standard_errors = NULL,
  scale_info = NULL,
  standardised = FALSE,
  tol = 1e-06
)
```

## Arguments

- x:

  Output of
  [`margot_lmtp()`](https://go-bayes.github.io/margot/reference/margot_lmtp.md)
  (a list with `combined_tables` and `contrasts`), or one combined table
  with columns `"E[Y(1)]-E[Y(0)]"` or `"E[Y(1)]/E[Y(0)]"`, `"2.5 %"` and
  `"97.5 %"`, and outcomes as row names.

- contrast:

  Contrast names to keep (for example `"lower_vs_null"`), matching the
  suffix of `combined_outcomes_<contrast>`. `NULL` keeps every contrast
  of the full output; for a bare table it names the contrast (default
  `"contrast"`).

- outcomes:

  Outcomes that must be present. The call stops if any is missing,
  because a table can lack an outcome that was expected.

- labels:

  Named character vector of display labels, keyed by outcome. Defaults
  to the outcome names.

- adjust:

  `"none"` or `"bonferroni"`.

- alpha:

  Two-sided significance level before correction. Default 0.05.

- family_size:

  Number of outcomes in the correction family. Required with
  `adjust = "bonferroni"`.

- se_source:

  `"native"` (lmtp's standard errors, or those implied by a bare table's
  interval) or `"supplied"` (the `standard_errors` argument).

- standard_errors:

  With `se_source = "supplied"`, a data frame with columns `outcome`,
  `contrast` and `standard_error`, for example linearised standard
  errors from a registered inference procedure. Ratio standard errors
  are on the log scale.

- scale_info:

  Optional data frame of preparation constants keyed by `outcome`, in
  the format used by
  [`margot_plot()`](https://go-bayes.github.io/margot/reference/margot_plot.md):
  `scale`, and optionally `center`, `orientation` and `unit_multiplier`.
  The model outcome is orientation \* (Y - center) / scale. When
  supplied, differences are converted to original units (`raw_estimate`,
  `raw_lower`, `raw_upper`), and regime means are converted when
  `center` is present. Only identity transformations are supported;
  ratios have no original-unit conversion.

- standardised:

  `TRUE` when differences are in standard-deviation units. Controls
  E-values; regime means of standardised outcomes are dropped unless
  `scale_info` supplies `center`.

- tol:

  Numerical tolerance for the consistency checks.

## Value

A data frame with columns `outcome`, `label`, `contrast`, `estimate`,
`lower`, `upper`, `conf_level`, `type` (`"difference"` or `"ratio"`),
`standard_error`, `se_source`, `family_size`, `mean_reference` and
`mean_intervention` (estimated population means under the reference and
intervention regimes, when available), and, when computed, `evalue`,
`evalue_bound`, `raw_estimate`, `raw_lower` and `raw_upper`.

## Details

With the full
[`margot_lmtp()`](https://go-bayes.github.io/margot/reference/margot_lmtp.md)
output, standard errors and the regime means come from lmtp's contrast
objects (`x$contrasts`). Each table interval must equal g(estimate) -/+
1.96 SE from those objects, where g is the identity for differences and
the log for ratios; lmtp reports the standard error of a ratio on the
log scale. With a bare combined table, the standard error is recovered
from the 95% interval, which must be a symmetric Wald interval (on the
log scale for ratios); an asymmetric interval is refused rather than
recentred. A ratio in a bare table is read as a risk ratio; odds ratios
(`contrast_scale = "or"` in
[`margot_lmtp()`](https://go-bayes.github.io/margot/reference/margot_lmtp.md))
are refused when the contrast objects show them, because the E-value and
the regime-mean interpretation used here assume risk ratios.

Under `adjust = "bonferroni"` the interval is g^-1(g(estimate) -/+ z SE)
with z = qnorm(1 - alpha / (2 m)) and m = `family_size`. The family size
is required, so that the correction follows the registered family of
outcomes rather than the number of rows in a table.

E-values are computed for the reported interval on ratio scales and,
when `standardised = TRUE`, for differences in standard-deviation units
through RR = exp(0.91 d). They are omitted for differences on an
outcome's original scale, because the conversion assumes a standardised
outcome. margot's own E-value columns are not passed through.

## See also

[`margot_plot_effects()`](https://go-bayes.github.io/margot/reference/margot_plot_effects.md),
[`margot_save_effects_plot()`](https://go-bayes.github.io/margot/reference/margot_save_effects_plot.md)

## Examples

``` r
tab <- data.frame(
  `E[Y(1)]-E[Y(0)]` = c(0.12, -0.03),
  `2.5 %` = c(0.12, -0.03) - 1.96 * 0.02,
  `97.5 %` = c(0.12, -0.03) + 1.96 * 0.02,
  check.names = FALSE, row.names = c("y_warmth", "y_trust")
)
margot_lmtp_effects(tab, contrast = "lower_vs_null", adjust = "bonferroni",
  family_size = 2, standardised = TRUE)
#>    outcome    label      contrast estimate       lower      upper conf_level
#> 1 y_warmth y_warmth lower_vs_null     0.12  0.07517112 0.16482888      0.975
#> 2  y_trust  y_trust lower_vs_null    -0.03 -0.07482888 0.01482888      0.975
#>         type standard_error se_source family_size mean_reference
#> 1 difference     0.02000037    native           2             NA
#> 2 difference     0.02000037    native           2             NA
#>   mean_intervention   evalue evalue_bound
#> 1                NA 1.474132      1.34614
#> 2                NA 1.196324      1.00000
```
