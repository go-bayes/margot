# Linearised inference for weighted LMTP policy means and additive contrasts

\`margot_lmtp_linearised_inference()\` recomputes the standard error of
a weighted policy-specific mean, or of an additive contrast between two
policy means fitted to the same participants, from the pseudo-outcomes
and weights stored in \`lmtp\` fits. It uses the linearisation of the
weighted (Hájek) mean, \$\$\hat\theta = \sum_i w_i \varphi_i / \sum_i
w_i, \qquad \widehat{\mathrm{se}}(\hat\theta) = \\{ s^2(u) / n
\\}^{1/2}, \qquad u_i = w_i(\varphi_i - \hat\theta)/\bar w,\$\$ where
\\(\hat\theta\\) is the estimate stored in the fit (for the SDR
estimator, the weighted mean shown), \\(\varphi_i\\) is participant
\\(i\\)'s uncentred pseudo-outcome, \\(w_i\\) the baseline weight,
\\(\bar w\\) the mean weight and \\(s^2\\) the sample variance. For a
contrast, \\(\varphi_i\\) is the difference of the two pseudo-outcomes.
With repeated identifiers, the contributions are summed within clusters
and the cluster-robust form used by \`ife\` is applied.

## Usage

``` r
margot_lmtp_linearised_inference(fit, ref = NULL, conf_level = NULL)
```

## Arguments

- fit:

  An \`lmtp\` fit, or the \`ife\` estimate stored in its \`estimate\`
  element.

- ref:

  Optional reference fit of the same form. When supplied, the function
  reports the additive contrast \`fit\` minus \`ref\`. Both fits must
  carry identical weights and identifiers.

- conf_level:

  Confidence level for the two-sided Wald interval. Defaults to the
  level implied by the critical value stored in \`fit\`.

## Value

A one-row data frame with the estimate, the linearised standard error,
interval limits, a two-sided p-value against zero (contrasts only), the
standard error reported by \`lmtp\`, the number of participants and
clusters, and the variance method.

## Details

\`lmtp\` 1.5.4 with \`ife\` 0.2.5 computes \\(s^2(w_i\varphi_i)/n\\),
which centres the weighted product rather than the pseudo-outcome. The
two agree when the weights are constant and differ when they vary. The
difference can be large for policy means on a scale far from zero and
depends on the contrast's size for additive contrasts. Point estimates
are unchanged. The function fits no model and does not account for
estimation of the weights.
