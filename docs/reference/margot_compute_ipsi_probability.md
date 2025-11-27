# Compute IPSI counterfactual initiation probabilities

Takes a formatted transition matrix (for example, one of the formatted
tables produced by \`margot_transition_table()\` such as
\`transition_tables\$tables_data\[\[i\]\]\` or the paired
\`knitr_kable\` stored under \`transition_tables\$tables\[\[i\]\]\`) and
computes the natural initiation probability for baseline non-attenders
along with the counterfactual probabilities implied by marginal
incremental propensity score interventions (IPSIs) for a set of proposed
deltas.

## Usage

``` r
margot_compute_ipsi_probability(trans_matrix, deltas = c(2, 5, 10))
```

## Arguments

- trans_matrix:

  A data frame containing at least a \`"From / To"\` column, a \`"State
  1"\` column, and a \`"Total"\` column. Must include a row identified
  as \`"State 0"\` under \`"From / To"\`, representing non-attenders at
  the prior wave. You can pass either the raw data frame or the
  \`knitr_kable\` object returned inside \`margot_transition_table(...
  )\$tables\[\[i\]\]\` (the function automatically reads the attached
  \`table_data\` attribute).

- deltas:

  Numeric vector of IPSI shift magnitudes (greater than 1). The defaults
  match the standard IPSI reporting set \`c(2, 5, 10)\`.

## Value

A data frame with one row per delta containing the natural initiation
probability \`p\`, its exact 95 counterfactual probability \`p' = 1 -
(1 - p) / delta\`, and the fold increase \`p'/p\`. The result carries a
\`counts\` attribute with the raw initiation and at-risk totals and the
natural-probability interval used to compute \`p\`.

## Examples

``` r
trans <- data.frame(
  `From / To` = c("State 0", "State 1"),
  `State 0` = c(27380, 330),
  `State 1` = c(845, 170),
  Total = c(28225, 500),
  check.names = FALSE
)
margot_compute_ipsi_probability(trans)
#>   delta delta_inverse natural_p natural_p_l natural_p_u counterfactual_p
#> 1     2           0.5  0.029938  0.02798067  0.03199286        0.5149690
#> 2     5           0.2  0.029938  0.02798067  0.03199286        0.8059876
#> 3    10           0.1  0.029938  0.02798067  0.03199286        0.9029938
#>   fold_increase
#> 1      17.20118
#> 2      26.92189
#> 3      30.16213
margot_compute_ipsi_probability(trans, deltas = c(1.5, 3))
#>   delta delta_inverse natural_p natural_p_l natural_p_u counterfactual_p
#> 1   1.5         0.667  0.029938  0.02798067  0.03199286         0.353292
#> 2   3.0         0.333  0.029938  0.02798067  0.03199286         0.676646
#>   fold_increase
#> 1      11.80079
#> 2      22.60158
```
