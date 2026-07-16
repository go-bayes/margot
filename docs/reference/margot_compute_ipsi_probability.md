# Compute IPSI counterfactual transition probabilities

Takes a formatted transition matrix (for example, one of the formatted
tables produced by \`margot_transition_table()\` such as
\`transition_tables\$tables_data\[\[i\]\]\` or the paired
\`knitr_kable\` stored under \`transition_tables\$tables\[\[i\]\]\`) and
computes the natural probability of moving to the target state along
with the counterfactual probabilities implied by incremental propensity
score interventions (IPSIs) for a set of proposed deltas.

## Usage

``` r
margot_compute_ipsi_probability(
  trans_matrix,
  deltas = c(2, 5, 10),
  direction = c("up", "down")
)
```

## Arguments

- trans_matrix:

  A data frame containing at least a \`"From / To"\` column, the two
  state columns (\`"State 0"\`, \`"State 1"\`), and a \`"Total"\`
  column. You can pass either the raw data frame or the \`knitr_kable\`
  object returned inside \`margot_transition_table(...
  )\$tables\[\[i\]\]\` (the function automatically reads the attached
  \`table_data\` attribute).

- deltas:

  Numeric vector of IPSI shift magnitudes (greater than 1), interpreted
  as in \`lmtp::ipsi()\` (risk ratio on the probability of not reaching
  the target state). The defaults match the standard IPSI reporting set
  \`c(2, 5, 10)\`.

- direction:

  Either \`"up"\` (State 0 cohort moving to State 1; the default) or
  \`"down"\` (State 1 cohort moving to State 0).

## Value

A data frame with one row per delta containing the direction, the
natural transition probability \`p\`, its exact 95 confidence limits,
the counterfactual probability \`p' = 1 - (1 - p) / delta\`, and the
fold increase \`p'/p\`. The result carries a \`counts\` attribute with
the raw event and at-risk totals (\`events\`, \`at_risk\`; the legacy
names \`initiations\` and \`non_attenders\` are kept as aliases) and the
natural-probability interval used to compute \`p\`.

## Details

The delta here matches \`lmtp::ipsi()\`, which implements the risk-ratio
incremental propensity score intervention: with probability \\1 -
1/\delta\\ the intervention sets the exposure to the target state, and
otherwise leaves the natural value. The counterfactual probability of
the target state is therefore \\p' = 1 - (1 - p)/\delta\\: the natural
probability of \*not\* reaching the target state is divided by
\\\delta\\. This is not the odds-multiplier IPSI of Kennedy (2019),
where \\\delta\\ multiplies the odds of exposure; describe registered
estimands accordingly.

Direction selects the transition of interest. With \`direction = "up"\`
(the default, matching earlier versions) the at-risk cohort is the
\`"State 0"\` row and the event is arrival in \`"State 1"\`
(initiation). With \`direction = "down"\` the at-risk cohort is the
\`"State 1"\` row and the event is arrival in \`"State 0"\` — the
natural reading for a contingency that raises the probability of the
lower state among the upper-state cohort.

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
#>   direction delta delta_inverse natural_p natural_p_l natural_p_u
#> 1        up     2           0.5  0.029938  0.02798067  0.03199286
#> 2        up     5           0.2  0.029938  0.02798067  0.03199286
#> 3        up    10           0.1  0.029938  0.02798067  0.03199286
#>   counterfactual_p fold_increase
#> 1        0.5149690      17.20118
#> 2        0.8059876      26.92189
#> 3        0.9029938      30.16213
margot_compute_ipsi_probability(trans, deltas = c(1.5, 3))
#>   direction delta delta_inverse natural_p natural_p_l natural_p_u
#> 1        up   1.5         0.667  0.029938  0.02798067  0.03199286
#> 2        up   3.0         0.333  0.029938  0.02798067  0.03199286
#>   counterfactual_p fold_increase
#> 1         0.353292      11.80079
#> 2         0.676646      22.60158
margot_compute_ipsi_probability(trans, direction = "down")
#>   direction delta delta_inverse natural_p natural_p_l natural_p_u
#> 1      down     2           0.5      0.66   0.6166258    0.701469
#> 2      down     5           0.2      0.66   0.6166258    0.701469
#> 3      down    10           0.1      0.66   0.6166258    0.701469
#>   counterfactual_p fold_increase
#> 1            0.830      1.257576
#> 2            0.932      1.412121
#> 3            0.966      1.463636
```
