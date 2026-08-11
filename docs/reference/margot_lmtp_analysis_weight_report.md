# Report full LMTP analysis-weight concentration

\`margot_lmtp_analysis_weight_report()\` multiplies the baseline design
weight by the cumulative joint exposure-and-censoring ratio at each
longitudinal node. Zero weights remain in every distribution and Kish
effective-sample-size denominator. The function reports aggregate
concentration without attaching a positivity or estimator-stability
judgement.

## Usage

``` r
margot_lmtp_analysis_weight_report(
  baseline_weights,
  joint_ratios,
  regularised_joint_ratios = NULL,
  wave_labels = NULL,
  policy_id = NULL,
  top_shares = c(0.01, 0.05, 0.1),
  regularisation = NULL
)
```

## Arguments

- baseline_weights:

  Non-negative baseline design weights, one per participant.

- joint_ratios:

  A non-negative matrix of per-node joint exposure-and-censoring ratios,
  with one participant per row and one longitudinal node per column.

- regularised_joint_ratios:

  Optional matrix after the registered numerical regularisation, with
  the same dimensions as \`joint_ratios\`.

- wave_labels:

  Optional longitudinal-node labels. Column names from \`joint_ratios\`
  are used when available.

- policy_id:

  Optional policy identifier recorded as provenance.

- top_shares:

  Fractions used for concentration summaries.

- regularisation:

  Optional aggregate description of the registered numerical
  regularisation.

## Value

An object of class \`margot_lmtp_analysis_weight_report\` containing
per-wave summaries, top-weight shares, aggregate zero and missingness
causes, regularisation comparisons, and provenance. The return value
contains no participant-level weights.
