# Report descriptive censoring evidence for an LMTP analysis

\`margot_lmtp_censoring_report()\` reports observed retention and, when
supplied, fitted continued-observation probabilities and censoring
factors. The function returns aggregate evidence only. It neither
classifies censoring support nor returns a route action.

## Usage

``` r
margot_lmtp_censoring_report(
  observed,
  baseline_weights = NULL,
  fitted_probabilities = NULL,
  censoring_factors = NULL,
  joint_ratios = NULL,
  wave_labels = NULL,
  policy_id = NULL,
  learner_specification = NULL,
  out_of_fold_performance = NULL,
  na_is_unobserved = TRUE
)
```

## Arguments

- observed:

  A logical or \`0\`/\`1\` matrix with one participant per row and one
  censoring transition per column. A vector represents one transition.

- baseline_weights:

  Optional non-negative baseline design weights, one per participant.
  Equal weights are used when this argument is \`NULL\`.

- fitted_probabilities:

  Optional matrix of fitted probabilities of continued observation, with
  the same dimensions as \`observed\`.

- censoring_factors:

  Optional matrix of separately identified censoring density-ratio
  factors, with the same dimensions as \`observed\`.

- joint_ratios:

  Optional matrix of joint exposure-and-censoring density ratios, with
  the same dimensions as \`observed\`.

- wave_labels:

  Optional transition labels. Column names from \`observed\` are used
  when available.

- policy_id:

  Optional policy identifier recorded as provenance.

- learner_specification:

  Optional aggregate description of the registered censoring learners.

- out_of_fold_performance:

  Optional aggregate out-of-fold performance record for the censoring
  learners.

- na_is_unobserved:

  Logical; whether an \`NA\` observation indicator denotes loss to
  follow-up. The default is \`TRUE\`.

## Value

An object of class \`margot_lmtp_censoring_report\` containing
retention, probability, factor, zero-cause, learner, and provenance
records. The computed tables contain no participant-level rows; supplied
learner records must likewise be aggregate.
