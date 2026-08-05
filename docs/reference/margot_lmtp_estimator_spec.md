# Lock the execution settings for a Margot LMTP analysis

\`margot_lmtp_estimator_spec()\` records the arguments that determine an
LMTP fit and protects them with a content hash. The specification
belongs to Margot and requires no companion package. It deliberately
records execution settings alone: the causal question, causal estimand,
identification assumptions, and policy rationale remain in the study
protocol.

## Usage

``` r
margot_lmtp_estimator_spec(
  trt,
  outcomes,
  policies,
  seed,
  baseline = NULL,
  time_vary = NULL,
  cens = NULL,
  compete = NULL,
  outcome_type = c("continuous", "binomial"),
  id = NULL,
  folds = 5L,
  bounds = NULL,
  learner_profile = c("ensemble", "glm"),
  trim = 0.999,
  weight_column = NULL
)
```

## Arguments

- trt:

  Character vector naming the exposure at each policy node.

- outcomes:

  Character vector naming the terminal outcomes.

- policies:

  Named logical vector. Each name is a policy arm and each value is the
  \`mtp\` setting passed to \`lmtp\` for that arm. Continuous shifts,
  including a natural-course arm represented by \`shift = NULL\`,
  ordinarily use \`TRUE\`.

- seed:

  Single whole-number estimation seed.

- baseline:

  Optional character vector of baseline covariates.

- time_vary:

  Optional time-varying covariate specification passed to \`lmtp\`.

- cens:

  Optional character vector of censoring indicators.

- compete:

  Optional character vector of competing-event indicators.

- outcome_type:

  Outcome model, \`"continuous"\` or \`"binomial"\`.

- id:

  Optional participant identifier column.

- folds:

  Number of cross-fitting folds.

- bounds:

  Optional common outcome bounds passed to \`lmtp\`.

- learner_profile:

  Registered learner profile, \`"glm"\` or \`"ensemble"\`.

- trim:

  Pooled density-ratio quantile cap passed to \[lmtp::lmtp_control()\].

- weight_column:

  Optional data column containing non-negative analysis weights. The
  values remain in the analysis data rather than the specification.

## Value

An object of class \`margot_lmtp_estimator_spec\`.

## Details

The specification can name several terminal outcomes. When it is passed
to \[margot_lmtp()\] with \`reuse_density_ratios = TRUE\`, Margot fits
each policy-specific treatment and censoring density-ratio process once
and reuses it across those outcomes.
