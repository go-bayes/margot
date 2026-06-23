# Outcome-blind exposure overlap diagnostics

\`margot_exposure_overlap()\` estimates or accepts propensity scores
using only the exposure and baseline covariates. It is intended for
registration-stage support checks before any outcome model, causal
forest, LMTP estimator, or causal contrast is fitted.

## Usage

``` r
margot_exposure_overlap(
  data,
  exposure,
  covariates,
  weights = NULL,
  method = c("supplied", "logistic", "probability_forest"),
  bounds = c(0.05, 0.95),
  plot = TRUE,
  propensity = NULL,
  grf_defaults = list(),
  seed = 12345,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame containing the exposure and, when \`covariates\` is a
  character vector, the baseline covariates.

- exposure:

  Character string naming a binary exposure coded 0/1.

- covariates:

  Character vector of baseline covariate names, or a numeric matrix/data
  frame with one row per observation in \`data\`.

- weights:

  Optional character string naming design weights in \`data\`, or a
  numeric vector with one value per observation.

- method:

  Character. One of \`"supplied"\`, \`"logistic"\`, or
  \`"probability_forest"\`.

- bounds:

  Numeric length-2 vector giving the lower and upper propensity score
  bounds used for overlap and trimming summaries.

- plot:

  Logical; if \`TRUE\`, return a propensity-score histogram.

- propensity:

  Optional numeric vector of propensity scores. Required for \`method =
  "supplied"\`.

- grf_defaults:

  Optional list passed to \`grf::probability_forest()\` when \`method =
  "probability_forest"\`.

- seed:

  Integer seed used by the probability forest path.

- verbose:

  Logical; emit progress messages.

- ...:

  Reserved. Outcome-like arguments supplied here trigger an error.

## Value

A list with support summaries, balance summaries, optional plot, and
enough metadata to document the outcome-blind exposure diagnostic.
