# Fit SDR from verified, previously estimated components

Use with
[`margot_lmtp()`](https://go-bayes.github.io/margot/reference/margot_lmtp.md)
through its `lmtp_model_type` argument. The caller supplies node density
ratios, the realised fold map and the RNG procedure. No exposure or
observation model is fitted and no additional cap is applied. The caller
remains responsible for the provenance and scientific validity of the
components. Input identities are checked before outcome fitting.

## Usage

``` r
margot_lmtp_sdr_from_components(
  data,
  trt,
  outcome,
  shift,
  components,
  baseline,
  time_vary,
  learners_outcome,
  bounds,
  control = lmtp::lmtp_control(.trim = 1),
  progress_bar = function(...) invisible(NULL),
  learners_trt = NULL
)
```

## Arguments

- data:

  Analysis frame, including one continuous outcome and `id`.

- trt:

  Ordered exposure column names.

- outcome:

  Outcome column name.

- shift:

  Registered shift function.

- components:

  List containing `frame` (all non-outcome analysis columns), `history`,
  `baseline`, `time_vary`, `trt`, `cens`, `density_ratios`, `folds`,
  `weights`, `seed`, `rng_kind`, `shift_source_sha256` and
  `shifted_exposures`.

- baseline:

  Baseline covariate names, identical to `components$baseline`.

- time_vary:

  List of covariates entering each node, identical to
  `components$time_vary`, including node order and within-node variable
  order.

- learners_outcome:

  Outcome learner library.

- bounds:

  Explicit outcome limits on the analysis scale.

- control:

  An `lmtp_control` object with `.trim = 1`.

- progress_bar:

  Optional native SDR progress callback.

- learners_trt:

  Ignored; exposure components are already supplied.

## Value

An ordinary `lmtp` fit with supplied folds, histories, bounds and RNG
provenance attached.

## Details

The current implementation supports continuous outcomes and requires a
unique, nonmissing `id` column. It uses the internal interface of `lmtp`
1.5.4; other versions are rejected by the shared interface guard. At
least two outer folds must be supplied, and the active future plan must
provide exactly one worker per fold. This function does not change the
future plan. Exposure waves may be nonconsecutive, and each node may
include covariates from multiple earlier surveys; the caller declares
when each covariate enters.
