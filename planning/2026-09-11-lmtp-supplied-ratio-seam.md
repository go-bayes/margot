# Proposal: a supported seam for supplied density ratios in the LMTP outcome stage

Date: 2026-09-11. Status: PROPOSED. Scope: public API only; no study, dataset, or private workflow detail.

## Problem

Downstream orchestration code that fits its own exposure and censoring models needs to hand `lmtp`'s sequentially doubly robust (SDR) outcome stage a ratio matrix, a fold map, and weights, and to be certain that no density-ratio fit runs. Today that requires three unexported internals, reached with `getFromNamespace()`: `margot_lmtp_make_task()`, `margot_lmtp_fit_sdr_outcome()`, and a post-construction assignment of the caller's fold map into the task object. Two consequences follow. A Margot release can change those internals without any signal to callers. And two engine defaults enter silently: `LmtpTask` derives continuous-outcome bounds from the observed outcome range when `bounds` is `NULL`, and it generates its own folds before the caller can replace them.

A second gap is in the worker probe. `margot_lmtp_worker_report()` fingerprints five `lmtp` internals (`cf_density_ratios`, `cf_sdr`, `theta_dr`, `make_shifted`, `LmtpTask`) by deparsed body. A locally modified helper that those functions call, such as `estimate_density_ratios`, `stack_data`, `trim`, `run_ensemble`, `followed_rule`, `shift_data`, `shift_cens`, `get_folded_data`, or `predict.lmtp_ensemble`, passes the probe while changing the fitted result, and so does a rebuilt binary of an unchanged source.

## Proposed exports

### `margot_lmtp_sdr_from_ratios()`

```r
margot_lmtp_sdr_from_ratios(
  data, trt, outcome, baseline = NULL, time_vary = NULL, cens = NULL,
  shift, density_ratios, folds, id, weights = NULL,
  outcome_type = c("continuous", "binomial"),
  bounds,                      # numeric length 2, or "observed_range"
  learners_outcome, control = lmtp::lmtp_control(),
  label = "supplied ratios", ratio_digest = NULL
)
```

Behaviour:

- Builds the task through the existing internals, installs `folds` (a list of `training_set` and `validation_set` index vectors, or an integer fold id vector) before any regression, and never calls `cf_density_ratios()`.
- Verifies that `density_ratios` is a numeric matrix with one row per record in `data` order and one column per exposure node, finite and non-negative, and that `id` order matches the task's identifiers.
- Requires `bounds`. `"observed_range"` is accepted only as an explicit election; the realised bounds are returned in `$bounds_realised` either way.
- Records the supplied weights and the task's internal weights side by side with a `normalised_by_engine` flag, because `lmtp` 1.5.4 rescales weights to mean one outside its tolerance.
- Returns an ordinary `lmtp` object with class `c("margot_lmtp_supplied_ratio_fit", "lmtp")`, `$density_ratios` identical to the input, `$outcome_history` (the per-node history the engine actually used), and `$provenance` (`ratio_source = "supplied"`, `ratio_digest`, `folds_source = "supplied"`, `label`).

### `margot_lmtp_node_histories()`

```r
margot_lmtp_node_histories(trt, baseline = NULL, time_vary = NULL, cens = NULL, k = Inf)
```

Returns the per-node history vectors the outcome stage will condition on, using the same construction as the task, without an outcome and without data. Orchestrators use it to assert that their declared histories equal the engine's before any fit.

### `margot_lmtp_worker_report()` extension

Add two fields and change none:

- `lmtp_helper_fingerprint`: deparsed-body digests of the nine called helpers listed above, alongside the existing five.
- `build_fingerprint`: SHA-256 digests of the installed `R/*.rdb`, `R/*.rdx`, and `libs/*` files for `margot`, `lmtp`, `SuperLearner`, `ranger`, `xgboost`, `glmnet`, and `nnls`.

`source_fingerprint` is unchanged, so existing task identities remain valid; schedulers may opt into the stricter identity by including the new fields.

## Non-goals

No change to `margot_lmtp()`, `margot_lmtp_sdr_shared()`, or the task scheduler's density stage. No new estimator. No change to reporting. Survival outcomes and competing events remain outside the seam, as they are outside `margot_lmtp_sdr_shared()` today.

## Tests (synthetic)

- Supplied ratios flow through unchanged: `identical(fit$density_ratios, supplied)`.
- The density fitter is never reached: `local_mocked_bindings()` on the internal accessor makes `cf_density_ratios` abort; the seam still returns.
- The supplied fold map is the one the regressions used, checked against `fit$outcome_history` and the task's fold sets.
- A call without `bounds` errors; `"observed_range"` returns the realised range.
- Weights multiplied by a constant leave estimates, influence scores, and standard errors unchanged and set `normalised_by_engine` to `TRUE`.
- `margot_lmtp_node_histories()` equals the histories reported by a fitted task on the same specification.
- The worker report changes when a helper body or a build file changes (simulated with a temporary library).

## Release

Minor version bump at implementation (new exported API), a `NEWS.md` entry, and pkgdown reference entries. No study-derived example appears in documentation.
