# Proposal: supplied density ratios for the LMTP outcome stage

Date: 2026-09-11. Status: PROPOSED. Scope: public API design and implementation acceptance requirements.

## Problem

Callers that fit exposure and censoring models need a supported interface to supply density ratios to the longitudinal modified treatment policy (LMTP) sequentially doubly robust (SDR) outcome stage. The existing interface uses unexported task-construction and outcome-fitting functions in `margot`, followed by replacement of generated folds. A public interface should validate the supplied ratios and partitions, specify the random-number procedure and record the outcome transformation.

The worker report fingerprints five `lmtp` objects: `cf_density_ratios`, `cf_sdr`, `theta_dr`, `make_shifted` and `LmtpTask`. Changes to called helpers or installed binaries can escape that comparison. The extension below records those dependencies and tests their use in worker admission and checkpoint identity.

## Proposed exports

### `margot_lmtp_sdr_from_ratios()`

```r
margot_lmtp_sdr_from_ratios(
  data, trt, outcome, baseline = NULL, time_vary = NULL, cens = NULL,
  shift, density_ratios, ratio_ids, folds, id, weights = NULL,
  outcome_type = c("continuous", "binomial"),
  bounds, learners_outcome, seed, rng_kind, rng_state = NULL,
  control = lmtp::lmtp_control(),
  label = "supplied ratios", ratio_digest = NULL
)
```

The interface has the following requirements:

- `density_ratios` is a finite, nonnegative numeric matrix in input-row and exposure-node order. `ratio_ids` identifies its rows and must equal the identifiers selected by `id` from `data`. The supplied ratios pass unchanged to the outcome regressions. Calling the density fitter is an error.
- `folds` accepts an integer fold-label vector or a list of training and validation index sets. Validation indices partition all input rows exactly once. Each fold has nonempty training and validation sets, unique in-range integer indices and disjoint sets; its training indices equal the complement of its validation indices. Repeated identifiers remain within a validation fold. Validate the complete partition before task construction or regression.
- `seed` is an explicit integer and `rng_kind` specifies the generator, normal generator and sampling method. Save the caller's RNG kind, state and seed-existence status, and restore them on success and error. Deterministically seed task construction and install the validated folds. Immediately before SDR fitting, restore the supplied `rng_state` when present, or reset the declared RNG kind and seed otherwise. Validate a supplied state against `rng_kind` and privately record the regression-start state. To preserve an existing procedure during migration, supply its recorded regression-start state; a matching scalar seed alone may omit random draws made during earlier task construction. Instrumented tests observe the partitions and RNG state inside the regression calls.
- `bounds` is explicit: numeric limits in the supplied outcome's scale, or `"observed_range"` for a continuous outcome. Validate the realised limits as finite, ordered and containing the observed values. Binary outcomes use limits zero and one. Return the realised limits in `$bounds_realised`.
- Record supplied and internally normalised weights with `normalised_by_engine`. Record the effective control settings, seed, RNG kind, supplied partitions and engine version with the fit. These fit records may contain private inputs and require an explicit export operation before public use.
- Return an ordinary `lmtp` object with class `c("margot_lmtp_supplied_ratio_fit", "lmtp")`, `$density_ratios` identical to the input, `$outcome_history` from the fitted task and `$provenance` containing `ratio_source = "supplied"`, `folds_source = "supplied"`, `ratio_digest` and `label`. The optional caller digest is an opaque reference; verifying it against a checkpoint belongs to the caller.

### `margot_lmtp_node_histories()`

```r
margot_lmtp_node_histories(trt, baseline = NULL, time_vary = NULL, cens = NULL, k = Inf)
```

Return the per-node history vectors using the task's construction rules and variable names alone. Test equality with the histories consumed by a fitted task on the same specification. History equality and partition equality are distinct assertions.

### Worker report extension

Extend `margot_lmtp_worker_report()` with:

- `lmtp_helper_fingerprint`: function-body digests for `estimate_density_ratios`, `stack_data`, `trim`, `run_ensemble`, `followed_rule`, `shift_data`, `shift_cens`, `get_folded_data` and `predict.lmtp_ensemble`.
- `build_fingerprint`: SHA-256 digests of installed `R/*.rdb`, `R/*.rdx` and `libs/*` files for `margot`, `lmtp`, `SuperLearner`, `ranger`, `xgboost`, `glmnet` and `nnls`.

Retain the existing fields for compatibility. Consumers adopting the stronger report must include the new fields in worker admission and checkpoint identity. A changed helper or build must reject an incompatible worker and invalidate the corresponding cached execution identity. Older identities remain explicitly associated with the earlier verification scheme.

## Acceptance

Use synthetic inputs for these tests:

- Instrument the SDR call to assert that its ratio matrix equals the supplied matrix. Replace the density fitter with a function that aborts; the outcome fit must still complete.
- Reject overlapping training and validation rows, incomplete validation coverage, duplicated indices, out-of-range indices, empty sets and identifiers split across validation folds. Accept equivalent label-vector and index-list partitions. Observe training and validation rows inside the regression calls; returned history metadata alone is insufficient evidence.
- Repeat calls from different ambient RNG states using the same declared seed and folds. Scientific outputs must agree on the same installed environment. Verify restoration of the caller's RNG kind and state after successful fitting and an injected regression error, covering both seed-existence states. Include a migration fixture using the recorded regression-start state from the earlier implementation and compare its scientific outputs.
- Reject omitted or invalid bounds. For `"observed_range"`, return the realised range and verify the transformation used by the task. Reject row-order disagreement between `ratio_ids` and `data`.
- Verify proportional weight normalisation and agreement of estimates, influence scores and standard errors under the declared numerical comparison rule. Assert `normalised_by_engine` against the actual supplied and internal vectors.
- Change a helper or build file in an isolated test environment. Verify both the changed report and the consumer's rejection of the mismatched worker or cached identity.

Existing public estimation functions retain their current interfaces. This proposal covers continuous and binary outcomes. Survival outcomes and competing events remain outside this interface. Implementation requires a minor version increment, a `NEWS.md` entry and updated reference documentation and site.
