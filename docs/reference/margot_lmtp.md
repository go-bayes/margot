# Batch Process LMTP Models

This function runs multiple Longitudinal Modified Treatment Policy
(LMTP) models for specified outcome variables, calculates contrasts,
creates evaluation tables, and optionally saves checkpoints and the
complete output as \`.rds\` files.

## Usage

``` r
margot_lmtp(
  data,
  outcome_vars = NULL,
  trt = NULL,
  shift_functions = list(),
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = c("pairwise", "null"),
  contrast_scale = c("additive", "rr", "or"),
  lmtp_defaults = list(),
  n_cores = parallel::detectCores() - 1,
  models_in_parallel = NULL,
  cv_workers = NULL,
  save_output = FALSE,
  save_path = here::here("push_mods"),
  base_filename = "lmtp_output",
  use_timestamp = FALSE,
  prefix = NULL,
  manage_future_plan = FALSE,
  progress = c("cli", "progressr", "none"),
  seed = NULL,
  reuse_density_ratios = FALSE,
  stages = c("all", "density", "outcome"),
  estimator_spec = NULL
)
```

## Arguments

- data:

  A data frame containing all necessary variables.

- outcome_vars:

  A character vector of outcome variable names to be modelled. Optional
  when \`estimator_spec\` is supplied, which locks it.

- trt:

  A character string specifying the treatment variable. Optional when
  \`estimator_spec\` is supplied, which locks it.

- shift_functions:

  A list of shift functions to be applied. Each function should take
  \`data\` and \`trt\` as arguments.

- include_null_shift:

  Logical, whether to include a null shift. Default is TRUE.

- lmtp_model_type:

  The LMTP model function to use. Default is lmtp_tmle.

- contrast_type:

  Type of contrasts to compute: "pairwise" or "null". Default is
  "pairwise".

- contrast_scale:

  Scale for contrasts: "additive", "rr", or "or". Default is "additive".

- lmtp_defaults:

  A list of default parameters for the LMTP models. Must be empty when
  \`estimator_spec\` is supplied, which builds the whole list from the
  specification.

- n_cores:

  Total number of CPU cores to budget for the batch run. Default is
  detectCores() - 1 (includes efficiency cores on Apple Silicon, so set
  manually if you want to cap at performance cores).

- models_in_parallel:

  Optional cap on how many LMTP models to run at once. Defaults to
  floor(n_cores / cv_workers).

- cv_workers:

  Number of workers consumed internally by each LMTP fit (usually the
  cross-validation folds). Defaults to future::nbrOfWorkers().

- save_output:

  Logical, whether to save per-model checkpoints and the complete
  output. Saved artefacts are written as \`.rds\` files. Default is
  FALSE.

- save_path:

  The directory path to save the output. Default is "push_mods" in the
  current working directory. A run that keeps checkpoints beyond itself
  — any \`stages\` other than \`"all"\`, or \`save_output = TRUE\` on
  the task-parallel shared route — must supply this argument explicitly
  and errors with \`margot_error_save_path_required\` otherwise, because
  where those artefacts live is the investigator's decision rather than
  Margot's.

- base_filename:

  The base filename for saving the output. Default is "lmtp_output".

- use_timestamp:

  Logical, whether to include a timestamp in the filename. Default is
  FALSE.

- prefix:

  Optional prefix to add to the saved output filename. Default is NULL.

- manage_future_plan:

  Logical, whether Margot schedules the outer model futures. Default is
  FALSE. On the independent route (\`reuse_density_ratios = FALSE\`),
  TRUE sets up nested futures (outer loop for models, inner loop for
  cross-validation) and restores the caller's plan on exit. On the
  shared density-ratio route (\`reuse_density_ratios = TRUE\`), TRUE
  selects task-parallel scheduling: see the "Scheduling modes" section.
  When FALSE, models run one at a time and each fit uses the caller's
  external \`future::plan()\` for parallel cross-fitting.

- progress:

  Progress reporting method: "cli" (default CLI progress bar),
  "progressr" (use progressr package handlers), or "none" (no progress
  reporting).

- seed:

  Optional single whole number seeding every stochastic step: the RNG at
  entry, each model fit, and the parallel streams. Default NULL leaves
  the RNG untouched. When \`estimator_spec\` is supplied the seed comes
  from the locked specification, and supplying a different one errors.

- reuse_density_ratios:

  Logical. When \`TRUE\`, sequentially doubly robust fits sharing one
  policy-specific nuisance identity fit the treatment and censoring
  density ratios once and reuse them across \`outcome_vars\`. The
  returned models, contrasts, and tables retain the existing Margot
  structure. Default is \`FALSE\` while the opt-in path is validated.

- stages:

  Which stages of the shared density-ratio route to execute: \`"all"\`
  (default) fits the density stage and every outcome stage in one call;
  \`"density"\` fits the policy-specific density-ratio stage alone and
  returns its diagnostics for positivity assessment; \`"outcome"\` fits
  the outcome stages from density checkpoints already written. Anything
  other than \`"all"\` requires \`reuse_density_ratios = TRUE\`,
  \`manage_future_plan = TRUE\`, and \`save_output = TRUE\`. See the
  "Stage-split execution" section.

- estimator_spec:

  Optional locked \`margot_lmtp_estimator_spec\` object from
  \[margot_lmtp_estimator_spec()\]. When supplied, the \`lmtp\` call is
  built from the specification and every conflicting user argument
  errors.

## Value

A list containing:

- models:

  A list of all LMTP models for each outcome and shift function.

- contrasts:

  A list of contrasts computed for each outcome.

- individual_tables:

  A list of individual tables for each contrast and outcome.

- combined_tables:

  A list of combined tables for each contrast type across all outcomes.

## Details

For very large datasets or models with many time points, parallel
processing may not improve performance as much as expected. This is
because LMTP models can be memory-bound rather than CPU-bound when
working with large data. In such cases, memory pressure and data copying
between workers may offset the benefits of parallelization. Consider
using fewer cores or sequential processing for very large models if you
experience performance degradation.

## Design and execution

\`margot_lmtp()\` executes an LMTP analysis. A study's causal question,
causal estimand, identification assumptions, policy rationale, and
decision rules belong in its protocol rather than this software call.
Keeping those design commitments outside the estimator prevents a later
computational improvement from changing the scientific workflow.

Supplying \`estimator_spec\` locks the execution settings. The \`lmtp\`
call is then built from the specification's \`call_arguments\` — the
exposure at each node, the baseline and time-varying covariates, the
censoring and competing-event indicators, the outcome and its model, the
identifier, the folds, the bounds, the registered learner library, the
analysis-weight column, and the cap — and any conflicting user argument
errors with a condition of class
\`margot_error_estimator_spec_conflict\` that names the conflict. The
specification supplies the whole \`lmtp_defaults\` list, so any entry
supplied alongside it — one the specification fixes, or one the derived
list would drop — raises that condition rather than passing in silence.
Margot re-verifies the specification's content hash on entry, so an
object edited after creation is refused.

## Scheduling modes

The shared density-ratio route (\`reuse_density_ratios = TRUE\`) runs in
one of two modes, selected by \`manage_future_plan\` and the caller's
\`future\` plan.

\*Fold-parallel\* (\`manage_future_plan = FALSE\`) fits each policy and
each outcome in turn, and the caller's plan parallelises the
cross-fitting folds inside \`lmtp\`.

\*Task-parallel\* (\`manage_future_plan = TRUE\`) schedules one density
task for each policy-specific ratio-fit identity and, as each density
task resolves, one outcome task for every terminal outcome. Each task
keeps its own folds sequential. When the current plan is already an
explicit multi-worker plan — \`future::multisession\`, or a
\`future::cluster\` plan spanning several machines — Margot schedules
over it and never alters it. Otherwise Margot opens a local
\`multisession\` pool sized from \`models_in_parallel\` and \`n_cores\`,
and restores the caller's plan exactly on success and on error. That
default local pool counts performance cores alone on Apple Silicon,
since a task that assumes one performance core cannot use an efficiency
core, and an explicit \`models_in_parallel\` overrides the cap. Every
worker reports its R, \`lmtp\`, and \`margot\` versions and a
fingerprint of the shared path's internals before any task is
dispatched; an inconsistent fleet is refused with a condition of class
\`margot_error_worker_ineligible\`. Because nested worker pools remain
deferred, \`cv_workers\` above one is refused with
\`margot_error_nested_parallel_unsupported\`. The coordinator transports
the recorded random-number state to each worker, so a deterministic
fixture reproduces the fold-parallel route bit for bit.

Task-parallel scheduling requires an explicit \`seed\`, and refuses
\`seed = NULL\` with \`margot_error_task_seed_required\`. The sequential
route lets each policy continue the previous policy's random-number
state, which concurrent policies cannot reproduce. Given a seed, the
mode leaves the caller's random-number state at exactly the state
\`set.seed(seed)\` produces, on every exit path and whatever the worker
count, resolution order, or checkpoint availability. That contract
differs from the sequential route, which leaves whatever state its last
fit reached; both routes change the caller's state, and neither
preserves the state at entry. The identity is built from that full state
and from \`RNGkind()\`, so the same integer seed under a different
generator is a different identity.

Shift functions travel to workers inside the task payload, so each shift
must be self-contained: it may read its arguments and its own captured
values, and must not depend on objects that exist only in the caller's
global environment. The realised policy-shifted values enter the task
identity, so a shift whose captured values change receives a new
identity. A shift that draws random numbers is refused with
\`margot_error_stochastic_shift_unsupported\`, because its realised
values would depend on when each task ran; a stochastic policy needs a
registered scheduling-independent design, which this mode does not yet
provide.

With \`save_output = TRUE\`, each policy-specific density result is
written once to an immutable, identity-keyed checkpoint under
\`\<save_path\>/checkpoints/density\`. A later call with the same inputs
reads and verifies that checkpoint instead of refitting; the reuse is
reported in \`ratio_checkpoint_reuse_count\` and does not increase
\`ratio_fit_count\`. A checkpoint carries the density ratios, the
treatment and censoring learner fits, the common fold map, and the
post-density random-number state alone; every terminal-outcome task is
built afresh from the current call's data, so changed outcome values are
always analysed. A corrupt or mismatched checkpoint refuses with
\`margot_error_density_checkpoint_invalid\`, and two distinct stored
results under one identity refuse with
\`margot_error_density_checkpoint_conflict\`, rather than being refitted
over.

The eligibility probe compares R, platform, \`margot\`, \`lmtp\`, and a
fixed set of learner-package versions, together with a fingerprint over
the shared path's \`margot\` and \`lmtp\` internals. It cannot
fingerprint an arbitrary user-registered \`SuperLearner\` wrapper or its
transitive dependencies, so a fleet that registers its own learners must
keep those packages aligned by other means.

## Stage-split execution

The shared route fits one treatment-and-censoring density stage per
policy and then one outcome regression per terminal outcome, so \`K\`
outcomes cost \`G + sum_k Q_k\` rather than \`sum_k (G + Q_k)\`.
\`stages\` lets the two halves run in separate calls, so positivity can
be assessed before any outcome model is fitted.

\`stages = "density"\` runs the coordinator preflight and the density
stage alone, writes the density checkpoints, and returns an object of
class \`margot_lmtp_density_stage\` carrying the per-policy
density-ratio matrices, the task records, the identities and result
fingerprints, and diagnostics from \[margot_lmtp_positivity()\] and
\[margot_lmtp_overlap()\]. Margot supplies the assessment artefacts
alone: no threshold is applied and no pass-or-fail verdict is recorded,
because both belong to the investigator's registered protocol.

\`stages = "outcome"\` fits the outcome stages and requires every
policy's density result to resolve from a verified checkpoint. A policy
without one refuses with \`margot_error_density_checkpoint_required\`
rather than refitting the exposure and censoring models.

## Outcome recovery

With \`save_output = TRUE\`, the task route also writes each fitted
outcome model once, keyed by its outcome-task fingerprint, under
\`\<save_path\>/checkpoints/outcomes\`. A later call whose task
fingerprint matches reuses that model instead of refitting, so an
interrupted run resumes where it stopped; a changed outcome column,
learner, control, or density result yields a different fingerprint and a
fresh fit. The per-run checkpoint directory holds a hard link to the
same single copy, so \[margot_lmtp_restore_checkpoints()\] keeps working
on a run directory without a second copy of every model. Reuse is
reported in \`outcome_checkpoint_reuse_count\` alongside
\`outcome_fit_count\`.

## Thread discipline

Task-parallel scheduling assumes one thread per worker. Cap the native
libraries in the launcher, before R starts, by exporting
\`OMP_NUM_THREADS=1\`, \`OPENBLAS_NUM_THREADS=1\`,
\`VECLIB_MAXIMUM_THREADS=1\`, \`MKL_NUM_THREADS=1\`, and
\`RCPP_PARALLEL_NUM_THREADS=1\`.

Learner wrappers must do the same: \`SL.ranger\` with \`num.threads =
1\` and \`SL.xgboost\` with \`nthread = 1\`. Margot does not administer
machines, so an uncapped learner will oversubscribe the performance
cores that the outer task budget assumes.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume we have a dataset 'my_data' with variables 'outcome', 'treatment', and some confounders

# Define shift functions
gain_function <- function(data, trt) {
  data[[trt]] + 1
}

loss_function <- function(data, trt) {
  pmax(data[[trt]] - 1, 0)
}

# Run LMTP analysis
result <- margot_lmtp(
  data = my_data,
  outcome_vars = c("outcome1", "outcome2"),
  trt = "treatment",
  shift_functions = list(gain = gain_function, loss = loss_function),
  lmtp_defaults = list(
    baseline = c("confounder1", "confounder2"),
    time_vary = c("time_var1", "time_var2"),
    outcome_type = "continuous"
  ),
  save_output = TRUE,
  save_path = here::here("output", "lmtp_results"),
  prefix = "my_study"
)
} # }
```
