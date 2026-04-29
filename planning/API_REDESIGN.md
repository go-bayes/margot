# API Redesign

## Purpose

This note defines a workflow-first redesign for `margot`.

The redesign should make the public API smaller, more regular, and easier to remember.

The redesign should also stay aligned with the package mission stated in the
README: helping analysts formulate causal questions, examine identification
obstacles, choose estimators, and communicate results clearly.

The package:

- prepares longitudinal and dyadic data
- describes observed study data
- diagnoses support, assumptions, and model behaviour
- estimates causal and naive models, often in batches
- interprets and reports results in tables, prose, and figures
- saves and restores intermediate artefacts

## Core Design Principles

### 1. Workflow first

Top-level organisation should follow analyst tasks, not package history.

The main workflow stages are:

- prepare
- describe
- diagnose
- simulate
- estimate
- interpret
- report
- table
- plot
- store

Estimator families such as `grf`, `lmtp`, and `naive` should sit within this
workflow, and it should remain extensible to estimators we later develop.

### 2. Batch first

`margot` creates a uniform pipeline for causal inference, starting with data
description, wrangling, and checking and moving on to estimation, model
evaluation, interpretation, and reporting.

Public estimators should treat batched model objects as first-class return types.
Single-model workflows should be special cases of the same API.

The default design assumption is therefore a batch object.
Singleton workflows should be represented as size-one batches rather than as a
separate front-door API.

### 3. Small public API, large internal API

The package can have many internal helpers.
It should have a much smaller set of public functions.

Public functions should correspond to repeated analyst tasks.

### 4. Separation of concerns

Each public function should do one main job.

- `simulate` functions generate teaching, testing, or benchmarking data
- `estimate` functions fit models
- `diagnose` functions assess support or model behaviour
- `interpret` functions produce structured prose or summaries
- `table` functions return manuscript-facing tabular outputs
- `plot` functions return figures
- `report` functions assemble multiple outputs into one stable object
- `store` functions write and read artefacts

### 5. Explicit persistence

Saving should not be the default behaviour of estimation functions.

Core estimators should return objects.
Saving should happen in a separate explicit step, or in a higher-level workflow
wrapper where checkpointing is a deliberate feature.

The exception is crash recovery for long-running jobs.
For those cases, checkpoint-aware wrappers are justified.

### 5a. Explicit workflow inputs for generated studies

Generated study scripts should make key workflow dependencies explicit in
configuration rather than relying on hidden assumptions in legacy data exports.

This is especially important for baseline design weights.

For new NZAVS-based generated workflows, the scaffold should:

- read a configured Arrow or Parquet object containing
  `target_nz_population_weights`
- join that weight by respondent and wave before study-specific filtering
- fall back to the legacy baseline weight only where the new target weight is
  missing
- save a provenance audit of which baseline rows used the new versus legacy
  weight
- carry the trimmed baseline design weight forward explicitly as
  `t0_sample_weights` into later attrition or IPCW weighting steps

This keeps generated GRF and related workflows aligned with the current
repository practice in `epic-models`.

### 6. Compatibility through wrappers

Old public names may remain temporarily as wrappers.
Compatibility wrappers should not dictate the new architecture.

The package also needs a compatibility tier for teaching and manuscript
reproduction.
If older exported functions protect real lecture material or published
workflows, they may remain exported even when they are no longer preferred for
new analyses.

## Regret Checks

The redesign should be evaluated against explicit low-regret constraints.

### 1. Do not break older teaching and manuscript code casually

If an older function still protects lectures, manuscripts, or rapid
regeneration of old outputs, the default action is to keep it and lower its
prominence rather than remove it.

This applies in particular to functions such as:

- `margot_subset_model()`
- `margot_subset_batch()`
- `margot_save_png()`
- `margot_get_labels()`

### 2. Do not let compatibility expand the core surface

Compatibility functions may remain, but they should move into helper, legacy,
or teaching tiers.
They should not continue to define the package story or the main reference
page.

### 3. Do not create a second plotting architecture for convenience alone

Composite figures that are natural scientific outputs, such as policy-tree
figures, are worth supporting directly.

Generic batch-of-batch plotting should remain secondary.
If manual composition with `patchwork` is usually clearer for journal work, the
package should not over-invest in a high-level nested plotting API.

### 4. Do not add bulk for the sake of realism

Longitudinal simulation is important, but bundled data should stay small.
For testing and method work, generated fixtures are preferable to large built-in
datasets unless the dataset has a clear teaching role.

### 5. Do not remove useful workflow infrastructure too early

Checkpointing, verbose messages, and progress indicators are part of the
practical value of `margot`.
Refactoring should simplify those systems, not erase them.

This means:

- retain `cli` as the default messaging layer for now
- keep `qs` soft-deprecated until save and restore paths are disentangled
- preserve checkpoint-aware LMTP workflows until a better replacement exists

### 6. Do not split the package before the boundaries are stable

The internal design should be modular enough to split later.
The repo and namespace should remain unified until the new boundaries are stable
enough to justify a separate release story.

## Naming Rules

### Public functions

Use one of two canonical patterns.

For cross-domain functions that operate on a harmonised result structure:

`margot_<verb>_<object>()`

Examples:

- `margot_plot_ate()`
- `margot_table_ate()`
- `margot_report_ate()`

For method-specific functions:

`margot_<verb>_<domain>_<object>()`

Examples:

- `margot_estimate_lmtp()`
- `margot_estimate_grf()`
- `margot_diagnose_lmtp_positivity()`
- `margot_diagnose_grf_overlap()`
- `margot_interpret_grf_hetero()`
- `margot_table_lmtp_results()`
- `margot_plot_grf_qini()`
- `margot_report_lmtp_positivity()`

Where the domain is unnecessary, omit it:

- `margot_save()`
- `margot_read()`
- `margot_save_arrow()`
- `margot_read_arrow()`

### Case and separators

- lowercase snake_case for ordinary functions
- dots only for S3 methods
- avoid mixed styles
- avoid historical names that expose implementation details

### Domain vocabulary

Use a small controlled vocabulary:

- `grf`
- `lmtp`
- `naive`
- `marginal`
- `panel`
- `dyad`
- `amelia`

### Object vocabulary

Use short stable nouns:

- `ate`
- `results`
- `effects`
- `overlap`
- `positivity`
- `support`
- `transition`
- `learners`
- `hetero`
- `rate`
- `qini`
- `policy`
- `scenario`

Use `hetero`, not `het`.

### Naming rule for domains

If the function consumes a genuinely harmonised object structure across GRF,
LMTP, and naive workflows, prefer the cross-domain form without the domain.

If the function depends on method-specific assumptions, diagnostics, or object
structure, include the domain explicitly.

### Terms to avoid in public names

Avoid public names that describe implementation history rather than analyst tasks.

Examples to avoid:

- `from_fit`
- `batch`, unless there is a genuine need for a separate batch-only API
- `dev`
- `wrapper`
- `helper`

## Proposed Module Families

### Prepare

Functions for reshaping, cleaning, and constructing analysis-ready data.

This includes:

- joining explicit baseline design weights from configured external sources
- saving small workflow audits that document weight provenance and eligibility
  decisions

- longitudinal helpers
- dyad workflows
- Amelia transformation helpers
- variable processing and encoding

Examples:

- `margot_prepare_panel_wide()`
- `margot_prepare_panel_encoded()`
- `margot_prepare_dyad_data()`
- `margot_prepare_amelia_long()`

### Describe

Functions for descriptive summaries of the observed data before fitting models.

This includes:

- baseline descriptive tables
- transition tables
- support summaries from observed data
- observed summaries that help anticipate whether later estimators are plausible

Examples:

- `margot_describe_transition()`
- `margot_describe_baseline()`
- `margot_describe_missingness()`

### Diagnose

Functions for support checks, assumption checks, and model-performance diagnostics.

This includes:

- GRF overlap diagnostics
- LMTP positivity diagnostics
- IPSI feasibility summaries based on observed transitions
- learner summaries
- calibration and related model checks

Examples:

- `margot_diagnose_grf_overlap()`
- `margot_diagnose_lmtp_positivity()`
- `margot_diagnose_lmtp_learners()`
- `margot_diagnose_ipsi_support()`

### Estimate

Functions for model fitting.

This includes:

- naive models
- GRF models
- LMTP models

Examples:

- `margot_estimate_naive()`
- `margot_estimate_grf()`
- `margot_estimate_lmtp()`

### Simulate

Functions for teaching, testing, benchmarking, and method development.

This family should support both simple causal examples and, later, more
realistic longitudinal settings with attrition and shift interventions.

Examples:

- `margot_simulate_marginal()`
- `margot_simulate_panel()`
- `margot_simulate_lmtp()`
- `margot_simulate_scenario()`

### Interpret

Functions that turn results into prose-ready summaries or structured narratives.

Examples:

- `margot_interpret_ate()`
- `margot_interpret_grf_hetero()`
- `margot_interpret_lmtp_positivity()`
- `margot_interpret_policy()`

### Report

Functions that assemble stable report objects from lower-level components.

Examples:

- `margot_report_ate()`
- `margot_report_lmtp_positivity()`
- `margot_report_lmtp_results()`
- `margot_report_grf_hetero()`
- `margot_report_grf_policy()`

### Table

Functions that return stable manuscript-facing tables.

Examples:

- `margot_table_transition()`
- `margot_table_ate()`
- `margot_table_lmtp_results()`
- `margot_table_lmtp_positivity()`
- `margot_table_grf_rate()`
- `margot_table_grf_qini()`
- `margot_table_grf_policy()`

### Plot

Functions that return stable figures.

Examples:

- `margot_plot_ate()`
- `margot_plot_transition()`
- `margot_plot_lmtp_positivity()`
- `margot_plot_lmtp_learners()`
- `margot_plot_grf_overlap()`
- `margot_plot_grf_rate()`
- `margot_plot_grf_qini()`
- `margot_plot_grf_policy()`

### Store

Functions for reading and writing workflow artefacts.

Canonical names should be:

- `margot_save()`
- `margot_read()`
- `margot_save_arrow()`
- `margot_read_arrow()`

Existing `here_*` functions can remain temporarily as compatibility wrappers.

## Result Objects

Public estimators and report builders should return stable objects with
predictable components.

At minimum, major result objects should include:

- `results` or `models`
- `diagnostics`
- `tables`
- `plots`
- `metadata`

Batch objects are the default for active workflows.

Simulation functions should also return stable objects with explicit metadata
about the data-generating process, estimands, and intervention rules.

## Persistence Policy

### Canonical rule

Core estimators should not save by default.

### Exception

Long-running LMTP jobs may require checkpointing to avoid catastrophic loss from
 crashes or power failure.

That capability should live in a workflow or control layer, not spread through
the estimator internals.

### Implication

We should distinguish between:

- core estimators
- checkpoint-aware runners or wrappers
- store helpers

## Simulation Scope

Simulation is part of the package mission and should be included in the
redesign plan.

However, simulation should enter the redesigned API in stages.

### Stage 1

Provide a small cross-domain simulation surface for teaching and testing.

The priority target is:

- `margot_simulate_marginal()`

This function should support marginal causal estimands such as `ATE`, `ATT`,
`ATC`, and `ATO` through arguments or metadata rather than through separate
function names.

### Stage 2

Add explicit panel and longitudinal simulation support.

This should include:

- time-varying confounding
- attrition or censoring
- deterministic shifts
- incremental or shift-aware intervention rules

### Stage 3

Assess which ideas from `margot.sim` should be folded into `margot`, kept in a
separate simulation-focused package, or bridged through wrappers.

The redesign should not assume that all of `margot.sim` moves into `margot`.
Instead, `margot` should reserve a clean simulation family and absorb only the
parts that support its core workflow mission.

## Transition Guidance for Existing Functions

### Keep as core or near-core

These functions are close to active workflows and should anchor the redesign.

- `margot_lmtp()`
- `margot_positivity_report()`
- `margot_transition_table()`
- `margot_compute_ipsi_probability()`
- `margot_causal_forest()`
- `margot_assess_overlap()` as a temporary wrapper
- `margot_interpret_heterogeneity()`
- `margot_policy_tree_stability()`
- `margot_policy_workflow()`
- `margot_policy_summary_report()`
- `margot_policy_methods_statement()`
- `here_save()`, `here_read()`, `here_save_arrow()`, `here_read_arrow()` as temporary wrappers

### Convert to clearer canonical names

Examples of the desired direction:

- `margot_lmtp()` -> `margot_estimate_lmtp()`
- `margot_causal_forest()` -> `margot_estimate_grf()`
- `margot_assess_overlap()` -> `margot_diagnose_grf_overlap()`
- `margot_positivity_report()` -> `margot_report_lmtp_positivity()`
- `margot_plot()` -> `margot_plot_ate()`
- `margot_plot_lmtp_overlap_grid()` -> `margot_plot_lmtp_positivity()`
- `here_save()` -> `margot_save()`
- `here_read()` -> `margot_read()`
- `here_save_arrow()` -> `margot_save_arrow()`
- `here_read_arrow()` -> `margot_read_arrow()`

### Push backward or internalise

These functions are either implementation-level or too specific to be central
to the public redesign.

- `margot_lmtp_weight_diag_from_fit()`
- `margot_overlap_bullets()`
- `margot_positivity_helpers()` internals
- duplicated overlap-grid plot wrappers
- dev-era `*_dev` functions

### Ease out

These functions should move to legacy status and then toward deprecation unless
an active workflow demonstrates clear current need.

- `margot_propensity_model_and_plots()`
- low-use specialised plotting variants

## Acceptance Workflows

The redesign should be validated against active workflows and manuscripts rather
than the entire historical surface.

Priority acceptance targets:

- recent GRF workflows in `epic-models/2025`
- recent GRF manuscripts in `epic-pubs/2025`
- recent LMTP manuscripts in `epic-pubs/2026`
- setup-driven `epic-pubs/2026` studies that lean heavily on save/read helpers

## Immediate Next Steps

1. Freeze the naming rules in `planning/DECISIONS.md`.
2. Define the target public API by module family.
3. Identify wrapper names versus canonical names.
4. Move persistence semantics out of core estimation where feasible.
5. Reduce duplicated reporting and plotting surfaces, especially for LMTP positivity.
6. Keep the repo unified until the redesigned boundaries have stabilised.
