# Proposal: margot.grf primitives

Status: DRAFT for Joseph's review

Date: 2026-07-30

Purpose: design the object model of `margot.grf` — the workflow-contract companion to `margot.lmtp` for NZAVS-GRF-v1 — before any code exists, against Joseph's stated criteria: clarity and long-term maintainability. Grounded in a full map of `margot.lmtp`'s actual primitives (2026-07-30) and the NZAVS-GRF-v1 architecture record (`arc/planning-decisions/2026-07-30-grf-workflow-architecture.md`). This document will move into the `margot.grf` repository when it is created.

## 1. What margot.lmtp actually provides, in one paragraph

Every sealed object is made by one internal constructor (`new_seal()`) that wraps a declarative payload in a fixed envelope: object type, schema version, named parent content-hashes, a content hash (schema + type + parent hashes + payload, canonically serialised), an event hash (content hash + parent event hashes + actor + timestamp), actor, timestamp, package versions. Objects append to a single-writer ledger directory — one JSONL line per sealing event with a previous-line hash chain, one content-addressed `.rds` per object. Bulky computed material travels beside the payload, outside the hashed content, covered by fingerprints that are inside the payload. A controlled vocabulary of report quantities is the join target for precommitted expectations; a pairing key built by one shared function places realised values beside expectation rows; the investigators declare every departure — no function computes a verdict, and a forbidden-token assertion (`status`, `pass`, `fail`, `tolerance`, `verdict`, `gate`, ...) runs on every payload and is enforced by tests over every export name and argument. Estimation takes the deposit, the data, and the policy closures and nothing else; everything else is read back from the ledger.

This is the right foundation. The design work for margot.grf is (a) which parts to share rather than copy, (b) the GRF object chain, (c) the GRF controlled vocabulary and expectations ledger, and (d) which warts in margot.lmtp to fix rather than inherit.

## 2. Ruling sought first: extract the sealing core into `margot.seal`

The sealing core — `seal.R`, `canonical.R`, `ledger.R`, `verify.R`, `extra_integrity.R`, `errors.R`, the `check_*` helpers, `lineage.R`, the seal print methods — contains nothing LMTP-specific beyond naming. Copying it into margot.grf creates a second divergent implementation of the most safety-critical code in the programme: the exact organic-growth regret this planning exercise exists to avoid. Two contract packages are already planned; a third estimator family would make three copies.

**Proposal.** A small package `margot.seal` (name states its one job) holding the estimator-agnostic core:

- `new_seal()` generalised: class vector `c(paste0(domain_prefix, object_type), "margot_seal", "list")`, with the domain prefix (`margot_lmtp_`, `margot_grf_`) supplied by the domain package at ledger creation and recorded in the ledger.
- Canonicalisation, both hashes, fingerprints (`fingerprint_function`, `fingerprint_text`, `fingerprint_data`), the `extra` mechanism, ledger append/read/verify, lineage, `margot_abort()` with the class-prefix discipline, the `check_*`/`assert_*` helpers, `print.margot_seal`.
- The forbidden-token doctrine lives here, as doctrine: the token list, `assert_no_judgement_fields()`, and an exported test helper the domain packages run over their own exports (margot.lmtp currently enforces this in its own test file; the helper makes the enforcement uniform).

Timing argument: margot.lmtp is at 0.0.0.9000, unreleased, with golden-hash tests. Refactoring it onto margot.seal now is the cheapest it will ever be, and the golden hashes are the safety net — content hashes exclude package-version fields by design, so a pure relocation that preserves schema-version strings and canonical serialisation must reproduce them bit-for-bit. If any golden hash moves, the refactor is wrong.

Alternative if Joseph declines: margot.grf vendors the core files verbatim with a recorded upstream commit, and a sync script diffs them in CI. Workable, but it is planned divergence.

## 3. Ruling sought second: margot.grf calls grf directly, and owns its registered displays

margot.lmtp does not import margot. It owns its design stage, calls its instrument (`lmtp`) through one pinned seam (`fit_lmtp_sdr()`, exactly version 1.5.4, conformity fixture required before any pin bump), and owns its reporting: report tables read from the sealed estimation record, narration comes from the boilerplate database, print methods interleave them.

**Proposal.** margot.grf follows the same doctrine:

- **Instrument seams**: `grf::causal_forest` / `grf::average_treatment_effect` / `grf::best_linear_projection` behind one seam; `policytree` (and `fastpolicytree`) behind a second. Versions pinned; a conformity fixture per seam (small fixed dataset, named properties with admitted differences) run before any pin bump.
- **margot is not a dependency.** The registered workflow must not depend on the monolith whose unplanned growth motivated this design. margot remains the exploratory and legacy engine.
- **Registered displays live in margot.grf**: the BLP forest plot, the policy-tree panel, and the binned conditional-effect display, each built once under the caption contract (mandatory sentences from boilerplate, effective-N annotation, no suppression except by explicit argument). This is a small, closed set — three displays — not a plotting library. margot's rich plotting stays available for exploration and is simply not part of the registered path.

**Consequence for the alignment plan** (`margot/planning/2026-07-30-grf-v1-reporting-alignment.md`, currently under three-model review): most of its registered-standard surface migrates here. What survives in margot is legacy hygiene — remove the undocumented borderline threshold, fix the hardcoded titles, soft-deprecate the detection prose, keep historical scripts running. The revised plan after the reviews will restate this split explicitly.

## 4. The margot.grf object chain

Mirrors the margot.lmtp stage graph, adapted to the eight steps of the architecture record. Point treatment makes the chain shorter than LMTP's (no density-ratio stage; the outcome-blind analysis is overlap, not sequential ratios).

```
question_registry                       questions, admissibility, priority, terminal-outcome boundaries
  └─ question_spec                      exposure coding rule + cutpoint, waves, target population,
                                        eligibility, weighting, contrast, POSTURE DECLARATION
       ├─ causal_estimand_spec          the two counterfactual means, scale
       │    └─ identification_spec      consistency, exchangeability, overlap-form positivity,
       │         │                      censoring-exchangeability (two IPCW stages)
       │         ├─ expectations_spec   BOTH expectation classes (see §5)
       │         └─ identification_judgement
       └─ data_snapshot                 design data, id column, outcome-leak audit,
                                        outcome-direction table (flips verified mechanically)
                     ↓
overlap_manifest                        parents: {expectations_spec, data_snapshot};
  │                                     the frozen outcome-blind table set (2026-07-06 standard),
  │                                     propensity bounds, weights; analysis code fingerprinted
  └─ overlap_report                     realised tables sealed; design-stage expectations PAIRED here
       └─ question_decision             retain / revise-within-registered-alternatives / stop;
            │                           departures declared by investigators, never computed
            └─ statistical_estimand_spec  AIPW ATE; posture per layer restated from lineage
                 └─ estimator_spec      grf settings (seed 42, 5000 trees, min node 50, honesty),
                      │                 BLP spec (covariate-set hash, target_sample),
                      │                 policy-tree protocol (depths, folds, repeats, screen rule),
                      │                 inferential-arm spec or explicit "none"
                      └─ registration_manifest → registration_deposit
                           └─ estimation_record   ATE + BLP + tree results, sealed
                                └─ heterogeneity_report   substantive expectations PAIRED here;
                                                          the expectations ledger (§5)
```

Wart repairs relative to margot.lmtp, applied from the start:

1. **Declared reads.** margot.lmtp constructors reach arbitrarily far up the lineage via `ancestor_seal()`, so the parent list understates real dependencies. margot.grf constructors declare `reads = c(...)` beside `parents`, sealed into the payload, so the dependency graph is honest.
2. **One meaning per word.** "Registry" names only the question registry. Fingerprint collections are "fingerprints".
3. **Predicate and sealer separated.** Admissibility checking is a pure predicate; sealing a revision is a separate constructor.
4. **Shared validators.** The seed/folds/settings checks duplicated across margot.lmtp constructors become shared `check_*` helpers in margot.seal.
5. **Version-specific rules live in a profile, not the machinery.** margot.lmtp hardcodes NZAVS-LMTP-v1 rules (difference-scale-only, `at_risk` rejection) inside general constructors. margot.grf carries an explicit `workflow_profile` object — NZAVS-GRF-v1's pinned values (seed 42, forest settings, depths, screen rule, binary-point-exposure scope) — that constructors validate against. Version 2 becomes a new profile, not a code edit.
6. **Single-writer made real.** A lock file on ledger append, since the delegation model runs parallel workers.

## 5. Expectations, the two classes, and the ledger

The record requires two expectation classes with different powers. They are one `expectations_spec` with a `class` column, because they share the pairing machinery, but they pair at different stages and only one can precede a question decision:

- `class = "design"` — targets quantities in the **overlap vocabulary** (propensity-bound mass, weight concentration, ESS shares, alignment-table quantities). Paired in `overlap_report`; feed `question_decision`. Prompted responses: retain-with-reasons / revise / stop, as in margot.lmtp.
- `class = "substantive"` — the effect-modification tuples: covariate (validated present in the registered covariate set at spec time, via the covariate-set hash carried from `estimator_spec`'s registered fields — spec-time validation is the reason the covariate set is registered early), expected direction, outcome family, the three examined outputs, rationale, and the departure prompt. Paired in `heterogeneity_report`; can never trigger the question decision.

**The GRF controlled vocabulary** (the join target, mirroring the 57-quantity LMTP table): identifiers over five sections — `average_effect.*` (estimate, interval, E-value); `projection.*` per (covariate, outcome): estimate, interval, sign; `tree.*` per outcome: splitter set, split thresholds, leaf contrasts, policy value against each baseline, coverage, stability quantities, `uniform_selected_action`; `conditional_display.*` per (covariate, outcome): binned direction, subgroup contrast estimate and interval; `resolution.*`: raw N, effective N by weight source, interval widths. The inferential arm, when registered, adds `prioritisation.*` (gain estimate, lower bound, upper bound, ATE-relative upper bound) — present in the vocabulary only when the arm is in the contract.

**The expectations ledger is the paired frame plus the investigators' declaration.** Exactly as in margot.lmtp's `question_decision`: the software joins realised values to expectation rows by the pairing key and enforces completeness (every expectation paired, every pairing carrying a realised value, every expectation carrying a declared verdict, every departure carrying a response). The verdict column takes the closed vocabulary — consistent / mixed / not consistent — and is **authored, never computed**. Between the realised values and the authored verdict sits one computed layer, recorded **as data, not as a rule**: a per-output evidence state — supports / opposes / uninformative — with "uninformative" covering an interval spanning zero, a splitter absent, a covariate outside the split-search screen, or overlapping ribbons in the binned display. Screen membership is its own recorded state ("not in the split-search screen"), because the tree search runs over the variable-importance screen, not the forest covariate set — a covariate the search never saw must not read as "the tree declined to split on it" (Opus review, m22). Tree absence therefore never "votes"; the record's own sentence — absence is not causal irrelevance — belongs in the narration. A resolution column beside the verdict carries the effective N and the widest interval, so an all-uninformative case is honestly undetermined without adding a fourth verdict term. This settles the mechanical-verdict-mapping question the three alignment-plan reviewers converged on from different directions: evidence states preserved as sealed data, judgement reserved to the authors.

Two naming repairs from review: "ledger" already denotes the seal chain in margot.lmtp (Opus m18), so in code this artefact is the **expectations record** (`margot_grf_expectations_record()`), while registration prose may keep the architecture record's phrase "expectations ledger" — the record's vocabulary governs prose, the package namespace governs code. And per-output evidence functions carry the review-derived seam constraints of the revised alignment plan §3 (BLP failure isolation, aliased-column disclosure, cluster-robust standard-error naming, per-display effective N, between-stratum doubly robust contrasts, the outcome manifest, and report-time narration — never fit-time).

**Posture** is a payload field of `question_spec` (declared at Step 1, per the record), restated in `statistical_estimand_spec`, and read from lineage by every reporting function. No call-site posture arguments anywhere; a caller cannot substitute a posture the registration did not fix — the same four-arguments-and-no-more doctrine as `margot_lmtp_estimate()`. Narration under the descriptive posture never renders detection language because the boilerplate entries are keyed by posture, not because functions branch on flags.

## 6. Naming, errors, conventions

- Exports: `margot_grf_*`. Sealed classes: `margot_grf_<object_type>` + `margot_seal`. Result classes: `margot_grf_<noun>`.
- `errors.R` pattern adopted wholesale via margot.seal: classed conditions, `margot_error_*` prefix enforced, the condition catalogue as the documented contract, tests dispatch on class never on message text.
- The forbidden-token test suite runs over margot.grf's exports from day one, extended with the GRF-specific banned registration vocabulary where it names judgements (`significant`, `detected`, `confirmed`, `reliable`, `negligible` as field or argument tokens).
- Golden-hash tests from the first sealed object type; example contract + example workflow as the executable specification, mirroring `margot_lmtp_example_contract()`.
- No vignette until the API settles; the README is the on-ramp (margot.lmtp's stated pattern).

## 7. What margot keeps (revised scope of the alignment plan)

- Legacy hygiene, shipped soon and small: delete the undocumented `borderline_threshold`; add the missing `title`/`subtitle` overrides; correct "individual treatment effects" labels; soft-deprecate the detection-language interpret family with pointers to margot.grf; **fix the seed defect** (seed argument not passed to `grf::causal_forest`) since exploratory work also deserves reproducibility.
- margot remains fully supported for exploration, teaching, and the two historical registered studies, which never migrate.
- New arc GRF studies, from `jb-grf-workflow` onward, run the registered path through margot.grf.

## 8. Open questions for Joseph

1. **Extract `margot.seal` now** (recommended, §2) or vendor the core into margot.grf with a sync script?
2. **Licensing — RULED (Joseph, 2026-07-30): MIT for both contract packages.** margot.lmtp relicensed MIT (DESCRIPTION + LICENSE committed 2026-07-30); margot.grf is MIT from its first commit; margot stays CC BY 4.0. The MIT licence governs our own code; the GPL-3 of grf/policytree and the AGPL-3 of lmtp govern use of those instruments and are unaffected by our package licence — dependency declarations, not derivation.
3. **Does margot.grf own the overlap engine** (propensity fitting for the outcome-blind stage, as margot.lmtp owns its ratio engine) or call the pinned grf propensity forest through the seam? Recommended: through the seam — unlike LMTP's ratio engine there is no methodological gap to close, and an owned engine is maintenance without a rationale.
4. **Scope of the first release**: the full chain through `heterogeneity_report`, or stop at `registration_deposit` (matching where margot.lmtp's maturity is) and add the estimation/report stages in a second work package once the jb-grf-workflow registration drafting exercises the contract stages?
5. **The inferential arm's power simulation** (Opus review, M9): the record binds an opting-in study to a full-pipeline power simulation before freeze, and no code anywhere can run one. Deliver `margot_grf_rate_power_sim()` inside the arm module (recommended — a registration must not promise a procedure no package implements), or keep the simulation a study-side script and have the registration cite a named reference implementation?
