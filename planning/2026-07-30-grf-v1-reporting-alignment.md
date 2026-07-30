# Plan v2: margot under NZAVS-GRF-v1 — legacy hygiene, with the registered surface moved to margot.grf

Status: REVISED after three-model review (Fable, Opus 5 xhigh, GPT-5.6 Sol xhigh) and the margot.grf adoption ruling. Supersedes v1 of this document (2026-07-30, same day).

Authority: `arc/planning-decisions/2026-07-30-grf-workflow-architecture.md`; the margot.grf companion-package ruling (2026-07-30); the three reviews (Fable inline; Opus in the session workflow record; GPT at the session scratchpad `codex-review-grf-plan.md`); the margot.lmtp primitives map (2026-07-30).

## What changed from v1

Joseph adopted `margot.grf` as the workflow-contract companion to `margot.lmtp`, with `margot` remaining the exploratory and legacy engine. margot.lmtp's precedent (mapped 2026-07-30) is decisive: it imports nothing from margot, owns its reporting, and calls its instrument through one pinned seam. margot.grf follows the same doctrine (see `2026-07-30-margot-grf-primitives.md`). Consequently v1's attempt to make margot itself GRF-v1-compliant is withdrawn: **margot is no longer the registered path for new grf studies**, and its plan shrinks to legacy hygiene. The registered-standard surface — BLP, graph-all reporting, captions, expectations, effective N, the inferential arm — is specified as design constraints on margot.grf instead (§3).

Both external reviewers judged v1 not implementation-ready (four blockers each, overlapping). Every finding is dispositioned in §4; none is dropped silently.

## §1. margot legacy-hygiene plan (small, one minor release)

Scope principle: fix what is wrong for *any* user (defects, undocumented thresholds, deployment prose); do not thread posture, captions, or graph-all through margot — that machinery now lives in margot.grf. margot gains no `posture` argument anywhere.

1. **Seed defect** (GPT 10): `margot_causal_forest()` defaults to seed 12345 and does not pass `seed` to `grf::causal_forest()` at all (`R/margot_causal_forest.R:416`, `:658`). Fix the pass-through and document that honesty rides on the grf default. Historical studies' saved objects are unaffected; future executions become reproducible — record as an execution-provenance note in `NEWS.md`.
2. **Deployment prose off by default** (Opus B1): expose `auto_recommend` on `margot_policy_workflow()` (currently hardcoded `TRUE` at `:299`, `:371`) with default `FALSE`; retire the `"recommended"` preset; soft-deprecate the `recommended_*` and `recommendations_text` returns. The "Recommend deploying full policy" / "Do not deploy" headings (`margot_policy_summary_report.R:1236–1245`) and the deployment counts in `margot_policy_summary_compare_depths.R:355` and `margot_policy_methods_builder.R:63–98` become opt-in.
3. **Borderline category** (Opus M4 supersedes v1): keep the classification computed for legacy consumers; make `borderline_threshold` an explicit argument defaulting to `NULL` (no borderline class formed); document the retired `0.01` constant in `NEWS.md`. Do not remove documented returns. The grouping switch is the existing `group_by_sign` (Opus M5 — v1's `group_by_evidence` named an argument that does not exist).
4. **`margot_select_grf_policy_trees()`** (Opus m19 supersedes v1): keep its defaults (flipping them makes the function a no-op and breaks its tests); add a one-time deprecation warning naming it inferential-arm-only, and update `test-margot_select_grf_policy_trees.R` to assert the warning.
5. **Negligibility and detection prose, worst offenders only**: delete "relatively homogeneous … a uniform treatment policy may be appropriate" and "good candidates for personalized treatment strategies" from `margot_rate_cv.R:1180–1190`; delete "No outcomes demonstrate statistically reliable policy value gains" at `margot_policy_summary_report.R:1727`; default `highlight_significant = FALSE` in `margot_rate.R` (Opus M8 — bolding by CI exclusion is a star by another name). `margot_interpret_heterogeneity()` is soft-deprecated whole, with a pointer to margot.grf — its SELECTED/EXCLUDED/PERMITTED output is inferential-arm machinery and margot no longer carries the arm's registered narration (resolves v1 open item 3; Opus and GPT both rejected the summary-under-same-name option).
6. **Label and title corrections**: rename "individual treatment effects" to estimated conditional effects in `margot_plot_tau` docs and default label (GPT 9); add `title`/`subtitle` arguments to `margot_plot_qini()` and the other hardcoded-title sites in Opus m16's inventory (`margot_plot_qini_direct.R:206`, `margot_plot_policy_tree.R:505,547`, `margot_plot_cv_results.R:298`, `helpers.R:1533`); `margot_plot_qini_batch()` gains explicit forwarding — `margot_plot_rate_batch()` is out of this change (GPT 13).
7. **`blp_top`**: deprecated-for-reporting note plus a wrapper print that drops p-values and the significance-star legend (Opus m12). No new BLP computation in margot — that is margot.grf's seam.
8. **Regression targets rewritten** (Opus M10): the two historical studies call `margot_causal_forest`, `margot_exposure_overlap`, `margot_rate_cv`, `margot_policy_tree_cv`, `margot_model_evalue`, `margot_plot_decision_tree`, `margot_grf_overlap` — not `margot_policy_workflow` or the summary report. The regression suite therefore covers exactly those calls with saved structural fixtures; `margot_rate_cv` prose is unchanged by this plan (no posture argument), and `margot_plot_decision_tree` gains no caption (caption contract lives in margot.grf), so the church-coop manuscript figure is untouched. "Identical numerical output" applies to numerical fields, not object shape.
9. **Versioning**: one minor release carrying all of the above; `NEWS.md` names every default change with the old value; no global compat option (all three reviewers concur). Existing tests asserting the old defaults (`test-margot_policy_workflow.R:51` among the 31 test files) are updated in the same release.

Explicitly not done in margot: graph-all mechanics, outcome manifests, caption/caveat layers, effective-N annotations, expectations machinery, posture threading, the inferential-arm report contract, boilerplate migration of the remaining interpret prose. Those are margot.grf's (§3). margot's remaining detection-language functions are documented as legacy-exploratory, excluded from any registered workflow by the margot.grf registration contract rather than rewritten.

## §2. Delegation for §1

Single worker (Opus 5 or GPT-5.6 Sol per availability), one branch, Fable review. The nine items are independent apart from 2–3 (same files); sequence 1 → 5 → 2 → 3 → 4 → 6 → 7 → 8 → 9. Estimated as one focused work package; no parallel split needed at this size (v1's two-worker split and its circular dependencies — Opus m21, GPT 12 — are dissolved by the migration).

## §3. Review-derived design constraints on margot.grf

These bind the margot.grf build (object model in `2026-07-30-margot-grf-primitives.md`). Sources in parentheses.

**BLP seam** (GPT 4; Opus B2, m11, m17; Fable):
- Projection on the same sample as the ATE; split-sample fits either project on the ATE sample or refuse standard reporting — the current whole-forest-sample `blp_top` mismatch must not be reproduced.
- The seam isolates failure: a projection error is a structured failure row, never the loss of the outcome's other results (margot's whole-outcome `tryCatch` at `margot_causal_forest.R:945` is the anti-pattern).
- Aliased/rank-deficient columns are detected and disclosed in the table note — grf's projection silently drops aliased covariates, which would otherwise corrupt the evidentiary state of an expectation covariate.
- Standard reports reject projection-set hashes differing across outcomes or from the registered set; any non-registered set or `target.sample = "overlap"` is labelled a sensitivity in the tidy frame and every note. Intercept excluded from the moderator grid.
- Standard errors named for what they are (cluster-robust, grf's `vcovCL`; store the cluster variable or `"none"`); the grid-disclosure sentence qualified "approximately, under independence".
- Recomputation at a different `target.sample` requires retained forest objects; absence errors with guidance at call time.

**Registered reporting** (Opus B3, M6, M7; GPT 5, 6):
- A registered outcome manifest drives every report; coverage is asserted — a failed outcome is a failure row, never a silent omission. No result-derived ordering anywhere; ordering comes from the registration.
- Narration is generated at report time from sealed records, never stored on fitted objects (margot's fit-time narration in `margot_rate_cv` is the anti-pattern that made call-site posture unworkable).
- Report builders receive the objects they narrate (leaf metrics, `uniform_selected_action`), so mandatory sentences are wired to computed facts, not to caller diligence.
- Narration comes exclusively from the boilerplate database; a missing entry is an error naming the entry, not a hardcoded fallback (GPT 1; margot.lmtp's degrade-by-naming pattern).

**Vocabulary and expectations** (Opus m22, open item 2; GPT 7; Fable; margot.lmtp doctrine):
- The tree section of the controlled vocabulary records split-search screen membership per covariate; "not in the split-search screen" is a distinct realised state from "splitter absent", because for margot-built trees the split search runs over the top-15 screen, not the forest covariate set.
- Per-output evidence states are computed **as data** — supports / opposes / uninformative (interval spanning zero; splitter absent; outside the screen; overlapping ribbons) — and sealed beside the expectation. The verdict — consistent / mixed / not consistent — is authored by the investigators, never computed (margot.lmtp doctrine; settles the GPT/Opus truth-table proposals by moving them from code to recorded evidence). A resolution column carries effective N and the widest interval so an all-uninformative case is honestly undetermined without a fourth verdict term.
- Between-stratum contrasts for registered subgroups are computed from aligned doubly robust scores with an estimate and interval (GPT 3 — margot's subset machinery never estimates the contrast; margot.grf's conditional-display engine must).

**Resolution disclosure** (GPT 8; Opus m13–m15; Fable):
- Effective N is per display and per weight source: analysis sample, overlap-adjusted (where grf multiplies by $\hat{e}(1-\hat{e})$), bin or stratum, held-out evaluation slice; reported beside raw N with the weight source named. Kish N is described as weight concentration; CI width remains the primary resolution statement. Scalars (`sum(w)`, `sum(w^2)`) stored at fit time so displays never depend on optional saved data.

**Inferential arm** (GPT 2; Opus M9):
- A structured arm report contract: registered outcomes in, weighting roles recorded, five sequential folds enforced, Bonferroni within each weighting (margot's `margot_rate_cv` corrects across everything supplied at once — the cc study hand-builds the correct families today), absolute and ATE-relative bounds out, power-simulation provenance and the redesign-or-demote record attached.
- The full-pipeline power simulation is a named deliverable (`margot_grf_rate_power_sim()` or an explicitly cited study-side reference implementation) — a registration must not promise a procedure no code can run. Decision deferred to the margot.grf first-release scoping (primitives doc §8.4).

**Arc-side patch** (Opus M10): none required now — the cc study's calls are untouched by §1, and caption changes no longer reach `margot_plot_decision_tree`. If the cc study later adopts margot.grf reporting, that is a prospective departure through its versioning process (already noted in the architecture record).

## §4. Finding disposition index

| Finding | Disposition |
|---|---|
| GPT 1 (fallback prose) | margot.grf constraint (§3 reporting); margot keeps mirrored fallbacks as legacy |
| GPT 2 (arm structure) | margot.grf §3 inferential arm |
| GPT 3 (subgroup contrasts) | margot.grf §3 vocabulary/expectations |
| GPT 4 / Opus B2, m11, m17 (BLP) | margot.grf §3 BLP seam |
| GPT 5 / Opus B1 (deployment) | margot §1.2 |
| GPT 6 / Opus B3 (graph-all leaks) | Registered path: margot.grf manifest (§3); margot: not pursued (no longer registered path) |
| GPT 7 / Opus open-2 / Fable (verdict mapping) | Superseded by authored-verdict doctrine; evidence states as data (§3) |
| GPT 8 / Opus m13–m15 / Fable (effective N) | margot.grf §3 resolution |
| GPT 9 (closing clause, tau label) | Clause: margot.grf captions; label: margot §1.6 |
| GPT 10 (seed) | margot §1.1; margot.grf estimator_spec pins |
| GPT 11 / Opus m20 (deprecation, tests) | margot §1.9 |
| GPT 12 / Opus m21 (sequencing circularity) | Dissolved by migration (§2) |
| GPT 13 / Opus m16 (titles) | margot §1.6 |
| Opus M4, M5 (borderline, group_by_sign) | margot §1.3 |
| Opus M6 (fit-time narration) | margot.grf principle (§3 reporting); margot unchanged |
| Opus M7 (text fn has no object) | margot.grf report builders (§3); margot skips |
| Opus M8 (five more prose files, highlight) | Worst offenders margot §1.5; rest documented legacy |
| Opus M9 (power sim) | margot.grf §3; scoping open item |
| Opus M10 (regression premise) | margot §1.8 |
| Opus m12 (blp_top stars) | margot §1.7 |
| Opus m18 (ledger name collision) | Primitives doc open question (naming) |
| Opus m19 (selector no-op) | margot §1.4 |
| Opus m22 (screen confound) | margot.grf §3 vocabulary |
| Fable S3 sweep, recompute dependency, grid qualifier, leaf-level N | margot.grf §3 (all four folded) |
| Open item 1 (posture) | Dissolved: posture is sealed in the margot.grf contract, read from lineage; margot carries none |
| Open item 3 (interpret_heterogeneity) | margot §1.5 (deprecate whole) |
| Open item 4 (versioning) | margot §1.9 (minor bump, no compat option — unanimous) |

## §5. Verification

- §1 ships behind the updated test suite plus the rewritten regression fixtures of §1.8; grep-based assertions confirm the deleted prose strings are gone and no new `posture` argument exists in margot.
- §3 constraints are acceptance criteria in the margot.grf build plan; each names its reviewer finding so the eventual implementation review can tick them off.
