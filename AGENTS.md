# AGENTS.md

## Repository purpose

`margot` is a public R package that implements estimation, sensitivity-analysis, reporting, plotting, storage, and policy-learning functions used by Joseph A. Bulbulia's causal-inference workflows. Changes can alter scientific results, execution contracts, or the interpretation of reported quantities. Review the statistical contract before treating a passing software test as evidence that a change is ready to release.

Follow direct user instructions first, then this file, then the global instructions under `/Users/joseph`.

## Scientific and API discipline

- Preserve the distinction between a causal question, causal estimand, identification assumptions, statistical estimand, estimator, estimate, sensitivity quantity, and presentation layer.
- Treat scale routing, outcome orientation, weighting, trimming, back-transformation, multiplicity, E-value calculation, policy-value evaluation, random-number generation, and thread topology as result-relevant until tests establish otherwise.
- Keep full numerical precision through estimation and derived calculations. Round only for presentation unless a public function explicitly documents a different contract.
- Do not silently change a documented default, fall back to another estimator or policy-tree engine, or reinterpret an existing argument. Record a behavioural correction in tests, documentation, and `NEWS.md`.
- A deterministic or parallel implementation must preserve the serial scientific and reporting objects under the same inputs, seeds, package versions, and machine. Differences confined to timing or execution metadata must be explicit.
- Prefer `fastpolicytree` for supported policy-tree workflows. Margot's validated fast-tree representation uses `strategy.datatype = 1`; do not introduce an automatic engine or representation fallback into a registered workflow.

## Pull requests and releases

Merging a pull request and publishing a package release are distinct actions. A pull request may merge into an unreleased development state. A version becomes a release only after the package, documentation, site, and release metadata form one verified commit.

When a pull request is intended to close as a public release, update these four surfaces together:

1. Increment `Version` in `DESCRIPTION`.
2. Update every applicable editable TOML or other downstream version manifest that consumes the released behaviour.
3. Replace the development heading in `NEWS.md` with the release date and version, and describe every user-visible change and behavioural correction.
4. Rebuild the complete pkgdown site so the displayed version, reference pages, articles, and news agree with the source package.

Several tightly related pull requests may form one release when an explicit release plan treats them as one coherent correction. Otherwise, give each released pull request a new version. Never reuse a released version number or amend the contents identified by a published tag.

Use semantic versioning for new releases:

- increment the patch component for a backward-compatible correction;
- increment the minor component for backward-compatible public functionality or a substantial compatible extension;
- increment the major component for a backward-incompatible public API change.

Margot's historical `1.1.0xx` versions retain their literal names. Use ordinary `MAJOR.MINOR.PATCH` numbering from `1.2.0` onward; do not rename historical versions or create a tag whose spelling differs from the corresponding `DESCRIPTION` version.

## Release verification

Before a release commit:

1. Rebase or merge the current `main` branch into the candidate and resolve stacked-pull-request bases.
2. Run focused tests for the changed scientific and API contracts.
3. Run the complete test suite, build the package, install it into a clean library, and run `R CMD check` or the repository's equivalent complete check.
4. Reject every new error, warning, or note. Identify inherited check findings explicitly rather than presenting them as consequences of the candidate.
5. Regenerate documentation with the repository's documented roxygen procedure.
6. Run `pkgdown::build_site()` for the complete site. Inspect the displayed package version, NEWS page, changed reference pages, affected articles, examples, and links.
7. Inspect the full source and generated-site diff. Generated version-string changes are expected but still require review.
8. Verify that `DESCRIPTION`, `NEWS.md`, generated documentation, site navigation, and applicable version manifests name the same release.

Commit and push the verified release. Create an annotated `vX.Y.Z` tag on the release commit only after the release commit exists on the remote. Push the tag and verify that the remote branch and tag resolve to the intended commit. Never move or replace a published tag; correct a released defect in a later version.

Create a GitHub Release from the same tag when a discoverable release record is useful. Use the matching `NEWS.md` section as the source for release notes.

## Downstream TOMLs and frozen studies

Margot currently has no package-local TOML release manifest. The TOML requirement applies to editable downstream workflows, study templates, and execution manifests that consume a newly released interface. Do not create a TOML merely to satisfy a checklist.

- Update an editable workflow or study TOML only when that workflow requires the new Margot behaviour.
- Never rewrite a deposited registration, frozen study specification, or completed execution record to imply that it used a later Margot version.
- When fitting and later reporting use different Margot versions, record the fitting version and reporting or correction version separately.
- Update a downstream minimum version only after the required Margot release and tag exist and the downstream acceptance test has passed.
- Preserve an earlier study's compatible version and explicit arguments when later Margot defaults change.

## Parallel execution

Parallelism is preferred when it is demonstrably safe; an understandable serial run lasting several hours is acceptable. Retain a tested single-worker implementation as the reference and recovery path.

A parallel release must:

- parallelise independent units without changing the registered within-unit procedure;
- construct and record seeds and partitions independently of scheduling order;
- prevent nested native-thread oversubscription;
- avoid exporting complete multi-outcome forest collections when workers need compact outcome-specific inputs;
- propagate worker failures without silently omitting outcomes;
- restore caller plans, options, environment variables, and random-number state;
- record requested and realised workers and native threads;
- compare serial and parallel scientific objects, reporting objects, and decisions on a realistic acceptance fixture.

Do not merge a performance optimisation merely because it is faster. Release it only after the serial-equivalence and failure-recovery checks pass.

## Documentation and Git

- Use New Zealand English and one source line per prose paragraph.
- Document exported arguments, returns, defaults, fallbacks, scientific scales, and inferential boundaries.
- Keep `NEWS.md` user-facing. State what changed, which results or calls may differ, and how a user can recover earlier behaviour when compatibility requires an opt-out.
- Preserve unrelated work. Stage only reviewed release files.
- Use small, reversible commits with imperative lower-case subjects of at most 72 characters. Never add AI attribution.
- Push every commit. A successful local build, test, site render, merge, or tag is not remote verification.
