# Workflow Notes

## Purpose

This file records practical lessons from active `margot` workflows that should
inform the refactor. It is for internal development notes, not public package
documentation.

## Current Planning Roles

- `REFACTOR_PLAN.md`: active package refactor roadmap
- `PLANNING.md`: architecture and transformation workstream notes
- `margotverse-plan.md`: long-range package-splitting vision
- `DECISIONS.md`: settled conventions that new code should follow

## LMTP Workflow Notes

### Public function families

For LMTP-facing helpers, the cleanest naming pattern is:

- `margot_plot_lmtp_<topic>()`
- `margot_interpret_lmtp_<topic>()`
- `margot_report_lmtp_<topic>()`

New wrappers should follow this pattern. Existing aliases can remain for
compatibility until the broader refactor is complete.

### Report wrapper shape

Report helpers should stay lightweight and structurally similar. A good default
return contract is:

- `summary_table`
- `plot`
- `narrative`
- `method_statement`
- `metadata`

Add raw data objects only when they materially support downstream reporting.

### Shift labels and ordering

- Keep the internal LMTP shift key as `null`
- Display `null` as `Identity`
- Preserve `null` as the first shift in default ordering

This avoids breaking internals while producing cleaner manuscript text.

### Positivity reporting

- Treat positivity reporting as an overlap or support screen, not a binary test
- Base the support summary on cumulative density ratios among uncensored rows
- Separate censoring burden from treatment-support diagnostics
- Treat ESS as a precision diagnostic, not a positivity verdict

This is more defensible than hard pass/fail logic based on ESS fractions alone.

### Default selection behaviour

When the user omits arguments:

- if `outcome = NULL`, use the first stored outcome
- if `shifts = NULL`, report all available shifts

This should be applied consistently across plotting, interpretation, and report
wrappers.

### Plotting lessons

- Overlap plots benefit from a faint reference line at density ratio `1`
- The `null` histogram needs styling that remains visible under global scaling
- Learner plots read better with left-side component headers than narrow strip
  text at the plot edge
- Remove redundant axis titles when row labels already provide the information

### Internal helper design

Centralise cross-cutting LMTP helpers rather than repeating them across files:

- outcome and shift selection
- shift ordering
- pretty shift labelling
- threshold normalisation

The positivity helper layer added in `margot_positivity_helpers.R` is a useful
pattern to extend.

### Future LMTP optimisation target

The main repeated work in multi-outcome LMTP workflows is treatment-side, not
outcome-side. For a fixed shift policy, the exposure and censoring nuisance
fits, fold structure, and density-ratio construction do not depend on which
outcome is being analysed. By contrast, the outcome regression and TMLE update
remain outcome-specific.

A clean optimisation target is therefore a two-stage LMTP workflow:

1. precompute and save shift-specific treatment objects
2. reuse those fixed shift objects across multiple outcomes

Concretely, stage 1 would build, for each shift:

- fixed cross-fitting fold assignments
- treatment and censoring nuisance fits
- density ratios
- support and censoring diagnostics

Stage 2 would then, for each outcome:

- fit the outcome regression using the same fold assignments
- combine that outcome fit with the saved shift-specific treatment object
- run the targeting and contrast steps

Fixed fold assignments are the key requirement. Without them, the treatment-side
object is not truly reusable across outcomes because each fit would be trained
and evaluated on a different cross-fitting split. That breaks reproducibility
and weakens the statistical coherence of the cached nuisance stage.

This is not a small wrapper tweak. `lmtp` currently exposes `folds` as a count,
but not an obvious public interface for injecting saved fold IDs or prefit
treatment-side objects. So the two-stage idea is best treated as a medium-term
refactor that may require either:

- an upstream extension in `lmtp`, or
- a lower-level `margot` interface that works with `lmtp` internals.

Until then, the smaller practical improvement is to centralise the shared LMTP
configuration and preserve crash-safe per-model checkpoints, while accepting the
current repeated nuisance fitting across outcomes.

### Documentation and build hygiene

- Internal planning notes belong in `planning/`
- Public site content belongs in `docs/`
- Planning and site artefacts should be excluded from the package build
- New exported functions must be added to `_pkgdown.yml` or pkgdown will fail

## Refactor Suggestions

### Short term

- Rationalise LMTP naming around the plot or interpret or report pattern
- Keep compatibility aliases, but stop adding new public names outside that
  scheme
- Consolidate duplicated LMTP labelling and selection logic into helpers

### Medium term

- Separate compute, plot, interpret, and report layers more clearly
- Reduce duplicate wrappers that assemble similar objects in slightly different
  shapes
- Make manuscript mode a first-class pattern for narrative helpers

### Longer term

- Decide whether LMTP functionality should remain inside `margot` or become a
  clearer sub-package boundary in a future `margotverse` split
- Keep workflow notes and decisions stable before moving code across package
  boundaries
