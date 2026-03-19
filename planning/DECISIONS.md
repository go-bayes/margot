# Decisions

## 2026-03-07

### Planning and build hygiene

- Internal refactor notes live in `planning/`
- These planning artefacts are excluded from the package build via
  `.Rbuildignore`

### LMTP naming

- New LMTP public wrappers should use the canonical pattern
  `margot_plot_lmtp_*()`, `margot_interpret_lmtp_*()`,
  `margot_report_lmtp_*()`
- Existing non-canonical aliases may remain temporarily for compatibility

### Positivity and overlap reporting

- Practical support is assessed using cumulative density ratios among
  uncensored rows
- Censoring burden is reported separately from treatment-support diagnostics
- ESS is treated as a precision diagnostic, not a positivity test

### Display conventions

- Internal shift key remains `null`
- User-facing label for `null` is `Identity`
- Default ordering continues to place `null` or `Identity` first

### Report design

- LMTP report wrappers should return a compact, predictable object with
  summary, narrative, methods, metadata, and optional plot components
- Narrative helpers should support manuscript-ready output without headings

## 2026-03-19

### API redesign frame

- The package will remain one repo, one package, and one user-facing namespace
  for now
- The redesign is workflow-first rather than estimator-first
- Core workflow families are `prepare`, `describe`, `diagnose`, `estimate`,
  `interpret`, `report`, `table`, `plot`, and `store`
- Estimator families such as `grf`, `lmtp`, and `naive` sit within those
  workflow families

### Naming conventions

- Public functions should follow one of two canonical forms:
  `margot_<verb>_<object>()` for cross-domain harmonised outputs, and
  `margot_<verb>_<domain>_<object>()` for method-specific workflows
- Public functions use lowercase snake_case
- Dots are reserved for S3 methods
- Use `hetero` rather than `het` in public names
- Avoid historical implementation-oriented public names such as `from_fit`,
  `dev`, and `wrapper`

### Persistence

- Saving is workflow infrastructure, not estimation semantics
- Core estimators should not save by default
- Checkpointing is allowed for long-running workflows, especially LMTP, but
  should live in a workflow or control layer where feasible
- Canonical store helpers should move toward `margot_save()`,
  `margot_read()`, `margot_save_arrow()`, and `margot_read_arrow()`
- Existing `here_*` helpers may remain as compatibility wrappers during the
  transition

## 2026-03-20

### Release and compatibility

- `main` remains the stable `1.x` line
- `2.0.0` is reserved for the first working prototype of the refactor, not for
  planning work
- The current cleanup and documentation work should be merged before the next
  `1.x` tag
- Tagging should happen only after the conservative cleanup branch is reviewed
  and merged, not from an in-progress redesign branch

### Batch-first design

- Batched outputs are the default design assumption
- Singleton outputs should be treated as size-one batches rather than as a
  separate public API family
- This principle should shrink the public surface, not enlarge it

### Legacy teaching compatibility

- Older teaching-facing functions may remain exported even when they are no
  longer preferred for new work
- `margot_subset_model()` and `margot_subset_batch()` stay available for
  compatibility with teaching material
- `impute_and_combine()` is a soft-deprecation candidate
- `create_ordered_variable()` should be treated as helper-tier unless active
  workflows show otherwise
- `margot_save_png()` remains an active utility
- `margot_get_labels()` remains an active helper for resolving model labels

### Output composition

- Composite policy-tree figures are a legitimate workflow target and should be
  supported explicitly
- Generic batch-of-batch plotting should not drive the package architecture
- `margot_plot_multi()` may remain available, but should not define the future
  plotting API
