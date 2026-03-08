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
