# Margot CRAN Submission Refactoring Plan

Companion internal notes:

- `API_REDESIGN.md` for the current workflow-first API redesign
- `DECISIONS.md` for conventions that new refactor work should follow

## Overview
Refactor margot to reduce Imports from 54 to ~15 packages while maintaining all functionality for active users. This will be done gradually and safely.

This refactor now sits inside a broader workflow-first redesign.
The package remains one repo and one namespace for the `1.x` line.
Any future `2.0.0` release should represent a working refactor prototype, not
an aspirational branch.

## Core Principles
1. **No breaking changes** - All current code must continue to work
2. **Gradual implementation** - Small, tested changes
3. **Clear error messages** - Users know exactly what to install
4. **Lab workflow priority** - Teaching and research use cases come first
5. **Batch first** - Batch results are the default, singleton outputs are
   size-one batches
6. **Compatibility tiers** - Older teaching helpers may remain exported without
   defining the main package story

## Immediate `1.x` Stabilisation Work

Before deeper refactor work, stabilise the conservative `1.x` line.

- [ ] Merge the conservative cleanup branch after review
- [ ] Rebuild pkgdown and confirm active workflows still run
- [ ] Tag the next stable `1.x` release from `main`
- [ ] Record compatibility tiers for core, helper, legacy, and
      soft-deprecated functions

## Compatibility Tiers

### Core

Active workflow functions that should remain first-class in `1.x`.

### Helper

Useful support functions that remain available, but are not part of the main
package story.

Current examples:

- `create_ordered_variable()` unless active workflows prove otherwise
- `margot_get_labels()`

### Legacy teaching

Functions retained because they protect older teaching and manuscript code.

Current examples:

- `margot_subset_model()`
- `margot_subset_batch()`

### Soft-deprecated

Functions that remain available for compatibility, but should lose prominence
and should not attract new development.

Current examples:

- `impute_and_combine()`
- `here_read_qs()`
- `here_save_qs()`

## Implementation Strategy

### Phase 1: Fix Immediate CRAN Blockers (Week 1)
- [ ] Add missing packages (future.apply, vctrs) to DESCRIPTION
- [ ] Run R CMD check to verify no other ERRORs
- [ ] Document current functionality with tests

### Phase 2: Identify Core vs Suggested Dependencies (Week 2)
- [ ] Audit all 54 current dependencies
- [ ] Categorise as Core (~15) or Suggested (~40)
- [ ] Map which functions use which packages

### Phase 3: Create Infrastructure (Week 3)
- [ ] Implement check_suggests() helper function
- [ ] Add package detection utilities
- [ ] Set up testing framework for both paths

### Phase 4: Gradual Function Migration (Weeks 4-8)
- [ ] Start with least-used functions
- [ ] Add check_suggests() guards one function at a time
- [ ] Test extensively after each change
- [ ] Maintain a rollback branch

### Phase 5: Documentation and Vignettes (Weeks 9-10)
- [ ] Write or restore only maintained vignettes based on course workflows
- [ ] Update all function documentation
- [ ] Create migration guide for users

### Phase 6: Testing and Validation (Weeks 11-12)
- [ ] Run full test suite with/without suggested packages
- [ ] Test on multiple platforms
- [ ] Validate against course examples

### Phase 7: CRAN Submission Prep (Week 13)
- [ ] Final R CMD check --as-cran
- [ ] Prepare cran-comments.md
- [ ] Submit to win-builder and rhub

## Core Dependencies (Keep in Imports)
Based on the workflow analysis, these must stay in Imports:
```
cli, data.table, dplyr, tidyr, purrr, rlang, 
lifecycle, stats, utils, methods, stringr,
lubridate, here, tools, tibble
```

## Suggested Dependencies (Move to Suggests)
### Estimation Stack
```
grf, lmtp, policytree, maq, SuperLearner,
clarify, EValue, WeightIt, MatchIt, MatchThem
```
Traditional parametric approaches are available through LMTP for comparison
purposes.

### Visualisation Stack
```
ggplot2, patchwork, ggokabeito, ggeffects,
DiagrammeR
```

### Reporting Stack
```
gt, gtsummary, flextable, kableExtra, knitr,
report, parameters, table1
```

### Other
```
mice, miceadds, naniar, zoo, janitor,
cobalt, crayon, doParallel, future,
fastDummies, glue, magrittr, tidyverse,
future.apply, vctrs, furrr, labelled
```

Near-term removal candidates include:

- `ggeffects`
- `tidyverse`

Near-term internalisation targets include:

- `ggokabeito`, once a stable internal `margot` palette is in place

## Helper Function Pattern
```r
# R/utils-suggests.R
check_suggests <- function(pkg, fun = NULL, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    fun_msg <- if (!is.null(fun)) paste0(" for ", fun, "()") else ""
    purpose_msg <- if (!is.null(purpose)) paste0(" (", purpose, ")") else ""
    
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required{fun_msg}{purpose_msg}.",
      "i" = "Install it with: {.code install.packages('{pkg}')}"
    ))
  }
}

# Usage in functions
margot_causal_forest <- function(...) {
  check_suggests("grf", "margot_causal_forest", "causal forest estimation")
  # ... rest of function
}
```

## Vignette Structure (Based on Course Workflow)
1. **Getting Started** - Installation and basic setup
2. **Data Preparation** - Using margot_wide_machine()
3. **Causal Forest Analysis** - Complete estimation workflow
4. **Visualisation** - Plots and interpretation
5. **Reporting** - Tables and summaries
6. **Advanced Topics** - Policy learning, subgroup analysis

## Testing Strategy
1. Create minimal test environment (Imports only)
2. Create full test environment (all packages)
3. Test each function in both environments
4. Automated CI tests for both configurations

## Rollback Plan
- Tag the next reviewed stable release from `main` before deeper behavioural
  refactor work
- Create refactor branch
- Keep main branch unchanged until fully tested
- Ability to revert any change within 24 hours

## Success Metrics
- [ ] All current code continues to work
- [ ] R CMD check passes with <20 Imports
- [ ] Clear error messages guide package installation
- [ ] Course examples run without modification
- [ ] Lab workflows unaffected

## Timeline
- Start: Today
- Target completion: 13 weeks
- CRAN submission: Only after extensive testing

## Notes
- Priority is maintaining functionality for active users
- Each change must be tested in real workflows
- Regular check-ins with lab users
- No rush - quality over speed
