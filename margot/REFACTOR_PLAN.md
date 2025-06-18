# Margot CRAN Submission Refactoring Plan

## Overview
Refactor margot to reduce Imports from 54 to ~15 packages while maintaining all functionality for active users. This will be done gradually and safely.

## Core Principles
1. **No breaking changes** - All current code must continue to work
2. **Gradual implementation** - Small, tested changes
3. **Clear error messages** - Users know exactly what to install
4. **Lab workflow priority** - Teaching and research use cases come first

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
- [ ] Write vignettes based on course workflows
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

## Helper Function Pattern
```r
# R/utils-suggests.R
check_suggests <- function(pkg, fun = NULL, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    fun_msg <- if (!is.null(fun)) paste0(" for ", fun, "()") else ""
    purpose_msg <- if (!is.null(purpose)) paste0(" (", purpose, ")") else ""
    
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required{fun_msg}{purpose_msg}.",
      "i" = "Install it with: {.code install.packages('{pkg}')}",
      "i" = "For all estimation packages: {.code install.packages('margot.models')}",
      "i" = "For all visualisation packages: {.code install.packages('margot.viz')}",
      "i" = "For all reporting packages: {.code install.packages('margot.report')}"
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
- Tag current version as v1.0.100-stable
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