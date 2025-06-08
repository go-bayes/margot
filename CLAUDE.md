# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `margot` package provides functions for **MARG**inal **O**bservational **T**reatment-effects. It streamlines causal inference workflows for psychological and social scientists, with a focus on evaluating causal assumptions, modelling time-series data, reporting results, and performing sensitivity analyses.

## Common Development Commands

### Building and Checking
```R
# build and check the package
devtools::check()

# install the package locally
devtools::install()

# build documentation (run after adding/modifying roxygen2 comments)
devtools::document()

# run all tests
devtools::test()

# run a single test file
testthat::test_file("tests/test-margot_wide_impute_baseline.R")

# build pkgdown site
pkgdown::build_site()

# check specific aspects
devtools::check_man()     # check documentation
devtools::check_examples() # check examples run
```

### Linting and Code Quality
```R
# comprehensive package check
devtools::check()

# r cmd check (command line)
R CMD check .

# check for common issues
devtools::spell_check()   # spell check documentation
devtools::check_win_devel() # check on windows development version
```

## Package Architecture

### Core Components

1. **Data Processing Functions** (`R/margot_process_*.R`, `R/margot_wide_*.R`)
   - Handle longitudinal data preparation
   - `margot_process_longitudinal_data()`: Transform to wide format
   - `margot_wide_impute_baseline()`: Baseline imputation
   - `margot_impute_carry_forward()`: Carry forward imputation
   - Panel data management utilities

2. **Causal Inference Models** 
   - `margot_causal_forest()`: Main causal forest implementation (grf-based)
   - `margot_lmtp()`: Modified treatment policy estimation
   - `margot_policy()`: Optimal policy learning
   - Treatment effect engines in `causal_contrast_*.R`

3. **Visualisation System** (`R/margot_plot_*.R`)
   - Unified plotting interface via `margot_plot()`
   - Specialised plots:
     - `margot_plot_qini()`: Qini curves for treatment prioritisation
     - `margot_plot_policy_tree()`: Decision tree visualisation
     - `margot_plot_tau()`: Treatment effect heterogeneity
     - `margot_plot_rate()`: Rate comparisons
   - Consistent theming via `margot_plot_create_options()`

4. **Interpretation and Reporting** (`R/margot_interpret_*.R`)
   - `margot_model_evalue()`: E-value sensitivity analysis
   - `margot_make_tables()`: Comprehensive balance tables
   - `margot_bind_tables()`: Combine results across models
   - Table formatting utilities

5. **Utility Functions**
   - `here_save()`, `here_read()`: File I/O with project paths
   - `back_transform_*()`: Transform variables back to original scale
   - `pretty_number()`: Format numbers for display

### Key Design Patterns

- Functions prefixed with `margot_` are exported user-facing functions
- Internal helper functions are not prefixed (e.g., `helpers.R`)
- Most functions work with data.table internally but accept data.frames
- Parallel processing variants exist for computationally intensive operations (e.g., `margot_causal_forest_parallel()`)
- Consistent use of tidyverse-style programming (dplyr, ggplot2)
- Extensive use of `cli` package for informative user messages
- Input validation at the start of each function
- Soft deprecation approach for API changes (see `R/soft-deprecation/`)

### Important Dependencies

- **grf**: Generalised random forests for causal inference
- **lmtp**: Longitudinal modified treatment policies
- **mice**: Multiple imputation by chained equations
- **policytree**: Policy learning and visualisation
- **maq**: Multi-armed qini curves
- **cli**: User messaging and progress bars
- **ggplot2**: All visualisations
- **data.table**: Internal data processing

### Data Structure

The package expects longitudinal data in wide format with time-prefixed variables:
- Baseline: `t0_variable`
- Follow-ups: `t1_variable`, `t2_variable`, etc.
- ~~The included `df_nz` dataset demonstrates the expected structure~~ We will remove `df_nz` from the package and move it to Zenodo. Data can now be fetched using `fetch_margot_data()` and converted to a quickstart object using `here_read_qs()` function.
- ID variable should be named `id`
- Sample weights: `sample_weights` or time-specific weights

### Development Tools (R/dev/)

Experimental or internal development functions:
- `simulate_ate_data_with_weights.R`: Generate test data
- `run_simulations.R`: Monte Carlo simulations
- `margot_multi_arm_causal_forest.R`: Multi-arm extensions
- `margot_summary_tables.R`: Advanced table generation

## Notes

- The package is licensed under CC BY 4.0
- Version numbering follows semantic versioning
- Documentation is built with pkgdown
- Test coverage is currently minimal - new functions should include tests
- Many functions support batch processing for multiple outcomes/exposures

## Style and Language Guidelines

- Always use new zealand english
- always use lower case code comments, please

## Development Workflow

### Task Management
- **Todo List**: Maintain an ongoing list of tasks to be completed for the margot package
- **Completed Work List**: Track and record completed tasks, milestones, and achievements

### Testing Workflow
- Default to writing unit tests for all functions using testthat, and storing them in the tests folder
- ~~Use the included `df_nz` dataset for testing examples~~ Use `fetch_margot_data()` to get test data
- Test both expected behaviour and edge cases
- Include tests for error messages and warnings

### Contributing Guidelines

1. Follow the `margot_` prefix convention for new exported functions
2. Add comprehensive roxygen2 documentation including `@examples`
3. Use `cli::cli_alert_*()` for user messages
4. Include progress bars for long-running operations using `cli` or `progressr`
5. Write unit tests for new functionality
6. Update NEWS.md with user-facing changes
7. Run `devtools::check()` before submitting changes
```
- lets call the fetch data function 'margot_data_fetch()'