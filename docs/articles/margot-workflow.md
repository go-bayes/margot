# Getting Started with Margot

## Overview

This vignette introduces the key components of the margot package for
causal inference with longitudinal data. The package provides tools for
three main stages:

1.  **Data Preparation**: Converting longitudinal data to wide format
2.  **Causal Inference**: Estimating treatment effects using causal
    forests
3.  **Interpretation**: Visualising and understanding results

## Installation

The margot package has a modular design. Install only what you need:

``` r

# core package (data manipulation and basic functions)
install.packages("margot")

# for causal inference models (optional)
install.packages(c("grf", "lmtp", "policytree", "maq"))

# for visualisation (optional)
install.packages(c("ggplot2", "patchwork", "ggokabeito"))

# for reporting tables (optional)
install.packages(c("gt", "gtsummary", "flextable"))

# or install everything at once (future)
# install.packages("margot.models")  # all estimation packages
# install.packages("margot.viz")     # all visualisation packages
# install.packages("margot.report")  # all reporting packages
```

## Stage 1: Data Preparation

### Loading and Exploring Data

``` r

library(margot)
library(dplyr)

# the package includes example data
data(df_nz)

# define variable groups
baseline_vars <- c(
  "male", "age", "eth_cat", "partner", "agreeableness",
  "conscientiousness", "extraversion", "honesty_humility", 
  "openness", "neuroticism", "sample_weights"
)

exposure_var <- "forgiveness"

outcome_vars <- c(
  "alcohol_frequency", "alcohol_intensity", 
  "hours_exercise", "hours_work", "life_satisfaction"
)
```

### Converting to Wide Format

The
[`margot_wide_machine()`](https://go-bayes.github.io/margot/reference/margot_wide_machine.md)
function handles the complete data preparation pipeline:

``` r

# prepare data for causal inference
wide_data <- margot_wide_machine(
  data = df_nz,
  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars
)

# check the structure
str(wide_data)
```

## Stage 2: Causal Inference

### Estimating Treatment Effects

``` r

# run causal forest (requires grf package)
results <- margot_causal_forest(
  data = wide_data,
  exposure = exposure_var,
  outcomes = outcome_vars,
  baseline_vars = baseline_vars,
  weights = "sample_weights"
)
```

### Screening for Heterogeneity

``` r

# identify which outcomes show treatment effect heterogeneity
heterogeneity_results <- margot_rate(results)
```

### Policy Learning

``` r

# learn optimal treatment policies
policy_results <- margot_policy(
  results,
  outcomes = outcome_vars,
  baseline_vars = baseline_vars
)
```

## Stage 3: Interpretation and Visualisation

### Visualising Treatment Effects

``` r

# plot average treatment effects (requires ggplot2)
margot_plot(
  results,
  type = "effects",
  title = "Average Treatment Effects of Forgiveness"
)

# create table output (requires gt)
margot_plot(
  results,
  type = "table",
  format = "publication"
)
```

### Understanding Heterogeneity

``` r

# visualise policy trees
margot_plot_policy_tree(
  policy_results,
  outcome = "hours_exercise"
)

# plot qini curves
margot_plot_qini(
  policy_results,
  outcome = "hours_exercise"
)
```

## Working with Missing Packages

If you haven’t installed optional packages, margot provides helpful
error messages:

``` r

# example: trying to use causal forest without grf installed
# margot_causal_forest(wide_data)
# Error: Package 'grf' is required for margot_causal_forest() (causal forest estimation).
# Install it with: install.packages('grf')
# For all estimation packages: install.packages('margot.models')
```

## Simulating Data for Testing

The
[`margot_simulate()`](https://go-bayes.github.io/margot/reference/margot_simulate.md)
function allows you to generate synthetic longitudinal data with known
treatment effects:

``` r

# simulate data with known treatment effect
sim_data <- margot_simulate(
  n = 500,                    # 500 individuals
  waves = 3,                  # 3 time points
  p_covars = 2,               # 2 time-varying covariates
  exposure_outcome = 0.6,     # true treatment effect
  positivity = "good",        # well-behaved propensity scores
  outcome_type = "continuous",# continuous outcomes
  wide = TRUE,                # return wide format
  seed = 123                  # for reproducibility
)

# simulate with treatment feedback and censoring
complex_sim <- margot_simulate(
  n = 1000,
  waves = 5,
  y_feedback = 0.5,           # past outcome affects future treatment
  covar_feedback = 0.3,       # treatment affects future covariates
  censoring = list(
    rate = 0.2,
    exposure_dependence = TRUE # censoring depends on treatment
  ),
  seed = 456
)
```

## Example Analysis

Here’s a minimal example to get started:

``` r

# load packages
library(margot)
library(dplyr)

# prepare data
data(df_nz)

# define variables
baseline_vars <- c("male", "age", "partner")
exposure_var <- "forgiveness"
outcome_vars <- c("hours_exercise")

# run complete pipeline
wide_data <- margot_wide_machine(
  df_nz, baseline_vars, exposure_var, outcome_vars
)

# estimate effects (requires grf)
if (requireNamespace("grf", quietly = TRUE)) {
  results <- margot_causal_forest(
    wide_data, exposure_var, outcome_vars, baseline_vars
  )
  
  # extract and view the average treatment effect
  ate <- results$ate
  cat("Average Treatment Effect:", round(ate$estimate, 3), "\n")
  cat("95% CI: [", round(ate$ci_lower, 3), ",", round(ate$ci_upper, 3), "]\n")
  
  # visualise if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    margot_plot(results, type = "effects")
  }
} else {
  message("Install the 'grf' package to run causal forest analysis")
}
```

## Further Resources

- Package documentation: <https://go-bayes.github.io/margot/>
- GitHub repository: <https://github.com/go-bayes/margot>
- Course materials: <https://go-bayes.github.io/psych-434-2025/>
