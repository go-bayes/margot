# Causal Contrast Marginal Effects Estimation

This function estimates the average treatment effect (ATE) or average
treatment effect on the treated (ATT) using generalized linear models
(GLMs). It supports handling of continuous and categorical treatments,
optional use of spline transformations, and adjustments for multiple
imputation datasets.

## Usage

``` r
causal_contrast_marginal(
  df,
  Y,
  X,
  baseline_vars = "1",
  treat_0,
  treat_1,
  estimand = c("ATE", "ATT"),
  type = c("RR", "RD"),
  nsims = 200,
  cores = parallel::detectCores(),
  family = "gaussian",
  weights = NULL,
  continuous_X = FALSE,
  splines = FALSE,
  vcov = "HC2",
  verbose = FALSE
)
```

## Arguments

- df:

  Data frame containing the data.

- Y:

  The response variable in the data frame.

- X:

  The treatment or exposure variable in the data frame.

- baseline_vars:

  A vector of names of baseline covariates to adjust for in the model.

- treat_0:

  The reference level of the treatment variable, corresponding to no
  treatment or control condition.

- treat_1:

  The active level of the treatment variable, corresponding to receiving
  the treatment.

- estimand:

  A character vector specifying the estimand; "ATE" for Average
  Treatment Effect or "ATT" for Average Treatment Effect on the Treated.

- type:

  A character vector specifying the type of effect size; "RD" for Risk
  Difference or "RR" for Risk Ratio.

- nsims:

  Number of simulations to perform, relevant when handling multiple
  imputation datasets.

- cores:

  Number of cores to use for parallel processing.

- family:

  The family of the GLM to be used (e.g., "gaussian" for linear models).

- weights:

  The name of the weights variable in the data frame, or NULL if no
  weights are to be used.

- continuous_X:

  Logical indicating whether the treatment variable X is continuous.

- splines:

  Logical indicating whether to use spline transformations for the
  treatment variable X.

- vcov:

  The method to use for variance-covariance matrix estimation.

- verbose:

  Logical indicating whether to display detailed output during model
  fitting.

## Value

Depending on the 'type' specified, it returns a summary object
containing either risk differences or risk ratios along with additional
statistics like confidence intervals.

## Examples

``` r
# Assume that df is your dataset with variables 'outcome', 'treatment', 'age', and 'gender'
result <- causal_contrast_marginal(
  df = df, Y = "outcome", X = "treatment",
  baseline_vars = c("age", "gender"),
  treat_0 = "control", treat_1 = "exposed",
  estimand = "ATE", type = "RD", nsims = 100,
  cores = 2, family = "gaussian", weights = "weight_var",
  continuous_X = FALSE, splines = FALSE,
  vcov = "HC3", verbose = TRUE
)
#> Error in causal_contrast_marginal(df = df, Y = "outcome", X = "treatment",     baseline_vars = c("age", "gender"), treat_0 = "control",     treat_1 = "exposed", estimand = "ATE", type = "RD", nsims = 100,     cores = 2, family = "gaussian", weights = "weight_var", continuous_X = FALSE,     splines = FALSE, vcov = "HC3", verbose = TRUE): could not find function "causal_contrast_marginal"
```
