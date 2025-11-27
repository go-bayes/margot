# Double Robust Marginal Estimation and Tabulation

This function provides a double robust approach for estimating causal
effects. It first computes marginal effects using the
\`causal_contrast_marginal\` function, then processes and tabulates
these results using \`tab_engine_marginal\`. The function handles both
continuous and categorical variables, and allows specification of the
type of effect measure for both causal estimation and tabulation.

## Usage

``` r
double_robust_marginal(
  df,
  Y,
  X,
  baseline_vars,
  treat_0,
  treat_1,
  nsims,
  cores,
  family,
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  vcov = "HC2",
  verbose = FALSE,
  delta = 1,
  sd = 1,
  new_name,
  estimand = c("ATE", "ATT"),
  type_causal = c("RR", "RD"),
  type_tab = c("RR", "RD")
)
```

## Arguments

- df:

  A data frame containing the dataset for analysis.

- Y:

  The name of the outcome variable as a string.

- X:

  The name of the treatment or exposure variable as a string.

- baseline_vars:

  A vector of covariate names included in the model.

- treat_0:

  The reference level of the treatment variable.

- treat_1:

  The treatment level of the treatment variable.

- nsims:

  The number of simulations to run, used in bootstrap or Monte Carlo
  methods.

- cores:

  The number of processor cores to use for parallel computation.

- family:

  A description of the error distribution and link function to be used
  in the model.

- weights:

  The name of the weights variable in the data frame, or NULL if no
  weights are to be used.

- continuous_X:

  Logical, indicating whether the treatment variable X is continuous.

- splines:

  Logical, indicating whether to use spline functions for continuous
  variables.

- vcov:

  The method to use for variance-covariance estimation.

- verbose:

  Logical, indicating whether to print detailed output during
  computation.

- delta:

  The assumed smallest worthwhile effect, used for E-value calculations
  in tabulation.

- sd:

  The standard deviation of the effect estimate, used for E-value
  calculations.

- new_name:

  A new name to assign to the tabulated output, typically describing the
  variable or model.

- estimand:

  Specifies the target of the causal inference, such as "ATE" (Average
  Treatment Effect) or "ATT" (Average Treatment on the Treated).

- type_causal:

  The type of effect size (e.g., "RR" for Risk Ratio or "RD" for Risk
  Difference) to be computed in the causal analysis.

- type_tab:

  The type of effect size to be used in the tabulation of results.

## Value

A list containing two elements: \`causal_results\` with the results from
the causal analysis, and \`tab_results\` with the tabulated results
including E-values and other statistics.

## Examples

``` r
# Assuming you have a dataset `df_ate` and necessary variables defined
results <- double_robust_marginal(
  df = df_ate,
  Y = "t2_kessler_latent_anxiety_z",
  X = "treatment_var",
  baseline_vars = c("age", "gender"),
  treat_1 = "intervention",
  treat_0 = "control",
  nsims = 200,
  cores = 4,
  family = "gaussian",
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  estimand = "ATE",
  type_causal = "RD",
  type_tab = "RD",
  vcov = "HC2",
  new_name = "Test Model Effect",
  delta = 1,
  sd = 1
)
#> Error in double_robust_marginal(df = df_ate, Y = "t2_kessler_latent_anxiety_z",     X = "treatment_var", baseline_vars = c("age", "gender"),     treat_1 = "intervention", treat_0 = "control", nsims = 200,     cores = 4, family = "gaussian", weights = TRUE, continuous_X = FALSE,     splines = FALSE, estimand = "ATE", type_causal = "RD", type_tab = "RD",     vcov = "HC2", new_name = "Test Model Effect", delta = 1,     sd = 1): could not find function "double_robust_marginal"
```
