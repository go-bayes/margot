# Compute Causal Contrasts

Estimates causal contrasts using generalized linear models for different
types of treatment effects (ATE, ATT) and outcomes (RR, RD). Supports
handling multiply imputed datasets and allows flexibility in model
specification.

## Usage

``` r
causal_contrast_engine(
  df,
  Y,
  X,
  baseline_vars,
  treat_0,
  treat_1,
  estimand = c("ATE", "ATT"),
  type = c("RR", "RD"),
  nsims = 200,
  cores = parallel::detectCores(),
  family = "gaussian",
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  vcov = "HC2",
  verbose = FALSE
)
```

## Arguments

- df:

  Data frame or \`mids\` object containing the data.

- Y:

  Response variable name as a string.

- X:

  Treatment or exposure variable name as a string.

- baseline_vars:

  Vector of baseline covariate names.

- treat_0:

  Reference level of the treatment variable.

- treat_1:

  Treatment level of interest for comparison.

- estimand:

  Type of causal estimand ("ATE", "ATT"); defaults to both.

- type:

  Type of effect size ("RR" for Risk Ratio, "RD" for Risk Difference);
  defaults to both.

- nsims:

  Number of simulations for bootstrap; defaults to 200.

- cores:

  Number of cores for parallel processing; uses all available cores by
  default.

- family:

  Model family as a string or family object; defaults to "gaussian".

- weights:

  The name of the weights variable in the data frame, or NULL if no
  weights are to be used.

- continuous_X:

  Whether X is a continuous variable; defaults to FALSE.

- splines:

  Whether to apply spline transformation to X; defaults to FALSE.

- vcov:

  Type of variance-covariance matrix for standard error estimation;
  defaults to "HC2".

- verbose:

  Whether to print detailed output; defaults to FALSE.

## Value

Depending on the configuration, returns a summary object containing
estimated causal contrasts, confidence intervals, and potentially other
diagnostics.
