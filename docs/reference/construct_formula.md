# Construct a Formula for Regression Models

This function dynamically constructs a regression formula based on
provided parameters. It supports incorporating splines for continuous
variables and can handle interaction terms, including a subclass
variable. The function ensures that the subclass variable is not
redundantly included in the baseline variables.

## Usage

``` r
construct_formula(
  Y,
  X = 1,
  baseline_vars,
  continuous_X = FALSE,
  splines = FALSE,
  subclass = NULL
)
```

## Arguments

- Y:

  A string specifying the dependent variable in the model.

- X:

  A string specifying the independent treatment or exposure variable;
  defaults to 1 (intercept only model).

- baseline_vars:

  A character vector of baseline covariate names to include in the
  model.

- continuous_X:

  A logical indicating whether \`X\` is a continuous variable; if TRUE
  and \`splines\` is TRUE, applies spline transformation to \`X\`.

- splines:

  A logical indicating whether to apply spline transformations to the
  treatment variable \`X\`.

- subclass:

  An optional string specifying a subclass variable for interaction with
  \`X\` and baseline covariates.

## Value

A string representing the constructed formula for use in regression
modeling functions like \`glm\`.

## Examples

``` r
# Example with basic interaction terms without subclass:
construct_formula("health_outcome", "treatment", c("age", "sex"), FALSE, FALSE)
#> [1] "health_outcome ~ treatment * ( age+sex )"

# Example with spline transformation for a continuous treatment:
construct_formula("health_outcome", "treatment", c("age", "sex"), TRUE, TRUE)
#> [1] "health_outcome ~ bs( treatment ) * ( age+sex )"

# Example including a subclass variable:
construct_formula("health_outcome", "treatment", c("age", "sex", "income"), FALSE, FALSE, "region")
#> [1] "health_outcome ~ region * ( treatment * ( age+sex+income ) )"

# Example with continuous treatment, splines, and subclass interaction:
construct_formula("health_outcome", "treatment", c("age", "sex", "income"), TRUE, TRUE, "region")
#> [1] "health_outcome ~ region * ( bs( treatment ) * ( age+sex+income ) )"
```
