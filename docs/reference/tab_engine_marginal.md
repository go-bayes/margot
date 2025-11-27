# Tabulate Marginal Effects with E-Values

This function processes simulation results to tabulate marginal effects
along with E-values, providing a summary suited for reporting. It
supports both risk difference (RD) and risk ratio (RR) types of
estimates and handles continuous and categorical treatment variables.

## Usage

``` r
tab_engine_marginal(
  x,
  new_name,
  delta = 1,
  sd = 1,
  type = c("RD", "RR"),
  continuous_X = FALSE
)
```

## Arguments

- x:

  A data frame or matrix containing simulation results to be processed.

- new_name:

  A new name to assign to the output row, typically describing the
  variable or model.

- delta:

  The assumed smallest worthwhile effect, used for E-value calculations.

- sd:

  The standard deviation of the effect estimate, used for E-value
  calculations.

- type:

  Character vector specifying the scale of effect size, either "RD" or
  "RR". This parameter determines how the effects are calculated and
  presented.

- continuous_X:

  Logical indicating whether the treatment variable X is continuous. If
  TRUE, adjusts row names based on the type parameter.

## Value

A data frame with the specified new_name as a row name. The data frame
includes effect estimates, confidence intervals, E-values, and other
relevant statistics formatted for easy reporting.

## Examples

``` r
# Assuming you have results from a simulation or model in `results_df`
tabulated_results <- tab_engine_marginal(
  x = results_df,
  new_name = "Treatment Effect",
  delta = 1,
  sd = 0.2,
  type = "RD"
) # Corrected 'scale' to 'type'
#> Error in tab_engine_marginal(x = results_df, new_name = "Treatment Effect",     delta = 1, sd = 0.2, type = "RD"): could not find function "tab_engine_marginal"
```
