# Create Propensity Score Model and Associated Plots

This function creates a propensity score model using the specified
exposure variable and baseline covariates. It also generates associated
plots and diagnostics.

## Usage

``` r
margot_propensity_model_and_plots(
  df_propensity,
  exposure_variable,
  baseline_vars,
  weights_var_name,
  estimand = "ATE",
  method = "ebal",
  focal = NULL,
  love_plot_options = list(),
  bal_tab_options = list(),
  verbose = TRUE
)
```

## Arguments

- df_propensity:

  A data frame containing the variables for the propensity score model.

- exposure_variable:

  A character string specifying the name of the exposure variable in
  df_propensity.

- baseline_vars:

  A character vector specifying the names of the baseline covariates in
  df_propensity.

- weights_var_name:

  A character string specifying the name of the weights variable in
  df_propensity.

- estimand:

  A character string specifying the estimand. Default is "ATE" (Average
  Treatment Effect).

- method:

  A character string specifying the method for propensity score
  estimation. Default is "ebal".

- focal:

  For binary treatments, a value of the treatment to be considered
  "focal" (i.e., the intervention). Default is NULL.

- love_plot_options:

  A list of options to be passed to cobalt::love.plot(). Default is an
  empty list.

- bal_tab_options:

  A list of options to be passed to cobalt::bal.tab(). Default is an
  empty list.

- verbose:

  A logical value indicating whether to print progress messages. Default
  is TRUE.

## Value

A list containing the following elements:

- match_propensity: The propensity score model object.

- love_plot: A love plot object created by cobalt::love.plot().

- summary: A summary of the propensity score model.

- summary_plot: A plot of the propensity score model summary.

- balance_table: A balance table created by cobalt::bal.tab().

- diagnostics: A list of additional diagnostic information.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming df_propensity is your dataset with appropriate variables
results <- margot_propensity_model_and_plots(
  df_propensity = df_propensity,
  exposure_variable = "treatment",
  baseline_vars = c("age", "sex", "bmi"),
  weights_var_name = "sample_weights",
  love_plot_options = list(
    thresholds = c(m = .05),
    size = 4
  ),
  bal_tab_options = list(
    thresholds = c(m = .1, v = 2.5)
  ),
  verbose = TRUE
)

# Access the results
print(results$summary)
plot(results$love_plot)
print(results$balance_table)
} # }
```
