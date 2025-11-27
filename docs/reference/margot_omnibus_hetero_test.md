# Omnibus Heterogeneity Test for GRF Models

This function performs an omnibus heterogeneity test for specified
models outputted from margot::margot_run_models_grf() and provides
interpretations in a readable format.

## Usage

``` r
margot_omnibus_hetero_test(
  model_results,
  outcome_vars = NULL,
  alpha = 0.05,
  detail_level = "standard",
  label_mapping = NULL,
  format = "table"
)
```

## Arguments

- model_results:

  A list of model results from margot::margot_run_models_grf().

- outcome_vars:

  Optional. A character vector of outcome variable names. If NULL, the
  function will attempt to use the outcome_vars from the model_results
  input.

- alpha:

  Significance level for hypothesis tests. Default is 0.05.

- detail_level:

  Character string specifying the level of detail in the output. Options
  are "basic", "standard" (default), or "detailed".

- label_mapping:

  Optional. A named list mapping outcome variable names to display
  labels. For example: list("t2_agreeableness_z" = "Agreeableness").

- format:

  Output format: "table" (default), "markdown", or "text". "table"
  returns a tibble for use with tidyverse tools. "markdown" returns
  formatted markdown text for Quarto documents. "text" returns plain
  text interpretations.

## Value

A list containing: - summary_table: A tibble with all test results -
interpretations: Results formatted according to the format parameter -
brief_interpretation: A concise summary of all results
