% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_omnibus_hetero_test.R
\name{margot_omnibus_hetero_test}
\alias{margot_omnibus_hetero_test}
\title{Omnibus Heterogeneity Test for GRF Models}
\usage{
margot_omnibus_hetero_test(
  model_results,
  outcome_vars = NULL,
  alpha = 0.05,
  detail_level = "standard",
  label_mapping = NULL,
  format = "table"
)
}
\arguments{
\item{model_results}{A list of model results from margot::margot_run_models_grf().}

\item{outcome_vars}{Optional. A character vector of outcome variable names. If NULL,
the function will attempt to use the outcome_vars from the model_results input.}

\item{alpha}{Significance level for hypothesis tests. Default is 0.05.}

\item{detail_level}{Character string specifying the level of detail in the output.
Options are "basic", "standard" (default), or "detailed".}

\item{label_mapping}{Optional. A named list mapping outcome variable names to display labels.
For example: list("t2_agreeableness_z" = "Agreeableness").}

\item{format}{Output format: "table" (default), "markdown", or "text".
"table" returns a tibble for use with tidyverse tools.
"markdown" returns formatted markdown text for Quarto documents.
"text" returns plain text interpretations.}
}
\value{
A list containing:
  - summary_table: A tibble with all test results
  - interpretations: Results formatted according to the format parameter
  - brief_interpretation: A concise summary of all results
}
\description{
This function performs an omnibus heterogeneity test for specified models
outputted from margot::margot_run_models_grf() and provides interpretations in a readable format.
}
