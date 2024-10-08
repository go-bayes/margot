% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_log_transform_vars.R
\name{margot_log_transform_vars}
\alias{margot_log_transform_vars}
\title{Log-transform Variables in a Data Frame}
\usage{
margot_log_transform_vars(
  data,
  vars,
  exceptions = character(0),
  prefix = "log_",
  keep_original = TRUE
)
}
\arguments{
\item{data}{A data frame to process.}

\item{vars}{A character vector of variable names or a tidyselect helper (e.g., starts_with("hours_")).}

\item{exceptions}{A character vector of variable names to exclude from transformation.}

\item{prefix}{A string to prepend to the names of transformed variables. Default is "log_".}

\item{keep_original}{Logical. If TRUE, keeps both original and transformed variables. If FALSE, replaces original variables. Default is TRUE.}
}
\value{
A data frame with log-transformed variables.
}
\description{
This function applies a log(x + 1) transformation to specified variables in a data frame.
It handles NA values, allows for exceptions, and can be applied to variables with specific prefixes.
}
\examples{
df <- data.frame(
  hours_work = c(0, 1, 5, NA),
  hours_sleep = c(6, 7, 8, 9),
  income = c(1000, 2000, 3000, 4000)
)
transformed_df <- margot_log_transform_vars(df,
                                            vars = c(starts_with("hours_"), "income"),
                                            exceptions = "hours_work")

}
