% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_interpret_rate_comparison.R
\name{margot_interpret_rate_comparison}
\alias{margot_interpret_rate_comparison}
\title{Compare and interpret RATE estimates from both AUTOC and QINI methods}
\usage{
margot_interpret_rate_comparison(autoc_df, qini_df, flipped_outcomes = NULL)
}
\arguments{
\item{autoc_df}{Data frame with AUTOC RATE results.}

\item{qini_df}{Data frame with QINI RATE results.}

\item{flipped_outcomes}{Character vector of outcome names that were inverted during pre-processing.}
}
\value{
A list containing the integrated comparison text as well as the individual
method interpretations.
}
\description{
This function compares and synthesises results from AUTOC and QINI analyses,
providing an integrated interpretation of treatment effect heterogeneity. It
evaluates prioritisation policies based on the estimated conditional treatment effect,
and examines how these estimates differ from targeting by the average
treatment effect (ATE). Negative RATE estimates provide strong evidence that CATE-based
targeting would lead to worse outcomes, while positive estimates remain suggestive.
}
\examples{
\dontrun{
comparison <- margot_interpret_rate_comparison(
  rate_table_all$rate_result,  # AUTOC results
  rate_table_all$rate_qini,    # QINI results
  flipped_outcomes = flipped_names
)
cat(comparison$comparison)
}

}
