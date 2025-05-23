% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_compute_policy_value_helpers.R
\name{margot_add_policy_values}
\alias{margot_add_policy_values}
\title{Add bootstrap policy-value tests for multiple depths}
\usage{
margot_add_policy_values(model, depths = c(1L, 2L), R = 499L, seed = 42L)
}
\arguments{
\item{model}{list. One element of `margot$results`.}

\item{depths}{integer vector. Depths to evaluate (default `c(1, 2)`).}

\item{R, seed}{Passed to [margot_compute_policy_value()].}
}
\value{
The modified `model` (invisibly).
}
\description{
Add bootstrap policy-value tests for multiple depths
}
\keyword{internal}
