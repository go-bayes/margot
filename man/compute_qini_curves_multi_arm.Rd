% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_qini_curves_multi_arm.R
\name{compute_qini_curves_multi_arm}
\alias{compute_qini_curves_multi_arm}
\title{Compute Qini Curves for Multi-Arm Treatments}
\usage{
compute_qini_curves_multi_arm(
  tau_hat,
  Y,
  W_multi,
  W.hat = NULL,
  cost = NULL,
  verbose = TRUE
)
}
\arguments{
\item{tau_hat}{A matrix or 3D array of estimated treatment effects. For multi-arm treatments,
this should be a matrix where each column represents a treatment arm.}

\item{Y}{A vector or matrix of observed outcomes.}

\item{W_multi}{A factor vector of treatment assignments for multi-arm treatments.}

\item{W.hat}{Optional vector specifying known treatment assignment probabilities for each arm.}

\item{cost}{Optional vector specifying the cost associated with each treatment arm.}

\item{verbose}{Logical indicating whether to display detailed messages during execution. Default is TRUE.}
}
\value{
A list containing:
  \item{qini_data}{A data frame containing Qini curve data for plotting.}
  \item{qini_objects}{A list of maq objects for each curve, which can be used to compute average gain.}
The qini_data has an attribute "imputed" which is TRUE if any curves were imputed with zeros.
}
\description{
This function computes Qini curves for multi-arm treatment effects using the maq package.
It handles various edge cases and provides detailed information about the computation process.
}
\details{
The function computes Qini curves for all arms combined, a baseline (no covariates),
and each individual treatment arm. It handles cases where some or all Qini objects
have zero length or are NULL, extending curves with zeros when necessary.
}
\keyword{internal}
