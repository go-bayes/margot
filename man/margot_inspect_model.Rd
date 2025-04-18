% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_flip_forests.R
\name{margot_inspect_model}
\alias{margot_inspect_model}
\title{Inspect and Debug Model Result Structure}
\usage{
margot_inspect_model(model_results, model_name = NULL, details = FALSE)
}
\arguments{
\item{model_results}{A list containing the model results from margot_causal_forest().}

\item{model_name}{Name of a specific model to inspect. If NULL, summarizes all models.}

\item{details}{Logical indicating whether to show detailed structure of objects. Default is FALSE.}
}
\value{
Invisibly returns a summary of the model structure.
}
\description{
This utility function inspects the structure of a specific model result or all models
in the results list, helping to debug issues with flipping estimates.
}
\keyword{internal}
