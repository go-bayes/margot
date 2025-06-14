% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/causal_contrast_engine.R
\name{causal_contrast_engine}
\alias{causal_contrast_engine}
\title{Compute Causal Contrasts}
\usage{
causal_contrast_engine(
  df,
  Y,
  X,
  baseline_vars,
  treat_0,
  treat_1,
  estimand = c("ATE", "ATT"),
  type = c("RR", "RD"),
  nsims = 200,
  cores = parallel::detectCores(),
  family = "gaussian",
  weights = TRUE,
  continuous_X = FALSE,
  splines = FALSE,
  vcov = "HC2",
  verbose = FALSE
)
}
\arguments{
\item{df}{Data frame or `mids` object containing the data.}

\item{Y}{Response variable name as a string.}

\item{X}{Treatment or exposure variable name as a string.}

\item{baseline_vars}{Vector of baseline covariate names.}

\item{treat_0}{Reference level of the treatment variable.}

\item{treat_1}{Treatment level of interest for comparison.}

\item{estimand}{Type of causal estimand ("ATE", "ATT"); defaults to both.}

\item{type}{Type of effect size ("RR" for Risk Ratio, "RD" for Risk Difference); defaults to both.}

\item{nsims}{Number of simulations for bootstrap; defaults to 200.}

\item{cores}{Number of cores for parallel processing; uses all available cores by default.}

\item{family}{Model family as a string or family object; defaults to "gaussian".}

\item{weights}{The name of the weights variable in the data frame, or NULL if no weights are to be used.}

\item{continuous_X}{Whether X is a continuous variable; defaults to FALSE.}

\item{splines}{Whether to apply spline transformation to X; defaults to FALSE.}

\item{vcov}{Type of variance-covariance matrix for standard error estimation; defaults to "HC2".}

\item{verbose}{Whether to print detailed output; defaults to FALSE.}
}
\value{
Depending on the configuration, returns a summary object containing estimated causal contrasts, confidence intervals, and potentially other diagnostics.
}
\description{
Estimates causal contrasts using generalized linear models for different types of treatment effects (ATE, ATT)
and outcomes (RR, RD). Supports handling multiply imputed datasets and allows flexibility in model specification.
}
\keyword{internal}
