% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_lmtp.R
\name{margot_lmtp}
\alias{margot_lmtp}
\title{Batch Process LMTP Models}
\usage{
margot_lmtp(
  data,
  outcome_vars,
  trt,
  shift_functions = list(),
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = c("pairwise", "null"),
  contrast_scale = c("additive", "rr", "or"),
  lmtp_defaults = list(),
  n_cores = parallel::detectCores() - 1,
  save_output = FALSE,
  save_path = here::here("push_mods"),
  base_filename = "lmtp_output",
  use_timestamp = FALSE,
  prefix = NULL
)
}
\arguments{
\item{data}{A data frame containing all necessary variables.}

\item{outcome_vars}{A character vector of outcome variable names to be modeled.}

\item{trt}{A character string specifying the treatment variable.}

\item{shift_functions}{A list of shift functions to be applied. Each function should take `data` and `trt` as arguments.}

\item{include_null_shift}{Logical, whether to include a null shift. Default is TRUE.}

\item{lmtp_model_type}{The LMTP model function to use. Default is lmtp_tmle.}

\item{contrast_type}{Type of contrasts to compute: "pairwise" or "null". Default is "pairwise".}

\item{contrast_scale}{Scale for contrasts: "additive", "rr", or "or". Default is "additive".}

\item{lmtp_defaults}{A list of default parameters for the LMTP models.}

\item{n_cores}{Number of cores to use for parallel processing. Default is detectCores() - 1.}

\item{save_output}{Logical, whether to save the complete output. Default is FALSE.}

\item{save_path}{The directory path to save the output. Default is "push_mods" in the current working directory.}

\item{base_filename}{The base filename for saving the output. Default is "lmtp_output".}

\item{use_timestamp}{Logical, whether to include a timestamp in the filename. Default is FALSE.}

\item{prefix}{Optional prefix to add to the saved output filename. Default is NULL.}
}
\value{
A list containing:
  \item{models}{A list of all LMTP models for each outcome and shift function.}
  \item{contrasts}{A list of contrasts computed for each outcome.}
  \item{individual_tables}{A list of individual tables for each contrast and outcome.}
  \item{combined_tables}{A list of combined tables for each contrast type across all outcomes.}
}
\description{
This function runs multiple Longitudinal Modified Treatment Policy (LMTP) models for specified outcome variables,
calculates contrasts, creates evaluation tables, and optionally saves the complete output.
}
\examples{
\dontrun{
# Assume we have a dataset 'my_data' with variables 'outcome', 'treatment', and some confounders

# Define shift functions
gain_function <- function(data, trt) {
  data[[trt]] + 1
}

loss_function <- function(data, trt) {
  pmax(data[[trt]] - 1, 0)
}

# Run LMTP analysis
result <- margot_lmtp(
  data = my_data,
  outcome_vars = c("outcome1", "outcome2"),
  trt = "treatment",
  shift_functions = list(gain = gain_function, loss = loss_function),
  lmtp_defaults = list(baseline = c("confounder1", "confounder2"),
                       time_vary = c("time_var1", "time_var2"),
                       outcome_type = "continuous"),
  save_output = TRUE,
  save_path = here::here("output", "lmtp_results"),
  prefix = "my_study"
)
}

}
