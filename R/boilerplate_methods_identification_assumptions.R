#' Generate Identification Assumptions Description (DEPRECATED)
#'
#' @description
#' \lifecycle{deprecated}
#' This function is deprecated. Please use `boilerplate::boilerplate_report_identification_assumptions()` instead.
#' Install the new package with `devtools::install_github("go-bayes/boilerplate")`.
#'
#' @param ... All arguments (ignored)
#'
#' @return A message indicating the function is deprecated.
#'
#' @examples
#' \dontrun{
#' # This function is deprecated. Use instead:
#' # devtools::install_github("go-bayes/boilerplate")
#' # library(boilerplate)
#' # boilerplate::boilerplate_report_identification_assumptions(...)
#' }
#'
#' @import lifecycle
#' @import cli
#'
#' @export
boilerplate_methods_identification_assumptions <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.23",
    what = "boilerplate_methods_identification_assumptions()",
    with = "boilerplate::boilerplate_report_identification_assumptions()"
  )

  cli::cli_alert_warning("This function is deprecated. Please use boilerplate::boilerplate_report_identification_assumptions() instead.")
  cli::cli_alert_info("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  cli::cli_alert_info("After installation, load the package with: library(boilerplate)")
}
#' #' Generate Identification Assumptions Description
#' #'
#' #' @param exposure_var Character string specifying the primary exposure variable.
#' #'
#' #' @return A character string containing the identification assumptions description in markdown format.
#' #'
#' #' @export
#' boilerplate_methods_identification_assumptions <- function(exposure_var) {
#'   markdown_text <- glue::glue("
#' ### Identification Assumptions
#'
#' This study relies on the following key identification assumptions for estimating the causal effect of {exposure_var}:
#'
#' 1. **Consistency**: The observed outcome under the observed {exposure_var} is equal to the potential outcome under that exposure level.
#'
#' 2. **Positivity**: There is a non-zero probability of receiving each level of {exposure_var} for every combination of values of {exposure_var} and confounders in the population.
#'
#' 3. **No unmeasured confounding**: All variables that affect both {exposure_var} and the outcome have been measured and accounted for in the analysis.
#'
#' 4. **No interference**: The potential outcomes for one individual are not affected by the {exposure_var} status of other individuals.
#'
#' These assumptions are critical for the causal interpretability of our results and should be carefully considered in light of the study design and available data.
#'   ")
#'
#'   return(markdown_text)
#' }
