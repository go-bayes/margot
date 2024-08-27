#' Generate Eligibility Criteria Description (DEPRECATED)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `boilerplate::boilerplate_report_eligibility_criteria()` instead.
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
#' # boilerplate::boilerplate_report_eligibility_criteria(...)
#' }
#'
#' @import lifecycle
#' @import cli
#'
#' @export
boilerplate_methods_eligibility_criteria <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.23",
    what = "boilerplate_methods_eligibility_criteria()",
    with = "boilerplate::boilerplate_report_eligibility_criteria()"
  )

  cli::cli_alert_warning("This function is deprecated. Please use boilerplate::boilerplate_report_eligibility_criteria() instead.")
  cli::cli_alert_info("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  cli::cli_alert_info("After installation, load the package with: library(boilerplate)")
}
#' #' Generate Eligibility Criteria Description
#' #'
#' #' @param inclusion_criteria Character vector specifying the inclusion criteria.
#' #' @param exclusion_criteria Character vector specifying the exclusion criteria.
#' #' @param n_participants Numeric value indicating the number of participants meeting the criteria.
#' #'
#' #' @return A character string containing the eligibility criteria description in markdown format.
#' #'
#' @export
#' @deprecated
#' boilerplate_methods_eligibility_criteria <- function(inclusion_criteria, exclusion_criteria, n_participants, baseline_wave) {
#'   # Function to convert a list to markdown bullet points
#'   list_to_markdown <- function(criteria_list) {
#'     if (is.character(criteria_list)) {
#'       paste(sapply(criteria_list, function(x) paste0("- ", x)), collapse = "\n")
#'     } else {
#'       "- No criteria specified"
#'     }
#'   }
#'   # Convert lists to markdown strings outside of glue
#'   inclusion_md <- list_to_markdown(inclusion_criteria)
#'   exclusion_md <- list_to_markdown(exclusion_criteria)
#'   markdown_text <- glue::glue("
#' ### Eligibility Criteria
#'
#' To be included in the analysis of this study, participants needed to meet the following eligibility criteria:
#'
#' #### Inclusion Criteria
#'
#' {inclusion_md}
#' Participants may have been lost to follow-up at the end of the study if they met eligibility criteria at {baseline_wave}. We adjusted for attrition and non-response using censoring weights, described below.
#'
#' #### Exclusion Criteria
#'
#' {exclusion_md}
#' A total of {n_participants} individuals met these criteria and were included in the study.
#'   ")
#'   return(markdown_text)
#' }
