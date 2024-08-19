#' Generate Variables Section for Methods
#'
#' This function is a wrapper around boilerplate_measures() that generates
#' a markdown-formatted section describing the variables used in the study.
#' It ignores baseline variables and includes an optional reference to an appendix.
#'
#' @param exposure_var A character string specifying the name of the exposure variable.
#' @param outcome_vars A named list of character vectors specifying the outcome variables by domain.
#' @param measure_data A list containing information about each measure.
#' @param custom_titles An optional named list of custom titles for measures.
#' @param print_waves A logical value indicating whether to print wave information. Default is FALSE.
#' @param appendix_ref An optional character string for the appendix reference. If NULL, no reference is included.
#'
#' @return A character string containing the markdown-formatted section on variables.
#'
#' @examples
#' \dontrun{
#' # Define outcomes by domain
#' outcomes_health <- c("smoker_binary", "hlth_bmi", "log_hours_exercise")
#' outcomes_psychological <- c("hlth_fatigue", "kessler_latent_anxiety")
#' outcomes_social <- c("belong", "neighbourhood_community")
#' all_outcomes <- list(
#'   health = outcomes_health,
#'   psychological = outcomes_psychological,
#'   social = outcomes_social
#' )
#' # Define the exposure variable
#' exposure_var <- "political_conservative"
#' # Load your measure_data
#' measure_data <- readRDS(here::here("boilerplate", "data", "measure_data.rds"))
#' # Call the function
#' result <- boilerplate_methods_variables(
#'   exposure_var = exposure_var,
#'   outcome_vars = all_outcomes,
#'   measure_data = measure_data,
#'   appendix_ref = "Appendix C"
#' )
#' # Print the result
#' cat(result)
#' }
#'
#' @export
boilerplate_methods_variables <- function(exposure_var,
                                          outcome_vars,
                                          measure_data,
                                          custom_titles = NULL,
                                          print_waves = FALSE,
                                          appendix_ref = NULL) {

  # Call boilerplate_measures with NULL baseline_vars
  variables_text <- boilerplate_measures(
    baseline_vars = NULL,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars,
    measure_data = measure_data,
    custom_titles = custom_titles,
    print_waves = print_waves
  )

  # Add appendix reference if provided
  if (!is.null(appendix_ref)) {
    appendix_text <- paste0("\n\nDetailed descriptions of how these variables were measured and operationalized can be found in **", appendix_ref, "**.")
    variables_text <- paste0(variables_text, appendix_text)
  }
}
# boilerplate_methods_variables <- function(exposure_var, outcome_vars = NULL, domains = NULL, ...) {
#   if (is.null(outcome_vars)) {
#     outcome_vars <- list(
#       health = outcomes_health,
#       psychological_well_being = outcomes_psychological_well_being,
#       present_reflective = outcomes_present_reflective,
#       life_reflective = outcomes_life_reflective,
#       social = outcomes_social
#     )
#   }
#
#   if (is.null(domains)) {
#     domains <- names(outcome_vars)
#   }
#
#   outcome_vars_list <- ""
#   for (domain in domains) {
#     if (domain %in% names(outcome_vars)) {
#       domain_vars <- outcome_vars[[domain]]
#       domain_list <- paste("  -", domain_vars, collapse = "\n")
#       outcome_vars_list <- paste0(outcome_vars_list,
#                                   "\n\n",
#                                   "**", tools::toTitleCase(gsub("_", " ", domain)), "**\n",
#                                   domain_list)
#     } else {
#       warning(paste("Domain", domain, "not found in outcome_vars."))
#     }
#   }
#
#   markdown_text <- glue::glue("
# ### Variables
#
# #### Exposure Variable
#
# The primary exposure variable in this study is '{exposure_var}'.
#
# #### Outcome Variables
#
# The outcome variables examined in this study are:
# {outcome_vars_list}
#
# Detailed descriptions of how these variables were measured and operationalized can be found in the study protocol.
#   ")
#
#   return(markdown_text)
# }
