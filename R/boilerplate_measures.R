#' Generate Appendix of Measures (DEPRECATED)
#'
#' @description
#' \lifecycle{deprecated}
#' This function is deprecated. Please use `boilerplate::boilerplate_report_variables()` instead.
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
#' # boilerplate::boilerplate_report_variables(...)
#' }
#'
#' @import lifecycle
#'
#' @export
boilerplate_measures <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.22",
    what = "boilerplate_measures()",
    with = "boilerplate::boilerplate_report_measures()"
  )

  message("This function is deprecated. Please use boilerplate::boilerplate_report_variables() instead.")
  message("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  message("After installation, load the package with: library(boilerplate)")
}
#' #' Generate Appendix of Measures (Soft Deprecated)
#' #'
#' #' @description
#' #' \lifecycle{soft-deprecated}
#' #' This function is soft deprecated. Please consider using `boilerplate::boilerplate_report_variables()` instead.
#' #' Install the package with: devtools::install_github("go-bayes/boilerplate")
#' #'
#' #' @param exposure_var A single character string naming the exposure variable, or NULL if not applicable.
#' #' @param outcome_vars A list of character vectors, where each element is named after a domain
#' #'   and contains the variable names for that domain, or NULL if not applicable.
#' #' @param measure_data A list containing information about each measure.
#' #' @param appendices_measures An optional string specifying the appendix reference.
#' #'
#' #' @return A character string containing the formatted appendix text.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # This function is soft deprecated. Consider using instead:
#' #' # devtools::install_github("go-bayes/boilerplate")
#' #' # library(boilerplate)
#' #' # boilerplate::boilerplate_report_variables(...)
#' #' }
#' #'
#' #' @import lifecycle
#' #'
#' #' @export
#' boilerplate_measures <- function(exposure_var,
#'                                  outcome_vars,
#'                                  measure_data,
#'                                  appendices_measures = NULL) {
#'   lifecycle::deprecate_soft(
#'     when = "0.2.1.22",
#'     what = "boilerplate_measures()",
#'     with = "boilerplate::boilerplate_report_variables()",
#'     details = c(
#'       "This function is soft deprecated and will be removed in a future version.",
#'       "Please consider using `boilerplate::boilerplate_report_variables()` instead.",
#'       "You can install the package with: devtools::install_github('go-bayes/boilerplate')",
#'       "After installation, load the package with: library(boilerplate)"
#'     )
#'   )
#'   # Helper function to format a single measure or scale item
#'   format_measure <- function(var_name, measure_info) {
#'     if (is.null(measure_info)) {
#'       warning(paste("No information available for variable:", var_name))
#'       return(paste0("##### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
#'     }
#'     title <- janitor::make_clean_names(var_name, case = "title")
#'     # Add variable type indicator
#'     if (endsWith(var_name, "_binary")) {
#'       title <- paste0(title, " (Binary)")
#'     } else if (endsWith(var_name, "_cat")) {
#'       title <- paste0(title, " (Categorical)")
#'     }
#'     description <- trimws(measure_info$description)
#'     reference <- measure_info$reference
#'     # Format the description with reference
#'     if (grepl("^string_is\\s+", reference)) {
#'       string_content <- sub("^string_is\\s+", "", reference)
#'       string_content <- gsub("^[\"']|[\"']$", "", string_content)
#'       description_with_ref <- paste0(description, " ", string_content)
#'     } else {
#'       description_with_ref <- paste0(description, " [@", reference, "]")
#'     }
#'     formatted_text <- paste0("##### ", title, "\n", description_with_ref, "\n")
#'     # If the measure is a scale item, include its items
#'     if ("items" %in% names(measure_info)) {
#'       formatted_text <- paste0(
#'         formatted_text,
#'         "This dimension includes the following item",
#'         if(length(measure_info$items) > 1) "s" else "", ":\n\n"
#'       )
#'
#'       if (length(measure_info$items) == 1) {
#'         # Use bullet point for single item
#'         formatted_text <- paste0(
#'           formatted_text,
#'           "• ", measure_info$items[1], "\n"
#'         )
#'       } else {
#'         # Use numbers for multiple items
#'         for (i in seq_along(measure_info$items)) {
#'           formatted_text <- paste0(
#'             formatted_text,
#'             i, ". ", measure_info$items[i], "\n"
#'           )
#'         }
#'       }
#'       formatted_text <- paste0(formatted_text, "\n")
#'     }
#'     return(formatted_text)
#'   }
#'
#'   # Generate exposure section
#'   exposure_section <- paste0(
#'     "#### Exposure Indicators\n",
#'     format_measure(exposure_var, measure_data[[exposure_var]])
#'   )
#'
#'   # Generate outcome sections by domain
#'   outcome_sections <- lapply(names(outcome_vars), function(domain) {
#'     domain_vars <- outcome_vars[[domain]]
#'     domain_section <- paste0("#### Outcome Domain: ", janitor::make_clean_names(domain, case = "title"), "\n")
#'     for (var in domain_vars) {
#'       domain_section <- paste0(domain_section, format_measure(var, measure_data[[var]]))
#'     }
#'     return(domain_section)
#'   })
#'
#'   # Combine all sections
#'   full_appendix <- paste0(
#'     "### Indicators\n",
#'     exposure_section,
#'     paste(outcome_sections, collapse = "\n\n")
#'   )
#'
#'   # Add appendix reference if provided
#'   if (!is.null(appendices_measures)) {
#'     appendix_text <- paste0("\n\nDetailed descriptions of how these variables were measured and operationalized can be found in **Appendix ", appendices_measures, "**.")
#'     full_appendix <- paste0(full_appendix, appendix_text)
#'   }
#'
#'   return(full_appendix)
#' }
# boilerplate_measures <- function(exposure_var,
#                                  outcome_vars,
#                                  measure_data,
#                                  appendices_measures = NULL) {
#
#   # Helper function to format a single measure or scale item
#   format_measure <- function(var_name, measure_info) {
#     if (is.null(measure_info)) {
#       warning(paste("No information available for variable:", var_name))
#       return(paste0("##### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
#     }
#
#     title <- janitor::make_clean_names(var_name, case = "title")
#
#     # Add variable type indicator
#     if (endsWith(var_name, "_binary")) {
#       title <- paste0(title, " (Binary)")
#     } else if (endsWith(var_name, "_cat")) {
#       title <- paste0(title, " (Categorical)")
#     }
#
#     description <- trimws(measure_info$description)
#     reference <- measure_info$reference
#
#     # Format the description with reference
#     if (grepl("^string_is\\s+", reference)) {
#       string_content <- sub("^string_is\\s+", "", reference)
#       string_content <- gsub("^[\"']|[\"']$", "", string_content)
#       description_with_ref <- paste0(description, " ", string_content)
#     } else {
#       description_with_ref <- paste0(description, " [@", reference, "]")
#     }
#
#     formatted_text <- paste0("##### ", title, "\n", description_with_ref, "\n")
#
#     # If the measure is a scale item, include its items
#     if ("items" %in% names(measure_info)) {
#       formatted_text <- paste0(formatted_text, "This dimension includes the following items:\n")
#       for (i in seq_along(measure_info$items)) {
#         formatted_text <- paste0(formatted_text, "   ", letters[i], ". ", measure_info$items[i], "\n")
#       }
#     }
#
#     return(formatted_text)
#   }
#
#   # Generate exposure section
#   exposure_section <- paste0(
#     "#### Exposure Indicators\n",
#     format_measure(exposure_var, measure_data[[exposure_var]])
#   )
#
#   # Generate outcome sections by domain
#   outcome_sections <- lapply(names(outcome_vars), function(domain) {
#     domain_vars <- outcome_vars[[domain]]
#     domain_section <- paste0("#### Outcome Domain: ", janitor::make_clean_names(domain, case = "title"), "\n")
#     for (var in domain_vars) {
#       domain_section <- paste0(domain_section, format_measure(var, measure_data[[var]]))
#     }
#     return(domain_section)
#   })
#
#   # Combine all sections
#   full_appendix <- paste0(
#     "### Indicators\n",
#     exposure_section,
#     paste(outcome_sections, collapse = "\n\n")
#   )
#
#   # Add appendix reference if provided
#   if (!is.null(appendices_measures)) {
#     appendix_text <- paste0("\n\nDetailed descriptions of how these variables were measured and operationalized can be found in **Appendix ", appendices_measures, "**.")
#     full_appendix <- paste0(full_appendix, appendix_text)
#   }
#
#   return(full_appendix)
# }
# boilerplate_measures <- function(exposure_var,
#                                  outcome_vars,
#                                  measure_data,
#                                  appendices_measures = NULL) {
#
#   # Helper function to format a single measure or scale item
#   format_measure <- function(var_name, measure_info) {
#     if (is.null(measure_info)) {
#       warning(paste("No information available for variable:", var_name))
#       return(paste0("##### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
#     }
#
#     title <- janitor::make_clean_names(var_name, case = "title")
#
#     # Add variable type indicator
#     if (endsWith(var_name, "_binary")) {
#       title <- paste0(title, " (Binary)")
#     } else if (endsWith(var_name, "_cat")) {
#       title <- paste0(title, " (Categorical)")
#     }
#
#     description <- trimws(measure_info$description)
#     reference <- measure_info$reference
#
#     # Format the description with reference
#     if (grepl("^string_is\\s+", reference)) {
#       string_content <- sub("^string_is\\s+", "", reference)
#       string_content <- gsub("^[\"']|[\"']$", "", string_content)
#       description_with_ref <- paste0(description, " ", string_content)
#     } else {
#       description_with_ref <- paste0(description, " [@", reference, "]")
#     }
#
#     formatted_text <- paste0("##### ", title, "\n", description_with_ref, "\n")
#
#     # If the measure is a scale item, include its items
#     if ("items" %in% names(measure_info)) {
#       formatted_text <- paste0(formatted_text, "This dimension includes the following items:\n")
#       for (i in seq_along(measure_info$items)) {
#         formatted_text <- paste0(formatted_text, "   ", letters[i], ". ", measure_info$items[i], "\n")
#       }
#     }
#
#     return(formatted_text)
#   }
#
#   # Generate exposure section
#   exposure_section <- paste0(
#     "#### Exposure Indicators\n",
#     format_measure(exposure_var, measure_data[[exposure_var]])
#   )
#
#   # Generate outcome sections by domain
#   outcome_sections <- lapply(names(outcome_vars), function(domain) {
#     domain_vars <- outcome_vars[[domain]]
#     domain_section <- paste0("#### Outcome Domain: ", janitor::make_clean_names(domain, case = "title"), "\n")
#     for (var in domain_vars) {
#       domain_section <- paste0(domain_section, format_measure(var, measure_data[[var]]))
#     }
#     return(domain_section)
#   })
#
#   # Combine all sections
#   full_appendix <- paste0(
#     "### Indicators\n",
#     exposure_section,
#     paste(outcome_sections, collapse = "\n\n")
#   )
#
#   # Add appendix reference if provided
#   if (!is.null(appendices_measures)) {
#     appendix_text <- paste0("\n\nDetailed descriptions of how these variables were measured and operationalized can be found in **Appendix ", appendices_measures, "**.")
#     full_appendix <- paste0(full_appendix, appendix_text)
#   }
#
#   return(full_appendix)
# }
# boilerplate_measures <- function(exposure_var,
#                                  outcome_vars,
#                                  measure_data,
#                                  appendices_measures = NULL) {
#
#   # Helper function to format a single measure or scale item
#   format_measure <- function(var_name, measure_info) {
#     if (is.null(measure_info)) {
#       warning(paste("No information available for variable:", var_name))
#       return(paste0("##### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
#     }
#
#     title <- janitor::make_clean_names(var_name, case = "title")
#
#     # Add variable type indicator
#     if (endsWith(var_name, "_binary")) {
#       title <- paste0(title, " (Binary)")
#     } else if (endsWith(var_name, "_cat")) {
#       title <- paste0(title, " (Categorical)")
#     }
#
#     description <- trimws(measure_info$description)
#     reference <- measure_info$reference
#
#     # Format the description with reference
#     if (grepl("^string_is\\s+", reference)) {
#       string_content <- sub("^string_is\\s+", "", reference)
#       string_content <- gsub("^[\"']|[\"']$", "", string_content)
#       description_with_ref <- paste0(description, " ", string_content)
#     } else {
#       description_with_ref <- paste0(description, " [@", reference, "]")
#     }
#
#     formatted_text <- paste0("##### ", title, "\n", description_with_ref, "\n")
#
#     # If the measure is a scale item, include its items
#     if ("items" %in% names(measure_info)) {
#       formatted_text <- paste0(formatted_text, "This dimension includes the following items:\n")
#       for (i in seq_along(measure_info$items)) {
#         formatted_text <- paste0(formatted_text, "   ", letters[i], ". ", measure_info$items[i], "\n")
#       }
#     }
#
#     return(formatted_text)
#   }
#
#   # Generate exposure section
#   exposure_section <- paste0(
#     "#### Exposure Indicators\n",
#     format_measure(exposure_var, measure_data[[exposure_var]])
#   )
#
#   # Generate outcome sections by domain
#   outcome_sections <- lapply(names(outcome_vars), function(domain) {
#     domain_vars <- outcome_vars[[domain]]
#     domain_section <- paste0("#### Outcome Domain: ", janitor::make_clean_names(domain, case = "title"), "\n")
#     for (var in domain_vars) {
#       domain_section <- paste0(domain_section, format_measure(var, measure_data[[var]]))
#     }
#     return(domain_section)
#   })
#
#   # Combine all sections
#   full_appendix <- paste0(
#     "### Indicators\n",
#     exposure_section,
#     paste(outcome_sections, collapse = "\n\n")
#   )
#
#   # Add appendix reference if provided
#   if (!is.null(appendices_measures)) {
#     appendix_text <- paste0("\n\nDetailed descriptions of how these variables were measured and operationalized can be found in **Appendix ", appendices_measures, "**.")
#     full_appendix <- paste0(full_appendix, appendix_text)
#   }
#
#   return(full_appendix)
# }
# boilerplate_measures <- function(all_vars = NULL,
#                                  baseline_vars = NULL,
#                                  exposure_var = NULL,
#                                  outcome_vars = NULL,
#                                  measure_data,
#                                  custom_titles = NULL,
#                                  print_keywords = FALSE,
#                                  print_waves = FALSE) {
#   # Input validation and processing
#   if (!is.null(all_vars)) {
#     if (is.logical(all_vars) && all_vars) {
#       all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
#     } else if (!is.character(all_vars)) {
#       stop("all_vars must be either TRUE or a character vector")
#     }
#   } else if (is.null(baseline_vars) || is.null(exposure_var) || is.null(outcome_vars)) {
#     stop("Either all_vars or all of baseline_vars, exposure_var, and outcome_vars must be provided")
#   } else {
#     all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
#   }
#
#   if (!is.list(measure_data)) {
#     stop("measure_data must be a list")
#   }
#
#   if (!is.null(custom_titles) && (!is.list(custom_titles) || is.null(names(custom_titles)))) {
#     stop("custom_titles must be a named list or NULL")
#   }
#
#   # Check if all variables are in measure_data
#   missing_vars <- all_vars[!all_vars %in% names(measure_data)]
#   if (length(missing_vars) > 0) {
#     warning(paste("The following variables are not in measure_data:", paste(missing_vars, collapse = ", ")))
#   }
#
#   # Helper function to format a single measure or scale item
#   format_measure <- function(var_name, measure_info) {
#     if (is.null(measure_info)) {
#       warning(paste("No information available for variable:", var_name))
#       return(paste0("#### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
#     }
#
#     if (!all(c("description", "reference") %in% names(measure_info))) {
#       warning(paste("Measure info for", var_name, "is missing 'description' or 'reference'"))
#       missing_fields <- setdiff(c("description", "reference"), names(measure_info))
#       for (field in missing_fields) {
#         measure_info[[field]] <- "Not provided"
#       }
#     }
#
#     # Use custom title if provided, otherwise use clean name
#     if (!is.null(custom_titles) && var_name %in% names(custom_titles)) {
#       title <- custom_titles[[var_name]]
#     } else {
#       title <- janitor::make_clean_names(var_name, case = "title")
#     }
#
#     # Add variable type indicator
#     if (endsWith(var_name, "_binary")) {
#       title <- paste0(title, " (Binary)")
#     } else if (endsWith(var_name, "_cat")) {
#       title <- paste0(title, " (Categorical)")
#     }
#
#     # Include wave information if available and print_waves is TRUE
#     if (print_waves && "waves" %in% names(measure_info)) {
#       title <- paste0(title, " (waves: ", measure_info$waves, ")")
#     }
#     # Sort all_vars alphabetically
#     all_vars <- sort(all_vars)
#
#     # Combine description and reference
#     description <- trimws(measure_info$description)
#     reference <- measure_info$reference
#
#     # Check if the reference is a 'string_is' type
#     if (grepl("^string_is\\s+", reference)) {
#       # Extract the string after 'string_is '
#       string_content <- sub("^string_is\\s+", "", reference)
#
#       # Remove surrounding quotes if present
#       string_content <- gsub("^[\"']|[\"']$", "", string_content)
#
#       # Combine description and string content
#       if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
#         description_with_ref <- paste0(description, " ", string_content)
#       } else {
#         description_with_ref <- paste0(description, ". ", string_content)
#       }
#
#       # Ensure the combined text ends with a period
#       if (!grepl("[.!?]$", description_with_ref)) {
#         description_with_ref <- paste0(description_with_ref, ".")
#       }
#     } else {
#       # Original citation formatting
#       if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
#         description_with_ref <- paste0(
#           substr(description, 1, nchar(description) - 1),
#           " [@",
#           reference,
#           "]",
#           substr(description, nchar(description), nchar(description))
#         )
#       } else {
#         description_with_ref <- paste0(description, " [@", reference, "].")
#       }
#     }
#
#     formatted_text <- paste0("#### ", title, "\n\n", description_with_ref, "\n\n")
#
#     # If the measure is a scale item, include its items
#     if ("items" %in% names(measure_info)) {
#       formatted_text <- paste0(formatted_text, "This dimension includes the following items:\n\n")
#       for (i in seq_along(measure_info$items)) {
#         formatted_text <- paste0(formatted_text, "   ", letters[i], ". ", measure_info$items[i], "\n")
#       }
#       formatted_text <- paste0(formatted_text, "\n")
#     }
#
#     # Include keywords if available and print_keywords is TRUE
#     if (print_keywords && "keywords" %in% names(measure_info) && length(measure_info$keywords) > 0) {
#       formatted_text <- paste0(formatted_text, "Keywords: ", paste(measure_info$keywords, collapse = ", "), "\n\n")
#     }
#
#     return(formatted_text)
#   }
#
#   # Generate section for all variables
#   generate_section <- function(vars, section_title) {
#     section <- paste0("### ", section_title, "\n\n")
#     # vars are already sorted in the main function, so we don't need to sort here
#     for (var in vars) {
#       section <- paste0(section, format_measure(var, measure_data[[var]]))
#     }
#     return(section)
#   }
#
#   # Generate main section with all variables
#   main_section <- generate_section(all_vars, "All Measures")
#
#   # Generate keywords section only if print_keywords is TRUE
#   # Generate keywords section only if print_keywords is TRUE
#   keywords_section <- ""
#   if (print_keywords) {
#     all_keywords <- unique(unlist(lapply(all_vars, function(var) {
#       if (!is.null(measure_data[[var]]) && "keywords" %in% names(measure_data[[var]])) {
#         return(measure_data[[var]]$keywords)
#       }
#       return(NULL)
#     })))
#
#     if (length(all_keywords) > 0) {
#       keywords_section <- paste0(
#         "### Keywords Used\n\n",
#         paste(sort(all_keywords), collapse = ", "),
#         "\n\n"
#       )
#     }
#   }
#
#
#   # Combine all sections
#   full_appendix <- paste0(
#     "## Appendix: Measures\n\n",
#     main_section,
#     keywords_section
#   )
#
#   return(full_appendix)
# }
