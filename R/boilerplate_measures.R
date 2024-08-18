#' Generate Appendix for Research Measures
#'
#' This function generates a formatted appendix describing research measures,
#' including baseline variables, exposure variables, and outcome variables.
#' It can handle both individual measures and scales with multiple dimensions,
#' and includes optional keywords for each measure and custom titles.
#'
#' @param all_vars A logical value or a character vector. If TRUE, outputs all unique
#'   variables from baseline_vars, exposure_var, and outcome_vars. If a character vector,
#'   it replaces the need for separate baseline_vars, exposure_var, and outcome_vars.
#' @param baseline_vars A character vector of baseline variable names (optional if all_vars is provided).
#' @param exposure_var A single character string naming the exposure variable (optional if all_vars is provided).
#' @param outcome_vars A character vector of outcome variable names (optional if all_vars is provided).
#' @param measure_data A list containing information about each measure. Each element
#'   should be a list with at least 'description' and 'reference' fields. Optional
#'   fields include 'waves', 'items' for scale dimensions, and 'keywords'.
#' @param custom_titles An optional named list of custom titles for measures.
#' @param print_keywords A logical value indicating whether to print keywords. Default is FALSE.
#' @param print_waves A logical value indicating whether to print wave information. Default is FALSE.
#'
#' @return A character string containing the formatted appendix text.
#'
#' @import janitor
#'
#' @export
boilerplate_measures <- function(all_vars = NULL,
                                 baseline_vars = NULL,
                                 exposure_var = NULL,
                                 outcome_vars = NULL,
                                 measure_data,
                                 custom_titles = NULL,
                                 print_keywords = FALSE,
                                 print_waves = FALSE) {
  # Input validation and processing
  if (!is.null(all_vars)) {
    if (is.logical(all_vars) && all_vars) {
      all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
    } else if (!is.character(all_vars)) {
      stop("all_vars must be either TRUE or a character vector")
    }
  } else if (is.null(baseline_vars) || is.null(exposure_var) || is.null(outcome_vars)) {
    stop("Either all_vars or all of baseline_vars, exposure_var, and outcome_vars must be provided")
  } else {
    all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
  }

  if (!is.list(measure_data)) {
    stop("measure_data must be a list")
  }

  if (!is.null(custom_titles) && (!is.list(custom_titles) || is.null(names(custom_titles)))) {
    stop("custom_titles must be a named list or NULL")
  }

  # Check if all variables are in measure_data
  missing_vars <- all_vars[!all_vars %in% names(measure_data)]
  if (length(missing_vars) > 0) {
    warning(paste("The following variables are not in measure_data:", paste(missing_vars, collapse = ", ")))
  }

  # Helper function to format a single measure or scale item
  format_measure <- function(var_name, measure_info) {
    if (is.null(measure_info)) {
      warning(paste("No information available for variable:", var_name))
      return(paste0("#### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
    }

    if (!all(c("description", "reference") %in% names(measure_info))) {
      warning(paste("Measure info for", var_name, "is missing 'description' or 'reference'"))
      missing_fields <- setdiff(c("description", "reference"), names(measure_info))
      for (field in missing_fields) {
        measure_info[[field]] <- "Not provided"
      }
    }

    # Use custom title if provided, otherwise use clean name
    if (!is.null(custom_titles) && var_name %in% names(custom_titles)) {
      title <- custom_titles[[var_name]]
    } else {
      title <- janitor::make_clean_names(var_name, case = "title")
    }

    # Add variable type indicator
    if (endsWith(var_name, "_binary")) {
      title <- paste0(title, " (Binary)")
    } else if (endsWith(var_name, "_cat")) {
      title <- paste0(title, " (Categorical)")
    }

    # Include wave information if available and print_waves is TRUE
    if (print_waves && "waves" %in% names(measure_info)) {
      title <- paste0(title, " (waves: ", measure_info$waves, ")")
    }
    # Sort all_vars alphabetically
    all_vars <- sort(all_vars)

    # Combine description and reference
    description <- trimws(measure_info$description)
    reference <- measure_info$reference

    # Check if the reference is a 'string_is' type
    if (grepl("^string_is\\s+", reference)) {
      # Extract the string after 'string_is '
      string_content <- sub("^string_is\\s+", "", reference)

      # Remove surrounding quotes if present
      string_content <- gsub("^[\"']|[\"']$", "", string_content)

      # Combine description and string content
      if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
        description_with_ref <- paste0(description, " ", string_content)
      } else {
        description_with_ref <- paste0(description, ". ", string_content)
      }

      # Ensure the combined text ends with a period
      if (!grepl("[.!?]$", description_with_ref)) {
        description_with_ref <- paste0(description_with_ref, ".")
      }
    } else {
      # Original citation formatting
      if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
        description_with_ref <- paste0(
          substr(description, 1, nchar(description) - 1),
          " [@",
          reference,
          "]",
          substr(description, nchar(description), nchar(description))
        )
      } else {
        description_with_ref <- paste0(description, " [@", reference, "].")
      }
    }

    formatted_text <- paste0("#### ", title, "\n\n", description_with_ref, "\n\n")

    # If the measure is a scale item, include its items
    if ("items" %in% names(measure_info)) {
      formatted_text <- paste0(formatted_text, "This dimension includes the following items:\n\n")
      for (i in seq_along(measure_info$items)) {
        formatted_text <- paste0(formatted_text, "   ", letters[i], ". ", measure_info$items[i], "\n")
      }
      formatted_text <- paste0(formatted_text, "\n")
    }

    # Include keywords if available and print_keywords is TRUE
    if (print_keywords && "keywords" %in% names(measure_info) && length(measure_info$keywords) > 0) {
      formatted_text <- paste0(formatted_text, "Keywords: ", paste(measure_info$keywords, collapse = ", "), "\n\n")
    }

    return(formatted_text)
  }

  # Generate section for all variables
  generate_section <- function(vars, section_title) {
    section <- paste0("### ", section_title, "\n\n")
    # vars are already sorted in the main function, so we don't need to sort here
    for (var in vars) {
      section <- paste0(section, format_measure(var, measure_data[[var]]))
    }
    return(section)
  }

  # Generate main section with all variables
  main_section <- generate_section(all_vars, "All Measures")

  # Generate keywords section only if print_keywords is TRUE
  # Generate keywords section only if print_keywords is TRUE
  keywords_section <- ""
  if (print_keywords) {
    all_keywords <- unique(unlist(lapply(all_vars, function(var) {
      if (!is.null(measure_data[[var]]) && "keywords" %in% names(measure_data[[var]])) {
        return(measure_data[[var]]$keywords)
      }
      return(NULL)
    })))

    if (length(all_keywords) > 0) {
      keywords_section <- paste0(
        "### Keywords Used\n\n",
        paste(sort(all_keywords), collapse = ", "),
        "\n\n"
      )
    }
  }


  # Combine all sections
  full_appendix <- paste0(
    "## Appendix: Measures\n\n",
    main_section,
    keywords_section
  )

  return(full_appendix)
}
# boilerplate_measures <- function(baseline_vars,
#                                  exposure_var,
#                                  outcome_vars,
#                                  measure_data,
#                                  custom_titles = NULL,
#                                  print_keywords = FALSE) {
#   # Check input types
#   if (!is.list(measure_data)) {
#     stop("measure_data must be a list")
#   }
#   if (!is.character(baseline_vars) ||
#       !is.character(exposure_var) ||
#       !is.character(outcome_vars)) {
#     stop("baseline_vars, exposure_var, and outcome_vars must be character vectors")
#   }
#   if (length(exposure_var) != 1) {
#     stop("exposure_var must be a single variable name")
#   }
#
#   # Check if custom_titles is provided and is a named list
#   if (!is.null(custom_titles) &&
#       (!is.list(custom_titles) ||
#        is.null(names(custom_titles)))) {
#     stop("custom_titles must be a named list or NULL")
#   }
#
#   # Check if all variables are in measure_data
#   all_vars <- c(baseline_vars, exposure_var, outcome_vars)
#   missing_vars <- all_vars[!all_vars %in% names(measure_data)]
#   if (length(missing_vars) > 0) {
#     warning(paste(
#       "The following variables are not in measure_data:",
#       paste(missing_vars, collapse = ", ")
#     ))
#   }
#
#   # Helper function to format a single measure or scale item
#   format_measure <- function(var_name, measure_info) {
#     if (!all(c("description", "reference") %in% names(measure_info))) {
#       stop(paste(
#         "Measure info for",
#         var_name,
#         "is missing 'description' or 'reference'"
#       ))
#     }
#
#     # Use custom title if provided, otherwise use clean name
#     if (!is.null(custom_titles) &&
#         var_name %in% names(custom_titles)) {
#       title <- custom_titles[[var_name]]
#     } else {
#       title <- janitor::make_clean_names(var_name, case = "title")
#     }
#
#     # Include wave information if available
#     if ("waves" %in% names(measure_info)) {
#       title <- paste0(title, " (waves: ", measure_info$waves, ")")
#     }
#
#     # Combine description and reference
#     description <- trimws(measure_info$description)  # Remove leading/trailing whitespace
#     if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
#       # If description ends with punctuation, place reference before the punctuation
#       description_with_ref <- paste0(
#         substr(description, 1, nchar(description) - 1),
#         " [@",
#         measure_info$reference,
#         "]",
#         substr(description, nchar(description), nchar(description))
#       )
#     } else {
#       # If description doesn't end with punctuation, add period after reference
#       description_with_ref <- paste0(description, " [@", measure_info$reference, "].")
#     }
#
#     formatted_text <- paste0("#### ", title, "\n\n", description_with_ref, "\n\n")
#
#     # If the measure is a scale item, include its items
#     if ("items" %in% names(measure_info)) {
#       formatted_text <- paste0(formatted_text,
#                                "This dimension includes the following items:\n\n")
#       for (i in seq_along(measure_info$items)) {
#         formatted_text <- paste0(formatted_text,
#                                  "   ",
#                                  letters[i],
#                                  ". ",
#                                  measure_info$items[i],
#                                  "\n")
#       }
#       formatted_text <- paste0(formatted_text, "\n")
#     }
#
#     # Include keywords if available and print_keywords is TRUE
#     if (print_keywords && "keywords" %in% names(measure_info) &&
#         length(measure_info$keywords) > 0) {
#       formatted_text <- paste0(
#         formatted_text,
#         "Keywords: ",
#         paste(measure_info$keywords, collapse = ", "),
#         "\n\n"
#       )
#     }
#
#     return(formatted_text)
#   }
#
#   # Helper function to generate a section
#   generate_section <- function(vars, section_title) {
#     section <- paste0("### ", section_title, "\n\n")
#     for (var in vars) {
#       if (var %in% names(measure_data)) {
#         section <- paste0(section, format_measure(var, measure_data[[var]]))
#       }
#     }
#     return(section)
#   }
#
#   # Generate sections
#   baseline_section <- generate_section(baseline_vars, "Baseline Measures")
#   exposure_section <- generate_section(exposure_var, "Exposure Measure")
#   outcome_section <- generate_section(outcome_vars, "Outcome Measures")
#
#   # Generate keywords section only if print_keywords is TRUE
#   keywords_section <- ""
#   if (print_keywords) {
#     generate_keywords_section <- function(vars) {
#       all_keywords <- unique(unlist(lapply(vars, function(var) {
#         if (var %in% names(measure_data) &&
#             "keywords" %in% names(measure_data[[var]])) {
#           return(measure_data[[var]]$keywords)
#         }
#         return(NULL)
#       })))
#
#       if (length(all_keywords) > 0) {
#         return(paste0(
#           "### Keywords Used\n\n",
#           paste(sort(all_keywords), collapse = ", "),
#           "\n\n"
#         ))
#       } else {
#         return("")
#       }
#     }
#
#     keywords_section <- generate_keywords_section(all_vars)
#   }
#
#   # Combine all sections
#   full_appendix <- paste0(
#     "## Appendix: Measures\n\n",
#     baseline_section,
#     exposure_section,
#     outcome_section,
#     keywords_section
#   )
#
#   return(full_appendix)
# }
