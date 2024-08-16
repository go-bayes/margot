#' Generate Appendix for Research Measures
#'
#' This function generates a formatted appendix describing research measures,
#' including baseline variables, exposure variables, and outcome variables.
#' It can handle both individual measures and scales with multiple dimensions,
#' and now includes optional keywords for each measure and custom titles.
#'
#' @param baseline_vars A character vector of baseline variable names.
#' @param exposure_var A single character string naming the exposure variable.
#' @param outcome_vars A character vector of outcome variable names.
#' @param measure_data A list containing information about each measure. Each element
#'   should be a list with at least 'description' and 'reference' fields. Optional
#'   fields include 'waves', 'items' for scale dimensions, and 'keywords'.
#' @param custom_titles An optional named list of custom titles for measures.
#' @param print_keywords A logical value indicating whether to print keywords. Default is FALSE
#'
#' @return A character string containing the formatted appendix text.
#'
#' @import janitor
#'
#' @examples
#' measure_data <- list(
#'   age = list(
#'     description = "We asked participants' ages.",
#'     reference = "author_year",
#'     waves = "1-15",
#'     keywords = c("demographic", "continuous")
#'   ),
#'   agreeableness = list(
#'     description = "Agreeableness dimension of Mini-IPIP6.",
#'     reference = "sibley2011",
#'     waves = "1-15",
#'     items = c("Item 1", "Item 2", "Item 3", "Item 4"),
#'     keywords = c("personality", "Big Five", "self-report")
#'   )
#' )
#' baseline_vars <- c("age", "agreeableness")
#' exposure_var <- "age"
#' outcome_vars <- "agreeableness"
#' custom_titles <- list(age = "Participant Age")
#'
#' # With keywords
#' appendix_text <- boilerplate_measures(baseline_vars, exposure_var, outcome_vars,
#'                                       measure_data, custom_titles)
#'
#' # Without keywords
#' appendix_text_no_keywords <- boilerplate_measures(baseline_vars, exposure_var, outcome_vars,
#'                                                   measure_data, custom_titles, print_keywords = TRUE)
#'
#' @export
boilerplate_measures <- function(baseline_vars,
                                 exposure_var,
                                 outcome_vars,
                                 measure_data,
                                 custom_titles = NULL,
                                 print_keywords = FALSE) {
  # Check input types
  if (!is.list(measure_data)) {
    stop("measure_data must be a list")
  }
  if (!is.character(baseline_vars) ||
      !is.character(exposure_var) ||
      !is.character(outcome_vars)) {
    stop("baseline_vars, exposure_var, and outcome_vars must be character vectors")
  }
  if (length(exposure_var) != 1) {
    stop("exposure_var must be a single variable name")
  }

  # Check if custom_titles is provided and is a named list
  if (!is.null(custom_titles) &&
      (!is.list(custom_titles) ||
       is.null(names(custom_titles)))) {
    stop("custom_titles must be a named list or NULL")
  }

  # Check if all variables are in measure_data
  all_vars <- c(baseline_vars, exposure_var, outcome_vars)
  missing_vars <- all_vars[!all_vars %in% names(measure_data)]
  if (length(missing_vars) > 0) {
    warning(paste(
      "The following variables are not in measure_data:",
      paste(missing_vars, collapse = ", ")
    ))
  }

  # Helper function to format a single measure or scale item
  format_measure <- function(var_name, measure_info) {
    if (!all(c("description", "reference") %in% names(measure_info))) {
      stop(paste(
        "Measure info for",
        var_name,
        "is missing 'description' or 'reference'"
      ))
    }

    # Use custom title if provided, otherwise use clean name
    if (!is.null(custom_titles) &&
        var_name %in% names(custom_titles)) {
      title <- custom_titles[[var_name]]
    } else {
      title <- janitor::make_clean_names(var_name, case = "title")
    }

    # Include wave information if available
    if ("waves" %in% names(measure_info)) {
      title <- paste0(title, " (waves: ", measure_info$waves, ")")
    }

    # Combine description and reference
    description <- trimws(measure_info$description)  # Remove leading/trailing whitespace
    if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
      # If description ends with punctuation, place reference before the punctuation
      description_with_ref <- paste0(
        substr(description, 1, nchar(description) - 1),
        " [@",
        measure_info$reference,
        "]",
        substr(description, nchar(description), nchar(description))
      )
    } else {
      # If description doesn't end with punctuation, add period after reference
      description_with_ref <- paste0(description, " [@", measure_info$reference, "].")
    }

    formatted_text <- paste0("#### ", title, "\n\n", description_with_ref, "\n\n")

    # If the measure is a scale item, include its items
    if ("items" %in% names(measure_info)) {
      formatted_text <- paste0(formatted_text,
                               "This dimension includes the following items:\n\n")
      for (i in seq_along(measure_info$items)) {
        formatted_text <- paste0(formatted_text,
                                 "   ",
                                 letters[i],
                                 ". ",
                                 measure_info$items[i],
                                 "\n")
      }
      formatted_text <- paste0(formatted_text, "\n")
    }

    # Include keywords if available and print_keywords is TRUE
    if (print_keywords && "keywords" %in% names(measure_info) &&
        length(measure_info$keywords) > 0) {
      formatted_text <- paste0(
        formatted_text,
        "Keywords: ",
        paste(measure_info$keywords, collapse = ", "),
        "\n\n"
      )
    }

    return(formatted_text)
  }

  # Helper function to generate a section
  generate_section <- function(vars, section_title) {
    section <- paste0("### ", section_title, "\n\n")
    for (var in vars) {
      if (var %in% names(measure_data)) {
        section <- paste0(section, format_measure(var, measure_data[[var]]))
      }
    }
    return(section)
  }

  # Generate sections
  baseline_section <- generate_section(baseline_vars, "Baseline Measures")
  exposure_section <- generate_section(exposure_var, "Exposure Measure")
  outcome_section <- generate_section(outcome_vars, "Outcome Measures")

  # Generate keywords section only if print_keywords is TRUE
  keywords_section <- ""
  if (print_keywords) {
    generate_keywords_section <- function(vars) {
      all_keywords <- unique(unlist(lapply(vars, function(var) {
        if (var %in% names(measure_data) &&
            "keywords" %in% names(measure_data[[var]])) {
          return(measure_data[[var]]$keywords)
        }
        return(NULL)
      })))

      if (length(all_keywords) > 0) {
        return(paste0(
          "### Keywords Used\n\n",
          paste(sort(all_keywords), collapse = ", "),
          "\n\n"
        ))
      } else {
        return("")
      }
    }

    keywords_section <- generate_keywords_section(all_vars)
  }

  # Combine all sections
  full_appendix <- paste0(
    "## Appendix: Measures\n\n",
    baseline_section,
    exposure_section,
    outcome_section,
    keywords_section
  )

  return(full_appendix)
}
