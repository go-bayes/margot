#' Manage Boilerplate Measures (DEPRECATED, use `boilerplate` package: <https://go-bayes.github.io/boilerplate/>)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `boilerplate::boilerplate_manage_measures()` instead.
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
#' # boilerplate::boilerplate_manage_measures(...)
#' }
#'
#' @import lifecycle
#' @import cli
#'
#' @export
margot_create_bibliography <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.23",
    what = "manager_create_bibliography()",
    with = "boilerplate::boilerplate_report_measures()"
  )

  cli::cli_alert_warning("This function is deprecated. Please use boilerplate::boilerplate_manage_measures() instead.")
  cli::cli_alert_info("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  cli::cli_alert_info("After installation, load the package with: library(boilerplate)")
}
#' #' Create a Bibliography of Measures
#' #'
#' #' This function generates a formatted bibliography of measures used in a study,
#' #' including details about questions and citations.
#' #'
#' #' @param all_vars A character vector of all variable names, or TRUE to use all variables
#' #'   from baseline_vars, exposure_var, and outcome_vars.
#' #' @param baseline_vars A character vector of baseline variable names.
#' #' @param exposure_var A character string of the exposure variable name.
#' #' @param outcome_vars A character vector of outcome variable names.
#' #' @param measure_data A list containing information about each measure.
#' #' @param custom_titles A named list of custom titles for variables (optional).
#' #' @param print_keywords Logical, whether to print keywords for each measure (default: FALSE).
#' #' @param print_waves Logical, whether to print wave information for each measure (default: FALSE).
#' #'
#' #' @return A character string containing the formatted bibliography.
#' #'
#' #' @examples
#' #' # Example 1: Using separate variable lists
#' #' baseline_vars <- c("age", "male_binary", "parent_binary")
#' #' exposure_var <- "political_conservative"
#' #' outcome_vars <- c("smoker_binary", "hlth_bmi", "log_hours_exercise",
#' #'                   "hlth_fatigue", "kessler_latent_anxiety",
#' #'                   "belong", "neighbourhood_community")
#' #'
#' #' # Assuming measure_data is a list with information about each variable
#' #' appendix_text_1 <- margot_create_bibliography(
#' #'   baseline_vars = baseline_vars,
#' #'   exposure_var = exposure_var,
#' #'   outcome_vars = outcome_vars,
#' #'   measure_data = measure_data
#' #' )
#' #'
#' #' # Example 2: Using all_vars
#' #' all_vars <- c(baseline_vars, exposure_var, outcome_vars)
#' #' appendix_text_2 <- margot_create_bibliography(
#' #'   all_vars = all_vars,
#' #'   measure_data = measure_data
#' #' )
#' #'
#' #' # Example 3: Using custom titles and printing keywords
#' #' custom_titles <- list(age = "Participant Age", male_binary = "Gender (Male)")
#' #' appendix_text_3 <- margot_create_bibliography(
#' #'   all_vars = all_vars,
#' #'   measure_data = measure_data,
#' #'   custom_titles = custom_titles,
#' #'   print_keywords = TRUE
#' #' )
#' #' @import cli
#' #' @export
#' margot_create_bibliography <- function(all_vars = NULL,
#'                                        baseline_vars = NULL,
#'                                        exposure_var = NULL,
#'                                        outcome_vars = NULL,
#'                                        measure_data,
#'                                        custom_titles = NULL,
#'                                        print_keywords = FALSE,
#'                                        print_waves = FALSE) {
#'
#'   # Generate section for variables
#'   generate_section <- function(vars, section_title) {
#'     section <- paste0(cli::col_magenta("### ", section_title, "\n\n"))
#'     for (var in vars) {
#'       section <- paste0(section, format_measure(var, measure_data[[var]]))
#'     }
#'     return(section)
#'   }
#'
#'   # Input validation and processing
#'   if (is.null(all_vars) && is.null(baseline_vars) && is.null(exposure_var) && is.null(outcome_vars)) {
#'     # If no variables are specified, use all variables from measure_data
#'     all_vars <- names(measure_data)
#'   } else if (!is.null(all_vars)) {
#'     if (is.logical(all_vars) && all_vars) {
#'       all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
#'     } else if (!is.character(all_vars)) {
#'       stop("all_vars must be either TRUE or a character vector")
#'     }
#'   } else if (is.null(baseline_vars) || is.null(exposure_var) || is.null(outcome_vars)) {
#'     stop("Either all_vars or all of baseline_vars, exposure_var, and outcome_vars must be provided")
#'   } else {
#'     all_vars <- unique(c(baseline_vars, exposure_var, outcome_vars))
#'   }
#'
#'   if (!is.list(measure_data)) {
#'     stop("measure_data must be a list")
#'   }
#'
#'   if (!is.null(custom_titles) && (!is.list(custom_titles) || is.null(names(custom_titles)))) {
#'     stop("custom_titles must be a named list or NULL")
#'   }
#'
#'   # Check if all variables are in measure_data
#'   missing_vars <- all_vars[!all_vars %in% names(measure_data)]
#'   if (length(missing_vars) > 0) {
#'     warning(paste("The following variables are not in measure_data:", paste(missing_vars, collapse = ", ")))
#'   }
#'
#'   format_measure <- function(var_name, measure_info) {
#'     if (is.null(measure_info)) {
#'       warning(paste("No information available for variable:", var_name))
#'       return(paste0("#### ", janitor::make_clean_names(var_name, case = "title"), "\n\nNo information available for this variable.\n\n"))
#'     }
#'
#'     if (!all(c("description", "reference") %in% names(measure_info))) {
#'       warning(paste("Measure info for", var_name, "is missing 'description' or 'reference'"))
#'       missing_fields <- setdiff(c("description", "reference"), names(measure_info))
#'       for (field in missing_fields) {
#'         measure_info[[field]] <- "Not provided"
#'       }
#'     }
#'
#'     # Use custom title if provided, otherwise use clean name
#'     if (!is.null(custom_titles) && var_name %in% names(custom_titles)) {
#'       title <- custom_titles[[var_name]]
#'     } else {
#'       title <- janitor::make_clean_names(var_name, case = "title")
#'     }
#'
#'     # Add variable type indicator, removing redundancy for binary variables
#'     if (endsWith(var_name, "_binary")) {
#'       title <- gsub(" Binary$", "", title)  # Remove " Binary" if it's at the end of the title
#'       title <- paste0(title, " (Binary)")
#'     } else if (endsWith(var_name, "_cat")) {
#'       title <- paste0(title, " (Categorical)")
#'     }
#'
#'     # Include wave information if available and print_waves is TRUE
#'     if (print_waves && "waves" %in% names(measure_info)) {
#'       title <- paste0(title, " (waves: ", measure_info$waves, ")")
#'     }
#'
#'     formatted_text <- paste0(cli::col_cyan("#### ", title, "\n\n"))
#'
#'     # Print items first
#'     if ("items" %in% names(measure_info)) {
#'       if (length(measure_info$items) == 1) {
#'         formatted_text <- paste0(
#'           formatted_text,
#'           cli::col_yellow("*", measure_info$items[1], "*\n\n")
#'         )
#'       } else {
#'         for (item in measure_info$items) {
#'           formatted_text <- paste0(
#'             formatted_text,
#'             cli::col_yellow("*", item, "*\n")
#'           )
#'         }
#'         formatted_text <- paste0(formatted_text, "\n")
#'       }
#'     }
#'     # Combine description and reference
#'     description <- trimws(measure_info$description)
#'     reference <- measure_info$reference
#'
#'     # Check if the reference is a 'string_is' type
#'     if (grepl("^string_is\\s+", reference)) {
#'       string_content <- sub("^string_is\\s+", "", reference)
#'       string_content <- gsub("^[\"']|[\"']$", "", string_content)
#'       description_with_ref <- paste0(description, ". ", string_content)
#'       if (!grepl("[.!?]$", description_with_ref)) {
#'         description_with_ref <- paste0(description_with_ref, ".")
#'       }
#'     } else {
#'       if (substr(description, nchar(description), nchar(description)) %in% c(".", "!", "?")) {
#'         description_with_ref <- paste0(
#'           substr(description, 1, nchar(description) - 1),
#'           " [@",
#'           reference,
#'           "]",
#'           substr(description, nchar(description), nchar(description))
#'         )
#'       } else {
#'         description_with_ref <- paste0(description, " [@", reference, "].")
#'       }
#'     }
#'
#'     formatted_text <- paste0(formatted_text, cli::col_yellow(description_with_ref), "\n\n")
#'
#'     if (print_keywords && "keywords" %in% names(measure_info) && length(measure_info$keywords) > 0) {
#'       formatted_text <- paste0(
#'         formatted_text,
#'         cli::col_green("Keywords: ", paste(measure_info$keywords, collapse = ", "), "\n\n")
#'       )
#'     }
#'
#'     return(formatted_text)
#'   }
#'
#'
#'   # Generate main section with all variables
#'   main_section <- generate_section(all_vars, "All Measures")
#'
#'   # Generate keywords section
#'   keywords_section <- ""
#'   if (print_keywords) {
#'     all_keywords <- unique(unlist(lapply(all_vars, function(var) {
#'       if (!is.null(measure_data[[var]]) && "keywords" %in% names(measure_data[[var]])) {
#'         return(measure_data[[var]]$keywords)
#'       }
#'       return(NULL)
#'     })))
#'
#'     if (length(all_keywords) > 0) {
#'       keywords_section <- paste0(
#'         cli::col_magenta("### Keywords Used\n\n"),
#'         cli::col_green(paste(sort(all_keywords), collapse = ", ")),
#'         "\n\n"
#'       )
#'     }
#'   }
#'
#'   # Combine all sections
#'   full_appendix <- paste0(
#'     cli::col_blue("## Appendix: Measures\n\n"),
#'     main_section,
#'     keywords_section
#'   )
#'
#'   # Print the colored output to the console
#'   cat(full_appendix)
#'
#'   # success message
#'   cli::cli_alert_success("Finished creating bibliography \U0001F44D")
#'
#'   # Return the uncoloured markdown text
#'   return(gsub("\033\\[[0-9;]*m", "", full_appendix))
#' }
