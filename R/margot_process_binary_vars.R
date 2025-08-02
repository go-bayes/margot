#' Process Binary Variables in a Data Frame
#'
#' This function identifies binary variables (both factors and numeric),
#' converts them to 0/1 format, and renames them. It allows for exceptions
#' to be specified, and ignores variables whose names already end with the
#' specified suffix.
#'
#' @param data A data frame to process.
#' @param exceptions A character vector of column names to exclude from processing.
#' @param suffix A string to append to renamed binary variables. Default is "_binary".
#'
#' @return A data frame with processed binary variables.
#'
#' @examples
#' df <- data.frame(
#'   a = factor(c("yes", "no", "yes")),
#'   b = c(1, 0, 1),
#'   c = c("apple", "banana", "apple"),
#'   d = factor(c("true", "false", "true")),
#'   e_binary = c(0, 1, 0)
#' )
#' processed_df <- margot_process_binary_vars(df, exceptions = "c")
#'
#' @importFrom dplyr mutate rename_with all_of
#' @importFrom cli cli_h1 cli_h2 cli_text cli_alert_info cli_alert_warning cli_alert_success cli_alert_danger cli_rule
#' @importFrom knitr kable
#' @export
margot_process_binary_vars <- function(data, exceptions = character(0), suffix = "_binary") {
  # Ensure required packages are loaded
  require(dplyr)
  require(cli)
  require(knitr)

  # Print function header
  cli::cli_h1("Processing Binary Variables")

  # Input validation
  if (!is.data.frame(data)) {
    stop(cli::cli_alert_danger("Error: Input must be a data frame."))
  }
  if (!is.character(exceptions)) {
    stop(cli::cli_alert_danger("Error: Exceptions must be a character vector."))
  }

  # Identify variables that end with the suffix; these will be ignored.
  ignored_vars <- names(data)[endsWith(names(data), suffix)]
  if (length(ignored_vars) > 0) {
    cli::cli_alert_info("Ignoring variables with suffix '{suffix}': {paste(ignored_vars, collapse = ', ')}")
  }

  # Report initial data summary
  cli::cli_h2("Initial Data Summary")
  cli::cli_text("Total variables: {ncol(data)}")
  cli::cli_text("Total observations: {nrow(data)}")
  cli::cli_text("Exceptions specified: {length(exceptions)}")

  # Helper functions
  is_binary <- function(x) {
    unique_vals <- unique(x[!is.na(x)])
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  }
  is_binary_factor <- function(x) {
    is.factor(x) && length(levels(x)) == 2
  }

  # Determine columns to convert: binary factors excluding exceptions and ignored variables.
  cols_to_convert <- names(data)[sapply(data, is_binary_factor) &
    !(names(data) %in% c(exceptions, ignored_vars))]
  if (length(cols_to_convert) > 0) {
    data <- data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_convert), ~ as.numeric(. == levels(.)[2])))
  }

  # Identify binary numeric variables (excluding exceptions and ignored)
  binary_vars <- names(data)[sapply(data, is_binary) &
    !(names(data) %in% c(exceptions, ignored_vars))]

  # Report on identified binary variables
  cli::cli_h2("Binary Variables Identified")
  cli::cli_text("Total binary variables: {length(binary_vars)}")
  if (length(binary_vars) > 0) {
    cli::cli_text("List of binary variables:")
    print(knitr::kable(data.frame(Variable = binary_vars), format = "pipe"))
  } else {
    cli::cli_alert_warning("No binary variables found.")
  }

  # Rename binary variables
  if (length(binary_vars) > 0) {
    data <- data %>%
      dplyr::rename_with(~ paste0(., suffix), dplyr::all_of(binary_vars))

    cli::cli_h2("Variable Renaming")
    rename_df <- data.frame(
      Original = binary_vars,
      New = paste0(binary_vars, suffix)
    )
    print(knitr::kable(rename_df, format = "pipe"))
  } else {
    cli::cli_alert_warning("No variables renamed.")
  }

  # Final summary
  cli::cli_h2("Final Data Summary")
  cli::cli_text("Total variables: {ncol(data)}")
  cli::cli_text("Variables processed: {length(binary_vars)}")
  cli::cli_text("Variables excluded (exceptions and ignored): {length(c(exceptions, ignored_vars))}")

  cli::cli_alert_success("Binary variable processing completed successfully!")
  cli::cli_rule()

  return(data)
}
# margot_process_binary_vars <- function(data, exceptions = character(0), suffix = "_binary") {
#   # Ensure required packages are loaded
#   require(dplyr)
#   require(crayon)
#   require(cli)
#   require(knitr)
#
#   # Print function header
#   cat(crayon::blue$bold("\n=== Processing Binary Variables ===\n"))
#
#   # Input validation
#   if (!is.data.frame(data)) {
#     stop(crayon::red$bold("Error: Input must be a data frame."))
#   }
#   if (!is.character(exceptions)) {
#     stop(crayon::red$bold("Error: Exceptions must be a character vector."))
#   }
#
#   # Helper functions
#   is_binary <- function(x) {
#     unique_vals <- unique(x[!is.na(x)])
#     length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
#   }
#   is_binary_factor <- function(x) {
#     is.factor(x) && length(levels(x)) == 2
#   }
#
#   # Report initial data summary
#   cat(crayon::blue$bold("\nInitial Data Summary:\n"))
#   cat(paste("  Total variables:", crayon::green(ncol(data)), "\n"))
#   cat(paste("  Total observations:", crayon::green(nrow(data)), "\n"))
#   cat(paste("  Exceptions specified:", crayon::yellow(length(exceptions)), "\n"))
#
#   # Convert binary factors to 0/1
#   data_converted <- data %>%
#     mutate(across(where(is_binary_factor) & !any_of(exceptions),
#                   ~ as.numeric(. == levels(.)[2])))
#
#   # Identify binary variables (now all numeric)
#   binary_vars <- data_converted %>%
#     select(where(is_binary) & !any_of(exceptions)) %>%
#     names()
#
#   # Report on identified binary variables
#   cat(crayon::blue$bold("\nBinary Variables Identified:\n"))
#   cat(paste("  Total binary variables:", crayon::green(length(binary_vars)), "\n"))
#   if (length(binary_vars) > 0) {
#     cat("  List of binary variables:\n")
#     print(knitr::kable(data.frame(Variable = binary_vars), format = "pipe"))
#   } else {
#     cat(crayon::yellow$bold("  No binary variables found.\n"))
#   }
#
#   # Rename binary variables
#   data_renamed <- data_converted %>%
#     rename_with(~ paste0(., suffix), all_of(binary_vars))
#
#   # Report on renamed variables
#   cat(crayon::blue$bold("\nVariable Renaming:\n"))
#   if (length(binary_vars) > 0) {
#     rename_df <- data.frame(
#       Original = binary_vars,
#       New = paste0(binary_vars, suffix)
#     )
#     print(knitr::kable(rename_df, format = "pipe"))
#   } else {
#     cat(crayon::yellow$bold("  No variables renamed.\n"))
#   }
#
#   # Final summary
#   cat(crayon::blue$bold("\nFinal Data Summary:\n"))
#   cat(paste("  Total variables:", crayon::green(ncol(data_renamed)), "\n"))
#   cat(paste("  Variables processed:", crayon::green(length(binary_vars)), "\n"))
#   cat(paste("  Variables excluded:", crayon::yellow(length(exceptions)), "\n"))
#
#   cat(crayon::green$bold("\nBinary variable processing completed successfully!\n"))
#   cat(crayon::blue$bold(cli::rule(line = "=")))
#
#   return(data_renamed)
# }
