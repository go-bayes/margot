#' Process Binary Variables in a Data Frame
#'
#' This function identifies binary variables (both factors and numeric),
#' converts them to 0/1 format, and renames them. It allows for exceptions
#' to be specified.
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
#'   d = factor(c("true", "false", "true"))
#' )
#' processed_df <- margot_process_binary_vars(df, exceptions = "c")
#'
#' @importFrom dplyr mutate across rename_with any_of all_of
#' @importFrom crayon green red yellow blue bold
#' @importFrom cli rule
#' @importFrom knitr kable
#' @export
margot_process_binary_vars <- function(data, exceptions = character(0), suffix = "_binary") {
  # Ensure required packages are loaded
  require(dplyr)
  require(crayon)
  require(cli)
  require(knitr)

  # Print function header
  cat(crayon::blue$bold("\n=== Processing Binary Variables ===\n"))

  # Input validation
  if (!is.data.frame(data)) {
    stop(crayon::red$bold("Error: Input must be a data frame."))
  }
  if (!is.character(exceptions)) {
    stop(crayon::red$bold("Error: Exceptions must be a character vector."))
  }

  # Helper functions
  is_binary <- function(x) {
    unique_vals <- unique(x[!is.na(x)])
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  }
  is_binary_factor <- function(x) {
    is.factor(x) && length(levels(x)) == 2
  }

  # Report initial data summary
  cat(crayon::blue$bold("\nInitial Data Summary:\n"))
  cat(paste("  Total variables:", crayon::green(ncol(data)), "\n"))
  cat(paste("  Total observations:", crayon::green(nrow(data)), "\n"))
  cat(paste("  Exceptions specified:", crayon::yellow(length(exceptions)), "\n"))

  # Convert binary factors to 0/1
  data_converted <- data %>%
    mutate(across(where(is_binary_factor) & !any_of(exceptions),
                  ~ as.numeric(. == levels(.)[2])))

  # Identify binary variables (now all numeric)
  binary_vars <- data_converted %>%
    select(where(is_binary) & !any_of(exceptions)) %>%
    names()

  # Report on identified binary variables
  cat(crayon::blue$bold("\nBinary Variables Identified:\n"))
  cat(paste("  Total binary variables:", crayon::green(length(binary_vars)), "\n"))
  if (length(binary_vars) > 0) {
    cat("  List of binary variables:\n")
    print(knitr::kable(data.frame(Variable = binary_vars), format = "pipe"))
  } else {
    cat(crayon::yellow$bold("  No binary variables found.\n"))
  }

  # Rename binary variables
  data_renamed <- data_converted %>%
    rename_with(~ paste0(., suffix), all_of(binary_vars))

  # Report on renamed variables
  cat(crayon::blue$bold("\nVariable Renaming:\n"))
  if (length(binary_vars) > 0) {
    rename_df <- data.frame(
      Original = binary_vars,
      New = paste0(binary_vars, suffix)
    )
    print(knitr::kable(rename_df, format = "pipe"))
  } else {
    cat(crayon::yellow$bold("  No variables renamed.\n"))
  }

  # Final summary
  cat(crayon::blue$bold("\nFinal Data Summary:\n"))
  cat(paste("  Total variables:", crayon::green(ncol(data_renamed)), "\n"))
  cat(paste("  Variables processed:", crayon::green(length(binary_vars)), "\n"))
  cat(paste("  Variables excluded:", crayon::yellow(length(exceptions)), "\n"))

  cat(crayon::green$bold("\nBinary variable processing completed successfully!\n"))
  cat(crayon::blue$bold(cli::rule(line = "=")))

  return(data_renamed)
}
# margot_process_binary_vars <- function(data, exceptions = character(0), suffix = "_binary") {
#   # input validation
#   if (!is.data.frame(data)) {
#     stop("Input must be a data frame.")
#   }
#   if (!is.character(exceptions)) {
#     stop("Exceptions must be a character vector.")
#   }
#
#   # helper functions
#   is_binary <- function(x) {
#     unique_vals <- unique(x[!is.na(x)])
#     length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
#   }
#
#   is_binary_factor <- function(x) {
#     is.factor(x) && length(levels(x)) == 2
#   }
#
#   # convert binary factors to 0/1
#   data_converted <- data %>%
#     mutate(across(where(is_binary_factor) & !any_of(exceptions),
#                   ~ as.numeric(. == levels(.)[2])))
#
#   # identify binary variables (now all numeric)
#   binary_vars <- data_converted %>%
#     select(where(is_binary) & !any_of(exceptions)) %>%
#     names()
#
#   # rename binary variables
#   data_renamed <- data_converted %>%
#     rename_with(~ paste0(., suffix), all_of(binary_vars))
#
#   return(data_renamed)
# }
