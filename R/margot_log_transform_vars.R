#' Log-transform Variables in a Data Frame
#'
#' This function applies a log(x + 1) transformation to specified variables in a data frame.
#' It handles NA values, allows for exceptions, and can be applied to variables with specific prefixes.
#'
#' @param data A data frame to process.
#' @param vars A character vector of variable names or a tidyselect helper (e.g., starts_with("hours_")).
#' @param exceptions A character vector of variable names to exclude from transformation.
#' @param prefix A string to prepend to the names of transformed variables. Default is "log_".
#' @param keep_original Logical. If TRUE, keeps both original and transformed variables. If FALSE, replaces original variables. Default is TRUE.
#'
#' @return A data frame with log-transformed variables.
#'
#' @examples
#' df <- data.frame(
#'   hours_work = c(0, 1, 5, NA),
#'   hours_sleep = c(6, 7, 8, 9),
#'   income = c(1000, 2000, 3000, 4000)
#' )
#' transformed_df <- margot_log_transform_vars(df,
#'                                             vars = c(starts_with("hours_"), "income"),
#'                                             exceptions = "hours_work")
#'
#' @importFrom dplyr mutate across select if_else all_of
#' @importFrom crayon green red yellow blue bold
#' @importFrom cli rule
#' @importFrom knitr kable
#' @export
margot_log_transform_vars <- function(data, vars, exceptions = character(0), prefix = "log_", keep_original = TRUE) {
  # Ensure required packages are loaded
  require(dplyr)
  require(crayon)
  require(cli)
  require(knitr)

  # Print function header
  cat(crayon::blue$bold("\n=== Log-transforming Variables ===\n"))

  # Input validation
  if (!is.data.frame(data)) {
    stop(crayon::red$bold("Error: Input must be a data frame."))
  }
  if (!is.character(exceptions)) {
    stop(crayon::red$bold("Error: Exceptions must be a character vector."))
  }

  # Report initial data summary
  cat(crayon::blue$bold("\nInitial Data Summary:\n"))
  cat(paste("  Total variables:", crayon::green(ncol(data)), "\n"))
  cat(paste("  Total observations:", crayon::green(nrow(data)), "\n"))
  cat(paste("  Exceptions specified:", crayon::yellow(length(exceptions)), "\n"))

  # Identify vars to transform
  vars_to_transform <- data %>%
    select({{ vars }}) %>%
    names()

  # Remove exceptions from vars_to_transform
  vars_to_transform <- setdiff(vars_to_transform, exceptions)

  # Report on variables to be transformed
  cat(crayon::blue$bold("\nVariables to be Transformed:\n"))
  cat(paste("  Total variables to transform:", crayon::green(length(vars_to_transform)), "\n"))
  if (length(vars_to_transform) > 0) {
    cat("  List of variables to transform:\n")
    print(knitr::kable(data.frame(Variable = vars_to_transform), format = "pipe"))
  } else {
    cat(crayon::yellow$bold("  No variables to transform.\n"))
  }

  # Apply log transformation
  data_transformed <- data %>%
    mutate(across(
      .cols = all_of(vars_to_transform),
      .fns = ~ if_else(!is.na(.), log(. + 1), NA_real_),
      .names = "{prefix}{.col}"
    ))

  # Report on transformed variables
  cat(crayon::blue$bold("\nTransformed Variables:\n"))
  if (length(vars_to_transform) > 0) {
    transform_df <- data.frame(
      Original = vars_to_transform,
      Transformed = paste0(prefix, vars_to_transform)
    )
    print(knitr::kable(transform_df, format = "pipe"))
  } else {
    cat(crayon::yellow$bold("  No variables were transformed.\n"))
  }

  # Remove original variables if keep_original is FALSE
  if (!keep_original) {
    data_transformed <- data_transformed %>%
      select(-all_of(vars_to_transform))
    cat(crayon::yellow$bold("\nOriginal variables removed as keep_original = FALSE\n"))
  } else {
    cat(crayon::green$bold("\nOriginal variables kept as keep_original = TRUE\n"))
  }

  # Final summary
  cat(crayon::blue$bold("\nFinal Data Summary:\n"))
  cat(paste("  Total variables:", crayon::green(ncol(data_transformed)), "\n"))
  cat(paste("  Variables transformed:", crayon::green(length(vars_to_transform)), "\n"))
  cat(paste("  Variables excluded:", crayon::yellow(length(exceptions)), "\n"))

  cat(crayon::green$bold("\nLog transformation completed successfully!\n"))
  cat(crayon::blue$bold(cli::rule(line = "=")))

  return(data_transformed)
}
# margot_log_transform_vars <- function(data, vars, exceptions = character(0), prefix = "log_", keep_original = FALSE) {
#   # input validation
#   if (!is.data.frame(data)) {
#     stop("Input must be a data frame.")
#   }
#   if (!is.character(exceptions)) {
#     stop("Exceptions must be a character vector.")
#   }
#
#   # identify vars to transform
#   vars_to_transform <- data %>%
#     select({{ vars }}) %>%
#     names()
#
#   # remove exceptions from vars_to_transform
#   vars_to_transform <- setdiff(vars_to_transform, exceptions)
#
#   # apply log transformation
#   data_transformed <- data %>%
#     mutate(across(
#       .cols = all_of(vars_to_transform),
#       .fns = ~ if_else(!is.na(.), log(. + 1), NA_real_),
#       .names = "{prefix}{.col}"
#     ))
#
#   # remove original variables if keep_original is FALSE (default)
#   if (!keep_original) {
#     data_transformed <- data_transformed %>%
#       select(-all_of(vars_to_transform))
#   }
#
#   return(data_transformed)
# }
