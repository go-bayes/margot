#' Select and Rename Columns Based on Criteria
#'
#' Selects columns from a base set that match specified baseline variables and renames
#' an outcome variable by changing its prefix. Useful for longitudinal data where
#' time-point prefixes need to be standardised or adjusted.
#'
#' @param names_base a character vector of column names from which to select.
#' @param baseline_vars a character vector of baseline variables to match in `names_base`.
#' @param outcome the name of the outcome variable whose prefix should be replaced.
#' @param from_prefix the original prefix of the outcome variable to be replaced,
#'   defaulting to "t2". the prefix should include any character immediately
#'   preceding the numeric value and underscore, e.g., "t2_".
#' @param to_prefix the new prefix to replace the original prefix in the outcome
#'   variable, defaulting to "t0". the prefix should be in the same format as
#'   `from_prefix`, including the character immediately preceding the numeric value
#'   and underscore, e.g., "t0_".
#'
#' @return a character vector of selected column names with the outcome variable name
#'   modified to reflect the new prefix.
#'
#' @examples
#' names_base <- c("t0_age", "t0_weight", "t0_height", "t0_outcome")
#' baseline_vars <- c("age", "weight")
#' outcome_var <- "t2_outcome"
#'
#' final_columns <- select_and_rename_cols(names_base, baseline_vars, outcome_var, "t2", "t0")
#' print(final_columns)
#'
#' @export
select_and_rename_cols <- function(names_base, baseline_vars, outcome, from_prefix = "t2", to_prefix = "t0") {
  # input validation
  if (!is.character(names_base) || !is.character(baseline_vars) || !is.character(outcome)) {
    stop("all inputs must be character vectors.")
  }
  if (!is.character(from_prefix) || !is.character(to_prefix)) {
    stop("`from_prefix` and `to_prefix` must be character strings.")
  }
  if (length(outcome) != 1) {
    stop("`outcome` must be a single character string.")
  }

  # select columns that match with baseline_vars prefixed with "t0_"
  baseline_prefix <- "t0_"
  selected_cols <- names_base[grepl(paste(paste0(baseline_prefix, baseline_vars), collapse = "|"), names_base)]

  if (length(selected_cols) == 0) {
    warning("no matching baseline variables found in `names_base`.")
  }

  # detect and replace the specified prefix in the outcome variable
  outcome_renamed <- gsub(paste0("^", from_prefix, "_"), paste0(to_prefix, "_"), outcome)

  # remove any duplicates
  final_cols <- unique(c(selected_cols, outcome_renamed))

  return(final_cols)
}
