#' Select and Rename Columns Based on Criteria
#'
#' Selects columns from a base set that match specified baseline variables and renames
#' an outcome variable by changing its prefix. Useful for longitudinal data where
#' time-point prefixes need to be standardized or adjusted.
#'
#' @param names_base A character vector of column names from which to select.
#' @param baseline_vars A character vector of baseline variables to match in `names_base`.
#' @param outcome The name of the outcome variable whose prefix should be replaced.
#' @param from_prefix The original prefix of the outcome variable to be replaced,
#'   defaulting to "t2". The prefix should include any character immediately
#'   preceding the numeric value and underscore, e.g., "t2_".
#' @param to_prefix The new prefix to replace the original prefix in the outcome
#'   variable, defaulting to "t0". The prefix should be in the same format as
#'   `from_prefix`, including the character immediately preceding the numeric value
#'   and underscore, e.g., "t0_".
#'
#' @return A character vector of selected column names with the outcome variable name
#'   modified to reflect the new prefix.
#'
#' @examples
#' names_base <- c("t1_age", "t2_weight", "t3_height", "t2_outcome")
#' baseline_vars <- c("age", "weight")
#' outcome_var <- "t2_outcome"
#'
#' final_columns <- select_and_rename_cols(names_base, baseline_vars, outcome_var, "t2", "t0")
#' print(final_columns)
#'
#' @export
select_and_rename_cols <- function(names_base, baseline_vars, outcome, from_prefix = "t2", to_prefix = "t0") {
  # Input validation
  if (!is.character(names_base) || !is.character(baseline_vars) || !is.character(outcome)) {
    stop("All inputs must be character vectors.")
  }
  if (!is.character(from_prefix) || !is.character(to_prefix)) {
    stop("`from_prefix` and `to_prefix` must be character strings.")
  }
  if (length(outcome) != 1) {
    stop("`outcome` must be a single character string.")
  }
  if (!outcome %in% names_base) {
    stop("The outcome variable is not present in `names_base`.")
  }

  # Select columns that match with baseline_vars
  selected_cols <- grep(paste(baseline_vars, collapse = "|"), names_base, value = TRUE)

  if (length(selected_cols) == 0) {
    warning("No matching baseline variables found in `names_base`.")
  }

  # Detect and replace the specified prefix in the outcome variable
  outcome_renamed <- gsub(paste0("^", from_prefix, "_"), paste0(to_prefix, "_"), outcome)

  # Check if the renaming process might have generated a duplicate name in the final set
  if (outcome_renamed %in% selected_cols) {
    stop("Renaming `outcome` results in a duplicate name in the selected columns.")
  }

  # Append the renamed outcome to selected columns
  final_cols <- c(selected_cols, outcome_renamed)

  return(final_cols)
}

