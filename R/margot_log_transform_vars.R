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
#' transformed_df <- log_transform_vars(df,
#'                                      vars = c(starts_with("hours_"), "income"),
#'                                      exceptions = "hours_work")
#'
#' @export
margot_log_transform_vars <- function(data, vars, exceptions = character(0), prefix = "log_", keep_original = FALSE) {
  # input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!is.character(exceptions)) {
    stop("Exceptions must be a character vector.")
  }

  # identify vars to transform
  vars_to_transform <- data %>%
    select({{ vars }}) %>%
    names()

  # remove exceptions from vars_to_transform
  vars_to_transform <- setdiff(vars_to_transform, exceptions)

  # apply log transformation
  data_transformed <- data %>%
    mutate(across(
      .cols = all_of(vars_to_transform),
      .fns = ~ if_else(!is.na(.), log(. + 1), NA_real_),
      .names = "{prefix}{.col}"
    ))

  # remove original variables if keep_original is FALSE (default)
  if (!keep_original) {
    data_transformed <- data_transformed %>%
      select(-all_of(vars_to_transform))
  }

  return(data_transformed)
}
