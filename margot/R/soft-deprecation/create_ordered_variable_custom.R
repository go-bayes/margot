#' @title create_ordered_variable_custom (Deprecated)
#' @description This function is deprecated. Please use `create_ordered_variable()` instead.
#' Create Ordered Variable with Custom Breaks and Auto-generated Labels
#'
#' This function creates an ordered categorical variable in a dataframe based on user-specified breaks.
#' It automatically generates labels reflecting the break points and handles various edge cases.
#'
#' @param df A data frame containing the variable to be transformed.
#' @param var_name The name of the numeric variable within the data frame to be converted into
#'        an ordered factor. This variable should ideally be continuous.
#' @param breaks A numeric vector specifying the breakpoints between intervals. The breakpoints
#'        should cover the entire range of the variable.
#' @param include_lowest Logical, should the lowest value be included in the first interval? Default is TRUE.
#' @param right Logical, should intervals be closed on the right (and open on the left)? Default is TRUE.
#' @param cutpoint_inclusive A character string specifying whether cutpoints should be included
#'        in the lower or upper category. Must be either "lower" or "upper". Default is "upper".
#' @param inf_label A string to use for infinity in labels. Default is "Inf".
#' @param neg_inf_label A string to use for negative infinity in labels. Default is "-Inf".
#'
#' @return Returns the data frame with an additional column representing the ordered factor variable.
#'         The new column is named by combining the original variable name and the suffix "_cat".
#'
#' @examples
#' df <- data.frame(x = runif(100, 0, 10))
#' df_updated <- create_ordered_variable_custom(df, "x", c(0, 2, 5, 10))
#'
#' @keywords internal
#' @importFrom lifecycle deprecate_warn
create_ordered_variable_custom <- function(df, var_name, breaks,
                                           include_lowest = TRUE,
                                           right = TRUE,
                                           cutpoint_inclusive = "upper",
                                           inf_label = "Inf",
                                           neg_inf_label = "-Inf") {

  # warning
  lifecycle::deprecate_warn("1.0.0", "create_ordered_variable_custom()", "create_ordered_variable()")

  # Check if breaks are provided and in ascending order
  if (is.null(breaks) || !is.numeric(breaks) || !all(diff(breaks) > 0)) {
    stop("Please provide a valid numeric vector of breaks in ascending order.")
  }

  # Check if the variable exists in the dataframe
  if (!var_name %in% names(df)) {
    stop(paste("Variable", var_name, "not found in the dataframe."))
  }

  # Check cutpoint_inclusive
  if (!cutpoint_inclusive %in% c("lower", "upper")) {
    stop("Invalid cutpoint_inclusive. Must be either 'lower' or 'upper'.")
  }

  # Generate labels based on breaks
  labels <- vapply(seq_len(length(breaks) - 1), function(i) {
    left <- if (i == 1 && !include_lowest) "(" else "["
    right <- if (i == length(breaks) - 1 && right) "]" else ")"
    left_val <- if (i == 1 && breaks[i] == -Inf) neg_inf_label else sprintf("%.1f", breaks[i])
    right_val <- if (i == length(breaks) - 1 && breaks[i + 1] == Inf) inf_label else sprintf("%.1f", breaks[i + 1])

    if (cutpoint_inclusive == "lower") {
      paste0(left, left_val, ",", right_val, right)
    } else {
      paste0(left, left_val, ",", right_val, right)
    }
  }, character(1))

  # Create the ordered factor variable
  new_col_name <- paste0(var_name, "_cat")
  df[[new_col_name]] <- cut(
    df[[var_name]],
    breaks = breaks,
    labels = labels,
    include.lowest = include_lowest,
    right = right,
    ordered_result = TRUE
  )

  # Print summary of the new variable
  cat("Summary of", new_col_name, ":\n")
  print(table(df[[new_col_name]], useNA = "ifany"))

  # Return the updated data frame
  return(df)
}
