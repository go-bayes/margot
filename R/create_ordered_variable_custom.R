#' Create Ordered Variable with Custom Breaks and Labels
#'
#' This function creates an ordered categorical variable in a dataframe based on user-specified breaks
#' and labels. It checks that the provided breaks and labels are correctly specified before creating
#' the factor variable. This is useful for creating a factor with custom defined intervals.
#'
#' @param df A data frame containing the variable to be transformed.
#' @param var_name The name of the numeric variable within the data frame to be converted into
#'        an ordered factor. This variable should ideally be continuous.
#' @param breaks A numeric vector specifying the breakpoints between intervals. The breakpoints
#'        should cover the entire range of the variable and should be one more than the number of labels.
#' @param labels A character vector specifying the labels for the intervals defined by breaks.
#'        The length of labels should be one less than the length of breaks.
#'
#' @return Returns the data frame with an additional column representing the ordered factor variable.
#'         The new column is named by combining the original variable name and the suffix "_coarsen".
#'
#' @examples
#' # Assuming df_nz is your dataset and 'hours_exercise' is the numeric column:
#' df_updated <- create_ordered_variable_custom(df_nz, "hours_exercise",
#'                                              c(1, 2, 7, Inf), c("[1_2)", "[2_7)", "[7_up]"))
#'
#' @export
create_ordered_variable_custom <-
  function(df, var_name, breaks, labels) {
    # Check if breaks and labels are provided
    if (is.null(breaks) || is.null(labels)) {
      stop("Please specify the breaks and labels.")
    }

    # Check if the lengths of breaks and labels are compatible
    if (length(breaks) != length(labels) + 1) {
      stop("The length of breaks should be one more than the length of labels.")
    }

    # Create the ordered factor variable with the new labels
    df[[paste0(var_name, "_coarsen")]] <- cut(
      df[[var_name]],
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = TRUE,
      ordered_result = TRUE
    )

    # Return the updated data frame
    return(df)
  }
