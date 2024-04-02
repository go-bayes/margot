#' Create a Custom Ordered Variable
#'
#' This function transforms a numeric variable into an ordered factor variable based on specified breaks
#' and labels. It is useful for categorizing a continuous variable into a discrete, ordered factor variable.
#' The function adds the newly created ordered factor variable to the dataframe with a modified name indicating
#' the transformation.
#'
#' @param df A dataframe containing the variable to be transformed.
#' @param var_name The name of the numeric variable in `df` to be transformed into an ordered factor.
#' @param breaks A numeric vector of values specifying the breakpoints between categories. The length of
#' `breaks` should be one more than the length of `labels`.
#' @param labels A character vector specifying the labels for the resulting categories. These labels correspond
#' to intervals defined by `breaks`.
#'
#' @return The input dataframe with an added column for the newly created ordered factor variable. This
#' column is named by appending "_coarsen" to the original variable name.
#'
#' @examples
#' # Assuming `dt` is a dataframe with a numeric variable "hours_work"
#' dt <- data.frame(hours_work = c(20, 35, 10, 45))
#' dt <- create_ordered_variable(dt, "hours_work", c(10, 30, 41, Inf), c("[10_30)", "[30_41)", "[41_up]"))
#' print(dt)
#'
#' @export
create_ordered_variable <-
  function(df, var_name, breaks, labels) {
    # check if breaks and labels are NULL
    if (is.null(breaks) || is.null(labels)) {
      stop("Please specify the breaks and labels.")
    }

    # check if breaks and labels have correct lengths
    if (length(breaks) != length(labels) + 1) {
      stop("The length of breaks should be one more than the length of labels.")
    }

    # create the ordered factor variable with the new labels
    df[[paste0(var_name, "_coarsen")]] <- cut(
      df[[var_name]],
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = TRUE,
      ordered_result = TRUE
    )

    # return the updated data frame
    return(df)
  }
