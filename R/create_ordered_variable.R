#' Create Ordered Variable Based on Quantile Breaks with Informative Labels
#'
#' This function divides a numeric variable into quantile-based categories and creates
#' an ordered factor variable with informative labels.
#'
#' @param df A data frame containing the variable to be divided into quantiles.
#' @param var_name The name of the variable within the data frame to divide into quantiles.
#' @param n_divisions The number of quantile divisions to create.
#' @param cutpoint_inclusive A character string specifying whether cutpoints should be included
#'        in the lower or upper category. Must be either "lower" or "upper". Default is "upper".
#' @param ties.method A character string specifying how ties should be handled.
#'        Must be one of "first", "last", "random", "ordered", or "average".
#'        If NULL (default), it will be set to "last" if cutpoint_inclusive is "upper",
#'        and "first" if cutpoint_inclusive is "lower".
#'
#' @return The input data frame with an additional column representing the ordered factor variable.
#'         The new column name will be the original variable name with "_cat" appended.
#'
#' @details
#' The function creates quantile breaks based on the specified number of divisions.
#' It then categorizes the values into these quantiles, creating an ordered factor variable.
#'
#' The `cutpoint_inclusive` parameter determines whether the cutpoint values themselves
#' are included in the lower or upper category. For example, if a cutpoint is 40:
#' - With "lower", the interval would be [40, 50)
#' - With "upper", the interval would be (30, 40]
#'
#' The `ties.method` parameter specifies how to handle tied values when calculating quantiles.
#' It's recommended to use a method consistent with `cutpoint_inclusive`:
#' - Use "first" or "last" to ensure consistency with "lower" or "upper" respectively.
#' - "average" is not recommended as it may lead to inconsistent results.
#'
#' @note
#' - If the number of unique values in the variable is less than the requested number of divisions,
#'   the function will adjust the number of divisions and issue a warning.
#' - Using "average" for `ties.method` will generate a warning due to potential inconsistencies.
#'
#' @examples
#' df <- data.frame(x = rnorm(100))
#' result <- create_ordered_variable(df, "x", n_divisions = 4)
#'
#' # Explicitly setting cutpoint_inclusive and ties.method
#' result2 <- create_ordered_variable(df, "x", n_divisions = 3,
#'                                    cutpoint_inclusive = "lower",
#'                                    ties.method = "first")
#'
#' @importFrom stats quantile
#' @export
create_ordered_variable <- function(df, var_name, n_divisions = NULL,
                                    cutpoint_inclusive = "upper",
                                    ties.method = NULL) {
  if (is.null(n_divisions)) stop("Please specify the number of divisions.")
  if (!var_name %in% names(df)) stop(paste("Variable", var_name, "not found in the dataframe."))
  if (!cutpoint_inclusive %in% c("lower", "upper")) {
    stop("Invalid cutpoint_inclusive. Must be either 'lower' or 'upper'.")
  }

  # Set default ties.method based on cutpoint_inclusive if not specified
  if (is.null(ties.method)) {
    ties.method <- if(cutpoint_inclusive == "lower") "first" else "last"
  }

  if (!ties.method %in% c("first", "last", "random", "ordered", "average")) {
    stop("Invalid ties.method. Must be one of 'first', 'last', 'random', 'ordered', or 'average'.")
  }

  if (ties.method == "average") {
    warning("Using 'average' for ties.method may lead to inconsistent results with cutpoint_inclusive setting.")
  }

  var <- df[[var_name]]
  var_clean <- var[!is.na(var)]
  n_unique <- length(unique(var_clean))
  if (n_unique < n_divisions) {
    warning(paste("The variable has fewer unique non-NA values than requested divisions.",
                  "Adjusting number of divisions."))
    n_divisions <- n_unique
  }

  # calculate quantile breaks
  probs <- seq(0, 1, length.out = n_divisions + 1)
  quantile_breaks <- unique(quantile(var_clean, probs = probs, na.rm = TRUE, type = 1, ties.method = ties.method))

  # Ensure minimum and maximum values are included
  quantile_breaks[1] <- min(var_clean, na.rm = TRUE)
  quantile_breaks[length(quantile_breaks)] <- max(var_clean, na.rm = TRUE)

  # ensure we have the correct number of unique breaks
  if (length(quantile_breaks) < n_divisions + 1) {
    epsilon <- diff(range(var_clean)) * .Machine$double.eps
    quantile_breaks <- unique(c(quantile_breaks, max(var_clean) + epsilon))
    quantile_breaks <- quantile_breaks[1:(n_divisions + 1)]  # Ensure exactly n_divisions + 1 breaks
  }

  # create informative labels based on cut points
  labels <- vapply(seq_len(n_divisions), function(x) {
    sprintf("[%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
  }, character(1))

  # use cut for categorisation, always including lowest and highest
  new_col_name <- paste0(var_name, "_cat")
  df[[new_col_name]] <- cut(var, breaks = quantile_breaks,
                            labels = labels, include.lowest = TRUE, right = TRUE)

  # print summary of the new variable
  cat("Summary of", new_col_name, ":\n")
  print(table(df[[new_col_name]], useNA = "ifany"))
  return(df)
}
