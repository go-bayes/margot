#' Create Ordered Variable Based on Quantile Breaks or Custom Breaks with Informative Labels
#'
#' This function divides a numeric variable into categories based on either quantile breaks
#' or custom-specified breaks, and creates an ordered factor variable with informative labels.
#'
#' @param df A data frame containing the variable to be divided into categories.
#' @param var_name The name of the variable within the data frame to divide into categories.
#' @param n_divisions The number of quantile divisions to create. Required if custom_breaks is not provided.
#' @param cutpoint_inclusive A character string specifying whether cutpoints should be included
#'        in the lower or upper category. Must be either "lower" or "upper". Default is "upper".
#' @param ties.method A character string specifying how ties should be handled when calculating quantiles.
#'        Must be one of "first", "last", "random", "ordered", or "average".
#'        If NULL (default), it will be set to "last" if cutpoint_inclusive is "upper",
#'        and "first" if cutpoint_inclusive is "lower".
#' @param custom_breaks A numeric vector of break points to use for categorization. If provided,
#'        this overrides the quantile-based division specified by n_divisions.
#'
#' @return The input data frame with an additional column representing the ordered factor variable.
#'         The new column name will be the original variable name with "_cat" appended.
#'
#' @details
#' The function creates categories based on either quantile breaks or custom-specified breaks.
#' It then categorizes the values into these breaks, creating an ordered factor variable.
#'
#' When using quantile breaks (custom_breaks is NULL):
#' - The function creates quantile breaks based on the specified number of divisions.
#' - If the number of unique values in the variable is less than the requested number of divisions,
#'   the function will adjust the number of divisions and issue a warning.
#'
#' When using custom breaks:
#' - If the lowest or highest values in the data are not included in the custom breaks,
#'   they will be automatically added and a warning will be issued.
#' - Custom breaks are sorted and de-duplicated before use.
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
#' - Using "average" for `ties.method` will generate a warning due to potential inconsistencies.
#' - When using custom breaks, ensure that the breaks cover the entire range of the data
#'   to avoid unexpected behavior.
#'
#' @examples
#' df <- data.frame(x = rnorm(100))
#'
#' # Using quantile breaks
#' result1 <- create_ordered_variable(df, "x", n_divisions = 4)
#'
#' # Using custom breaks
#' result2 <- create_ordered_variable(df, "x", custom_breaks = c(-2, -1, 0, 1, 2))
#'
#' # Explicitly setting cutpoint_inclusive and ties.method
#' result3 <- create_ordered_variable(df, "x", n_divisions = 3,
#'                                    cutpoint_inclusive = "lower",
#'                                    ties.method = "first")
#'
#' @importFrom stats quantile
#' @export
create_ordered_variable <- function(df, var_name, n_divisions = NULL,
                                    cutpoint_inclusive = "upper",
                                    ties.method = NULL,
                                    custom_breaks = NULL) {
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
  min_val <- min(var_clean, na.rm = TRUE)
  max_val <- max(var_clean, na.rm = TRUE)

  if (!is.null(custom_breaks)) {
    if (!is.numeric(custom_breaks) || length(custom_breaks) < 2) {
      stop("custom_breaks must be a numeric vector with at least 2 elements.")
    }

    custom_breaks <- sort(unique(custom_breaks))

    if (min(custom_breaks) > min_val) {
      warning("Lowest break is higher than the minimum value. Adding minimum value to breaks.")
      custom_breaks <- c(min_val, custom_breaks)
    }

    if (max(custom_breaks) < max_val) {
      warning("Highest break is lower than the maximum value. Adding maximum value to breaks.")
      custom_breaks <- c(custom_breaks, max_val)
    }

    quantile_breaks <- custom_breaks
    n_divisions <- length(quantile_breaks) - 1
  } else {
    if (is.null(n_divisions)) stop("Please specify the number of divisions or provide custom breaks.")

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
    quantile_breaks[1] <- min_val
    quantile_breaks[length(quantile_breaks)] <- max_val

    # ensure we have the correct number of unique breaks
    if (length(quantile_breaks) < n_divisions + 1) {
      epsilon <- diff(range(var_clean)) * .Machine$double.eps
      quantile_breaks <- unique(c(quantile_breaks, max_val + epsilon))
      quantile_breaks <- quantile_breaks[1:(n_divisions + 1)]  # Ensure exactly n_divisions + 1 breaks
    }
  }

  # create informative labels based on cut points and cutpoint_inclusive
  labels <- vapply(seq_len(n_divisions), function(x) {
    if (cutpoint_inclusive == "lower") {
      if (x == n_divisions) {
        sprintf("[%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
      } else {
        sprintf("[%.1f,%.1f)", quantile_breaks[x], quantile_breaks[x + 1])
      }
    } else {  # cutpoint_inclusive == "upper"
      if (x == 1) {
        sprintf("[%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
      } else {
        sprintf("(%.1f,%.1f]", quantile_breaks[x], quantile_breaks[x + 1])
      }
    }
  }, character(1))

  # use cut for categorisation, adjusting for cutpoint inclusivity
  new_col_name <- paste0(var_name, "_cat")
  if (cutpoint_inclusive == "lower") {
    df[[new_col_name]] <- cut(var, breaks = quantile_breaks,
                              labels = labels, include.lowest = TRUE, right = FALSE)
  } else {  # cutpoint_inclusive == "upper"
    df[[new_col_name]] <- cut(var, breaks = quantile_breaks,
                              labels = labels, include.lowest = TRUE, right = TRUE)
  }
  # print summary of the new variable
  cat("Summary of", new_col_name, ":\n")
  print(table(df[[new_col_name]], useNA = "ifany"))
  return(df)
}
