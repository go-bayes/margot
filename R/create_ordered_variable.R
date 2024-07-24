#' Create Ordered Variable Based on Quantile Breaks with Informative Labels
#'
#' @param df A data frame containing the variable to be divided into quantiles.
#' @param var_name The name of the variable within the data frame to divide into quantiles.
#' @param n_divisions The number of quantile divisions to create.
#'
#' @return The input data frame with an additional column representing the ordered factor variable.
#'
#' @importFrom stats quantile
#' @export
create_ordered_variable <- function(df, var_name, n_divisions = NULL) {
  if (is.null(n_divisions)) stop("Please specify the number of divisions.")
  if (!var_name %in% names(df)) stop(paste("Variable", var_name, "not found in the dataframe."))

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
  quantile_breaks <- unique(quantile(var_clean, probs = probs, na.rm = TRUE, type = 1))

  # ensure we have the correct number of unique breaks
  if (length(quantile_breaks) < n_divisions + 1) {
    epsilon <- diff(range(var_clean)) * .Machine$double.eps
    quantile_breaks <- unique(c(quantile_breaks, seq(max(quantile_breaks) + epsilon,
                                                     by = epsilon,
                                                     length.out = n_divisions + 1 - length(quantile_breaks))))
  }

  # create informative labels based on cut points
  labels <- vapply(seq_len(n_divisions), function(x) {
    if (x == n_divisions) {
      sprintf("[%.1f,Inf]", quantile_breaks[x])
    } else {
      sprintf("[%.1f,%.1f)", quantile_breaks[x], quantile_breaks[x + 1])
    }
  }, character(1))

  # use findInterval for categorisation
  new_col_name <- paste0(var_name, "_cat")
  df[[new_col_name]] <- factor(labels[findInterval(var, quantile_breaks, all.inside = TRUE)],
                               levels = labels,
                               ordered = TRUE)

  # print summary of the new variable
  cat("Summary of", new_col_name, ":\n")
  print(table(df[[new_col_name]], useNA = "ifany"))

  return(df)
}
