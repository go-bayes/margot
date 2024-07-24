#' Create Ordered Variable Based on Quantile Breaks
#'
#' This function takes a data frame and a specified variable name and divides the variable
#' into ordered categories based on quantile breaks. The number of quantile divisions must
#' be specified. The function is designed to handle non-unique breaks by adjusting them
#' appropriately. It returns the data frame with a new ordered factor variable.
#'
#' @param df A data frame containing the variable to be divided into quantiles.
#' @param var_name The name of the variable within the data frame to divide into quantiles.
#'        For example, in the `df_nz` dataset, you might use "perfectionism".
#' @param n_divisions The number of quantile divisions to create. This must be a positive integer.
#'        If NULL or not specified, the function will stop and ask the user to provide this parameter.
#'
#' @return The input data frame with an additional column representing the ordered factor
#'         variable. This new column is named by combining the original variable name, the
#'         number of divisions, and the suffix 'tile', e.g., 'perfectionism_5tile' for
#'         5 divisions of the 'perfectionism' variable.
#'
#' @examples
#' # Assuming df_nz is your dataset and 'perfectionism' is the column of interest:
#' df_updated <- create_ordered_variable(df_nz, "perfectionism", 5)
#'
#' @export
#' @importFrom data.table as.data.table setDT
#' @importFrom stats quantile
create_ordered_variable <- function(df, var_name, n_divisions = NULL) {
  if (is.null(n_divisions)) {
    stop("Please specify the number of divisions.")
  }

  # Convert to data.table for faster operations
  dt <- data.table::as.data.table(df)

  # Get unique values and their counts
  unique_vals <- sort(unique(dt[[var_name]]))
  n_unique <- length(unique_vals)

  if (n_unique < n_divisions) {
    warning(paste("The variable has fewer unique values (", n_unique,
                  ") than requested divisions (", n_divisions,
                  "). Adjusting number of divisions.", sep=""))
    n_divisions <- n_unique
  }

  # Calculate quantile breaks
  quantile_breaks <- stats::quantile(unique_vals, probs = seq(0, 1, length.out = n_divisions + 1),
                                     na.rm = TRUE, type = 1)

  # Ensure uniqueness of breaks
  quantile_breaks <- unique(quantile_breaks)

  # If we still don't have enough breaks, add small increments
  if (length(quantile_breaks) < n_divisions + 1) {
    message("Adjusting breaks to handle ties or insufficient unique values.")
    while (length(quantile_breaks) < n_divisions + 1) {
      quantile_breaks <- c(quantile_breaks, max(quantile_breaks) + .Machine$double.eps)
    }
  }

  # Create labels
  cut_labels <- paste0("tile_", seq_len(n_divisions))

  # Pre-allocate new column
  new_col_name <- paste0(var_name, "_", n_divisions, "tile")
  dt[, (new_col_name) := factor(rep(NA_character_, .N), levels = cut_labels, ordered = TRUE)]

  # Use data.table's fast operations for cutting
  dt[, (new_col_name) := {
    cuts <- findInterval(get(var_name), quantile_breaks, all.inside = TRUE)
    factor(cut_labels[cuts], levels = cut_labels, ordered = TRUE)
  }]

  return(as.data.frame(dt))
}
