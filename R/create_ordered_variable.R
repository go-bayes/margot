#' Create Ordered Variable Based on Quantile Breaks
#'
#' This function takes a data frame and a specified variable name and divides the variable
#' into ordered categories based on quantile breaks. The number of quantile divisions must
#' be specified. The function is designed to handle non-unique breaks by adjusting them
#' appropriately. It returns the data frame with a new ordered factor variable.
#'
#' @param df A data frame containing the variable to be divided into quantiles.
#' @param var_name The name of the variable within the data frame to divide into quantiles.
#'           For example, in the `df_nz` dataset, you might use "perfectionism".
#' @param n_divisions The number of quantile divisions to create. This must be a positive integer.
#'           If NULL or not specified, the function will stop and ask the user to provide this parameter.
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
#' @importFrom stats quantile
create_ordered_variable <- function(df, var_name, n_divisions = NULL) {
  if (is.null(n_divisions)) {
    stop("Please specify the number of divisions.")
  }

  # Calculate initial quantile breaks
  quantile_breaks <- quantile(df[[var_name]], probs = seq(0, 1, length.out = n_divisions + 1), na.rm = TRUE)

  # Check for unique breaks and adjust if necessary
  if (length(unique(quantile_breaks)) != length(quantile_breaks)) {
    warning("Quantile breaks are not unique; adjusting to handle ties or insufficient unique values.")
    quantile_breaks <- sort(unique(quantile_breaks))

    # Adding small adjustments to ensure there are enough breaks
    while (length(quantile_breaks) < n_divisions + 1) {
      # Extending quantile breaks by adding small increments to the max value
      quantile_breaks <- c(quantile_breaks, max(quantile_breaks) + diff(range(quantile_breaks))/100)
    }
  }

  # Create labels for each segment based on the actual number of breaks
  cut_labels <- paste0("tile_", seq_len(length(quantile_breaks) - 1))

  # Assign the ordered factor to the dataframe
  df[[paste0(var_name, "_", n_divisions, "tile")]] <- cut(
    df[[var_name]],
    breaks = quantile_breaks,
    labels = cut_labels,
    ordered_result = TRUE,
    include.lowest = TRUE
  )

  return(df)
}
