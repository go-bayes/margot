#' Filter Data Based on Exposure Variables
#'
#' This function filters a dataframe based on the levels of a single factor variable or arranges
#' the dataframe by identifier if only continuous variables are present.
#'
#' @param dat_wide Dataframe to filter.
#' @param exposure_vars Vector of names of exposure variables to consider.
#' @param sort_var Optional; the variable by which to sort the dataframes.
#' @return A list of dataframes filtered by the levels of the factor variable or arranged by identifier.
#'
#' @importFrom dplyr filter arrange
#' @export
margot_filter <- function(dat_wide, exposure_vars, sort_var = "id") {
  # Check for the presence of exposure variables in the dataframe
  for (exposure_var in exposure_vars) {
    if (!exposure_var %in% names(dat_wide)) {
      stop(paste("Exposure variable", exposure_var, "is not found in the dataframe."))
    }
  }

  # Distinguish between factor and continuous variables
  factor_exposure_vars <- exposure_vars[sapply(dat_wide[exposure_vars], is.factor)]
  continuous_exposure_vars <- setdiff(exposure_vars, factor_exposure_vars)

  # Enforce only one factor variable
  if (length(factor_exposure_vars) > 1) {
    stop("Only one factor exposure variable is allowed. Please adjust the input.")
  }

  # Prepare to store filtered dataframes
  list_filtered_df <- list()

  # Process factor variable
  if (length(factor_exposure_vars) == 1) {
    factor_levels <- levels(dat_wide[[factor_exposure_vars]])
    for (level in factor_levels) {
      filtered_df <- dat_wide %>%
        filter((!!rlang::sym(factor_exposure_vars)) == level) %>%
        arrange(!!rlang::sym(sort_var))

      list_filtered_df[[level]] <- filtered_df
    }
  } else {
    # Handle the case with no factor variables
    filtered_df <- dat_wide %>%
      arrange(!!rlang::sym(sort_var))
    list_filtered_df[["data"]] <- filtered_df
  }

  return(list_filtered_df)
}
