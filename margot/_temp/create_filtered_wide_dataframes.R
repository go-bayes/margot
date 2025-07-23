#' Create Filtered Wide Dataframes Based on Exposure Variables
#'
#' This function processes a wide format dataframe to filter and create a list of dataframes
#' based on the levels of a specified factor exposure variable. It separates the dataframes
#' based on each level of the factor variable, if one is present. If only continuous variables
#' are specified, it returns the dataframe arranged by an identifier.
#'
#' @param dat_wide A wide format dataframe that contains the exposure variables.
#' @param exposure_vars A character vector listing the names of the exposure variables.
#'        This vector must include at least one valid variable name in the dataframe.
#'
#' @return A list of dataframes, each corresponding to a level of the factor exposure variable
#'         if a factor is present; otherwise, a single dataframe arranged by identifier.
#'
#' @examples
#' # Assuming wide_data is a dataframe and "exposure_var" includes factor or continuous variables:
#' list_filtered_df <- create_filtered_wide_dataframes(wide_data, c("exposure_var1", "exposure_var2"))
#' # Access individual filtered dataframe if factor variables present:
#' q1_df <- list_filtered_df[["tile_1"]]  # For factor level "tile_1"
#'
#' @importFrom dplyr filter arrange
#' @importFrom rlang sym
#' @export
create_filtered_wide_dataframes <- function(dat_wide, exposure_vars) {
  # Validate presence of exposure variables in the dataframe
  for (exposure_var in exposure_vars) {
    if (!exposure_var %in% names(dat_wide)) {
      stop(paste("Exposure variable", exposure_var, "is not in the dataframe."))
    }
  }

  # Distinguish between factor and continuous exposure variables
  factor_exposure_vars <- exposure_vars[sapply(dat_wide[exposure_vars], is.factor)]
  continuous_exposure_vars <- setdiff(exposure_vars, factor_exposure_vars)

  # Check to ensure only one factor exposure variable is present
  if (length(factor_exposure_vars) > 1) {
    stop("More than one factor exposure variable is not allowed.")
  }

  # Initialize list to store filtered dataframes
  list_filtered_df <- list()

  # Filter dataframes based on the level of factor exposure variable
  if (length(factor_exposure_vars) == 1) {
    factor_levels <- levels(dat_wide[[factor_exposure_vars]])
    # Loop through each level and filter the dataframe accordingly
    for (level in factor_levels) {
      filtered_df <- dat_wide %>%
        filter((!!rlang::sym(factor_exposure_vars)) == level) %>%
        arrange(id)  # assuming 'id' is a column used to arrange the data
      list_filtered_df[[level]] <- filtered_df
    }
  } else {
    # If no factor variables, return the dataframe arranged by id
    filtered_df <- dat_wide %>% arrange(id)
    list_filtered_df[["data"]] <- filtered_df
  }

  return(list_filtered_df)
}
