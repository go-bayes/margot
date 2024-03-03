#' Wide Format Data Transformation with Baseline Imputation
#'
#' This function transforms longitudinal data into a wide format and performs imputation on baseline variables.
#' It leverages the `mice` package for imputation, ensuring that missing baseline data are appropriately handled.
#' The function arranges data by participant id and time, filters based on time conditions, and reshapes the data into wide format.
#' Imputed data are then merged back, maintaining the structure suitable for analysis.
#'
#' @param .data A dataframe containing the longitudinal data.
#' @param baseline_vars A character vector of baseline variable names to include and impute if necessary.
#' @param exposure_var A character vector specifying the names of exposure variables.
#' @param outcome_vars A character vector specifying the names of outcome variables.
#'
#' @return A data frame in wide format with imputed baseline variables.
#'
#' @examples
#' # the df_nz is a dataset available in the 'margot' package
#' data(df_nz, package = "margot")
#'
#' df_nz <- data.frame(df_nz)
#' wide_data_imputed <- margot_wide_impute_baseline(df_nz,
#'   baseline_vars = c("age", "male", "religion_believe_god"),
#'   exposure_var = ("forgiveness"),
#'   outcome_vars = ("kessler_latent_anxiety")
#'   )
#'
#' @importFrom dplyr mutate arrange filter select relocate
#' @importFrom tidyr pivot_wider
#' @importFrom mice mice complete
#' @export

# refactor
margot_wide_impute_baseline <- function(.data, baseline_vars, exposure_var, outcome_vars) {
  if (!is.data.frame(.data)) {
    stop("The provided data is not a data frame.")
  }
  # Use margot_wide to transform data into wide format
  wide_data <- margot_wide(.data, baseline_vars, exposure_var, outcome_vars)

  # Identify columns for baseline (t0_) that need imputation
  t0_columns <- names(wide_data)[grepl("^t0_", names(wide_data)) &
                                   names(wide_data) %in% paste0("t0_", c(baseline_vars, exposure_var, outcome_vars))]

  if (length(t0_columns) > 0) {
    # Impute missing values for baseline variables
    t0_data <- wide_data[t0_columns]
    imputed_data <- mice::mice(t0_data, method = 'pmm', m = 1, print = FALSE)
    complete_t0_data <- mice::complete(imputed_data, 1)

    # Merge the imputed data back into the wide data
    wide_data[t0_columns] <- complete_t0_data
  }

  return(wide_data)
}



