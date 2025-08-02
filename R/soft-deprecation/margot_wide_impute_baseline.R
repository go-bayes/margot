#' Transform longitudinal data to wide format and impute baseline (soft-deprecated)
#'
#' **Deprecated**: Use `margot_wide_machine(imputation_method = "mice")` instead.
#'
#' @section Deprecated:
#' This function was soft-deprecated in version 1.0.38 and will be removed in a future major release.
#'
#' @param .data A data frame containing the longitudinal data in long format.
#' @param baseline_vars A character vector of baseline variable names to include and impute.
#' @param exposure_var A character vector specifying the names of exposure variables.
#' @param outcome_vars A character vector specifying the names of outcome variables.
#'
#' @return A data frame in wide format with imputed baseline variables.
#'
#' @examples
#' # Preferred: use margot_wide_machine with mice imputation
#' # wide_df <- margot_wide_machine(
#' #   df,
#' #   baseline_vars = c("age", "male"),
#' #   exposure_var = "forgiveness",
#' #   outcome_vars = "kessler_latent_anxiety",
#' #   imputation_method = "mice"
#' # )
#'
#' @importFrom lifecycle deprecate_soft
#' @keywords internal
margot_wide_impute_baseline <- function(.data,
                                        baseline_vars,
                                        exposure_var,
                                        outcome_vars) {
  # emit soft-deprecation warning
  lifecycle::deprecate_soft(
    when = "1.0.38",
    what = "margot::margot_wide_impute_baseline()",
    with = "margot::margot_wide_machine(imputation_method = \"mice\")"
  )

  # delegate to the new wide-machine implementation
  margot_wide_machine(
    .data,
    id = "id",
    wave = "wave",
    baseline_vars = baseline_vars,
    exposure_var = exposure_var,
    outcome_vars = outcome_vars,
    confounder_vars = NULL,
    imputation_method = "mice",
    include_exposure_var_baseline = TRUE,
    include_outcome_vars_baseline = TRUE,
    extend_baseline = FALSE,
    include_na_indicators = FALSE
  )
}

#' #' Transform to wide data with labels and impute baseline missing values
#' #'
#' #' This function transforms longitudinal data into a wide format and performs imputation on baseline variables.
#' #' It leverages the `mice` package for imputation, ensuring that missing baseline data are appropriately handled.
#' #' The function arranges data by participant id and time, filters based on time conditions, and reshapes the data into wide format.
#' #' Imputed data are then merged back, maintaining the structure suitable for analysis.
#' #'
#' #' @param .data A dataframe containing the longitudinal data.
#' #' @param baseline_vars A character vector of baseline variable names to include and impute if necessary.
#' #' @param exposure_var A character vector specifying the names of exposure variables.
#' #' @param outcome_vars A character vector specifying the names of outcome variables.
#' #'
#' #' @return A data frame in wide format with imputed baseline variables.
#' #'
#' #' @examples
#' #' # df_nz is a synthetic dataset included in this package
#' #' data(df_nz, package = "margot")
#' #'
#' #' df_nz <- data.frame(df_nz)
#' #' wide_data_imputed <- margot_wide_impute_baseline(df_nz,
#' #'   baseline_vars = c("age", "male", "religion_believe_god"),
#' #'   exposure_var = ("forgiveness"),
#' #'   outcome_vars = ("kessler_latent_anxiety")
#' #'   )
#' #'
#' #' @importFrom dplyr mutate arrange filter select relocate
#' #' @importFrom tidyr pivot_wider
#' #' @importFrom mice mice complete
#' margot_wide_impute_baseline <- function(.data,
#'                                         baseline_vars,
#'                                         exposure_var,
#'                                         outcome_vars) {
#'   if (!is.data.frame(.data)) {
#'     stop("The provided data is not a data frame.")
#'   }
#'
#'   # verify all vars are in the data
#'
#'   all_vars <- c(baseline_vars, exposure_var, outcome_vars)
#'   if (!all(all_vars %in% names(.data))) {
#'     missing_vars <- all_vars[!all_vars %in% names(.data)]
#'     stop("The following variables are missing: ",
#'          paste(missing_vars, collapse = ", "))
#'   }
#'   # add a check for unused levels of factor variables
#'   lapply(names(.data), function(col_name) {
#'     column <- .data[[col_name]]
#'     if (is.factor(column) && any(table(column) == 0)) {
#'       # Option to remove unused levels or issue a warning
#'       warning("Removing unused levels in factor variable: ", col_name)
#'       .data[[col_name]] <- factor(column)
#'     }
#'   })
#'
#'   # add the 'time' column to the data
#'   data_with_time <- .data |>
#'     dplyr::mutate(time = as.numeric(wave) - 1) |>
#'     dplyr::arrange(id, time)
#'
#'   # filter the data based on the time condition
#'   data_filtered <- data_with_time |>
#'     dplyr::filter(time >= 0)
#'
#'   # create the wide data frame
#'   wide_data <- data_filtered |>
#'     tidyr::pivot_wider(
#'       id_cols = id,
#'       names_from = time,
#'       values_from = -c(id, time),
#'       names_glue = "t{time}_{.value}",
#'       names_prefix = "t"
#'     )
#'
#'   # identify the columns starting with "t0_" that need to be imputed
#'   t0_columns <-
#'     grepl("^t0_", names(wide_data)) &
#'     names(wide_data) %in% paste0("t0_", c(baseline_vars, exposure_var, outcome_vars))
#'
#'   # apply the imputation
#'   t0_data <- wide_data[, t0_columns, drop = FALSE]
#'   imputed_data <- mice::mice(t0_data, method = 'pmm', m = 1)
#'   complete_t0_data <- mice::complete(imputed_data, 1)
#'
#'   # merge the imputed data back into the wide data
#'   wide_data[, t0_columns] <- complete_t0_data
#'
#'   # define a custom function to filter columns based on conditions
#'   custom_col_filter <- function(col_name) {
#'     if (startsWith(col_name, "t0_")) {
#'       return(col_name %in% c(
#'         paste0("t0_", baseline_vars),
#'         paste0("t0_", exposure_var),
#'         paste0("t0_", outcome_vars)
#'       ))
#'     } else if (startsWith(col_name, "t1_")) {
#'       return(col_name %in% paste0("t1_", exposure_var))
#'     } else if (grepl("^t[2-9][0-9]*_", col_name)) {
#'       return(col_name %in% paste0("t2_", outcome_vars))
#'     } else {
#'       return(FALSE)
#'     }
#'   }
#'
#'   # apply the custom function to select the desired columns
#'   wide_data_filtered <- wide_data |>
#'     dplyr::select(id, which(sapply(
#'       colnames(wide_data), custom_col_filter
#'     ))) |>
#'     dplyr::relocate(starts_with("t0_"), .before = starts_with("t1_"))  |>
#'     dplyr::arrange(id)
#'
#'   # extract unique time values from column names
#'   time_values <-
#'     gsub("^t([0-9]+)_.+$", "\\1", colnames(wide_data_filtered))
#'   time_values <- time_values[grepl("^[0-9]+$", time_values)]
#'   time_values <- unique(as.numeric(time_values))
#'   time_values <- time_values[order(time_values)]
#'
#'   # relocate columns iteratively
#'   for (i in 2:(length(time_values) - 1)) {
#'     wide_data_filtered <- wide_data_filtered |>
#'       dplyr::relocate(starts_with(paste0("t", time_values[i + 1], "_")), .after = starts_with(paste0("t", time_values[i], "_")))
#'   }
#'
#'   # reorder t0_ columns
#'   t0_column_order <-
#'     c(
#'       paste0("t0_", baseline_vars),
#'       paste0("t0_", exposure_var),
#'       paste0("t0_", outcome_vars)
#'     )
#'   wide_data_ordered <- wide_data_filtered |>
#'     dplyr::select(id, t0_column_order, everything())
#'
#'   return(data.frame(wide_data_ordered)) # Ensure output is a data.frame
#' }
