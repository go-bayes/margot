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
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate arrange filter select relocate
#' @importFrom tidyr pivot_wider
#' @importFrom mice mice complete
#' @export

margot_wide_impute_baseline <-
  function(.data,
           baseline_vars,
           exposure_var,
           outcome_vars) {
    if (!is.data.frame(.data)) {
      stop("The provided data is not a data frame.")
    }

    # Add a check for unused levels of factor variables
    lapply(.data, function(column) {
      if (is.factor(column) && any(table(column) == 0)) {
        stop("There are unused levels in the factor variable: ", deparse(substitute(column)))
      }
    })
    # add the 'time' column to the data
    data_with_time <- .data %>%
      mutate(time = as.numeric(wave) - 1) %>%
      arrange(id, time)

    # filter the data based on the time condition
    data_filtered <- data_with_time %>%
      filter(time >= 0)

    # create the wide data frame
    wide_data <- data_filtered %>%
      pivot_wider(
        id_cols = id,
        names_from = time,
        values_from = -c(id, time),
        names_glue = "t{time}_{.value}",
        names_prefix = "t"
      )

    # identify the columns starting with "t0_" that need to be imputed
    t0_columns <-
      grepl("^t0_", names(wide_data)) &
      names(wide_data) %in% paste0("t0_", c(baseline_vars, exposure_var, outcome_vars))

    # apply the imputation
    t0_data <- wide_data[, t0_columns, drop = FALSE]
    imputed_data <- mice(t0_data, method = 'pmm', m = 1)
    complete_t0_data <- complete(imputed_data, 1)

    # merge the imputed data back into the wide data
    wide_data[, t0_columns] <- complete_t0_data

    # define a custom function to filter columns based on conditions
    custom_col_filter <- function(col_name) {
      if (startsWith(col_name, "t0_")) {
        return(col_name %in% c(
          paste0("t0_", baseline_vars),
          paste0("t0_", exposure_var),
          paste0("t0_", outcome_vars)
        ))
      } else if (startsWith(col_name, "t1_")) {
        return(col_name %in% paste0("t1_", exposure_var))
      } else if (grepl("^t[2-9][0-9]*_", col_name)) {
        return(col_name %in% paste0("t2_", outcome_vars))
      } else {
        return(FALSE)
      }
    }

    # apply the custom function to select the desired columns
    wide_data_filtered <- wide_data %>%
      dplyr::select(id, which(sapply(
        colnames(wide_data), custom_col_filter
      ))) %>%
      dplyr::relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
      arrange(id)

    # extract unique time values from column names
    time_values <-
      gsub("^t([0-9]+)_.+$", "\\1", colnames(wide_data_filtered))
    time_values <- time_values[grepl("^[0-9]+$", time_values)]
    time_values <- unique(as.numeric(time_values))
    time_values <- time_values[order(time_values)]

    # relocate columns iteratively
    for (i in 2:(length(time_values) - 1)) {
      wide_data_filtered <- wide_data_filtered %>%
        dplyr::relocate(starts_with(paste0("t", time_values[i + 1], "_")), .after = starts_with(paste0("t", time_values[i], "_")))
    }
    existing_cols <- names(wide_data_filtered)
    t0_column_order <- c(
      paste0("t0_", baseline_vars),
      paste0("t0_", exposure_var),
      paste0("t0_", outcome_vars)
    )
    t0_column_order <- t0_column_order[t0_column_order %in% existing_cols]

    # reorder t0_ columns
    t0_column_order <-
      c(
        paste0("t0_", baseline_vars),
        paste0("t0_", exposure_var),
        paste0("t0_", outcome_vars)
      )
    wide_data_ordered <- wide_data_filtered %>%
      select(id, all_of(t0_column_order), everything())

    return(data.frame(wide_data_ordered)) # Ensure output is a data.frame

    return(data.frame(wide_data_ordered)) # Ensure output is a data.frame
  }
