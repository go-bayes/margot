#' Transform longitudinal data to wide format with labels
#'
#' This function transforms longitudinal data from long format to wide format,
#' ensuring that baseline measurements are correctly labelled and included.
#' It handles multiple observations per subject across different waves,
#' and allows for the specification of baseline variables, exposure variables,
#' and outcome variables. It ensures that all specified variables are
#' included in the resulting wide-format data frame, with columns prefixed
#' by the time of measurement.
#'
#' @param .data A data frame containing the longitudinal data in long format.
#' @param baseline_vars A character vector of baseline variable names to be included at t0.
#' @param exposure_var A character vector of exposure variable names to be tracked across time.
#' @param outcome_vars A character vector of outcome variable names to be tracked across time.
#'
#' @return A wide-format data frame with each subject's observations across time points
#'         represented in a single row, and variables prefixed by their respective
#'         time of measurement.
#'
#' @examples
#' # Defining variables as per the function's documentation
#' baseline_vars <- c(
#'   "male", "age",  "eth_cat",
#'   "partner", "agreeableness",
#'   "conscientiousness", "extraversion", "honesty_humility",
#'   "openness", "neuroticism", "sample_weights"
#' )
#'
#' exposure_var <- c("forgiveness")
#'
#' outcome_vars <- c(
#'   "alcohol_frequency", "alcohol_intensity",
#'   "hlth_bmi", "hours_exercise"
#' )
#'
#' # df_nz is a dataset included in this package
#' # wide_data <- margot_wide(df_nz, baseline_vars, exposure_var, outcome_vars)
#' # print(wide_data)
#'
#' @export
#' @importFrom dplyr mutate arrange filter select
#' @importFrom tidyr pivot_wider
margot_wide <-
  function(.data,
           baseline_vars,
           exposure_var,
           outcome_vars) {
    require(dplyr)
    require(tidyr)

    # add a check for unused levels of factor variables
    lapply(.data, function(column) {
      if (is.factor(column) && any(table(column) == 0)) {
        stop("There are unused levels in the factor variable: ",
             deparse(substitute(column)))
      }
    })

    # add the 'time' column to the data
    data_with_time <- .data |>
      dplyr::mutate(time = as.numeric(wave) - 1) |>
      dplyr::arrange(id, time)

    # filter the data based on the time condition
    data_filtered <- data_with_time |>
      dplyr::filter(time >= 0)

    # create the wide data frame
    wide_data <- data_filtered |>
      tidyr::pivot_wider(
        id_cols = id,
        names_from = time,
        #values_from = -c(id, time),
        values_from = setdiff(names(data_filtered), c("id", "time")),
        names_glue = "t{time}_{.value}",
        names_prefix = "t"
      )

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
    wide_data_filtered <- wide_data |>
      dplyr::select(id, which(sapply(
        colnames(wide_data), custom_col_filter
      ))) |>
      dplyr::relocate(starts_with("t0_"), .before = starts_with("t1_"))  |>
      dplyr::arrange(id)

    # extract unique time values from column names
    time_values <-
      gsub("^t([0-9]+)_.+$", "\\1", colnames(wide_data_filtered))
    time_values <- time_values[grepl("^[0-9]+$", time_values)]
    time_values <- unique(as.numeric(time_values))
    time_values <- time_values[order(time_values)]

    # relocate columns iteratively
    for (i in 2:(length(time_values) - 1)) {
      wide_data_filtered <- wide_data_filtered |>
        dplyr::relocate(starts_with(paste0("t", time_values[i + 1], "_")), .after = starts_with(paste0("t", time_values[i], "_")))
    }

    # reorder t0_ columns
    t0_column_order <-
      c(
        paste0("t0_", baseline_vars),
        paste0("t0_", exposure_var),
        paste0("t0_", outcome_vars)
      )
    wide_data_ordered <- wide_data_filtered |>
      dplyr::select(id, all_of(t0_column_order), everything())

    return(data.frame(wide_data_ordered)) # Ensure output is a data.frame
  }
