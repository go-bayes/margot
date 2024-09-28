#' Process Longitudinal Data for Multiple Waves
#'
#' @description
#' This function processes longitudinal data for an arbitrary number of waves (e.g., t0, t1, t2, ...).
#' It handles attrition, scales continuous variables, optionally encodes ordinal variables, and manages exposure variables.
#'
#' @param df_wide A wide-format dataframe containing longitudinal data for multiple waves.
#' @param ordinal_columns A character vector of column names to be treated as ordinal and dummy-coded.
#' @param continuous_columns_keep A character vector of continuous column names to keep without scaling.
#' @param exposure_var_name Name of the exposure variable. This variable will be used to determine attrition.
#' @param scale_exposure Logical. If TRUE, scales the exposure variable. Default is FALSE.
#' @param not_lost_indicator_name Name of the 'not lost' indicator. Default is "not_lost".
#' @param lost_indicator_name Name of the 'lost' indicator. If NULL, no 'lost' indicator is created.
#' @param remove_selected_columns Logical. If TRUE, removes selected columns after encoding. Default is FALSE.
#' @param time_point_prefixes A character vector of time point prefixes. If NULL, they will be inferred from the data.
#' @param time_point_regex A regex pattern to identify time points. Used if time_point_prefixes is NULL.
#'
#' @return A processed dataframe suitable for use in longitudinal analyses with multiple waves.
#'
#' @details
#' The function performs the following steps:
#' 1. Identifies all time points in the dataset.
#' 2. Creates 'not_lost' indicators based on the exposure variable in subsequent waves.
#' 3. Applies attrition logic across all waves.
#' 4. Scales continuous variables across all waves, removing original non-scaled columns.
#' 5. Optionally encodes ordinal columns.
#' 6. Handles missing outcomes in the final wave.
#' 7. Reorders columns, placing exposure and 'not_lost' indicators appropriately.
#'
#' @import dplyr
#' @import fastDummies
#' @import cli
#'
#' @examples
#' # Assuming df_wide is your wide-format dataframe with multiple waves
#' processed_data <- margot_process_longitudinal_data_wider(
#'   df_wide,
#'   ordinal_columns = c("education", "income_category"),
#'   continuous_columns_keep = c("age", "bmi"),
#'   exposure_var_name = "treatment",
#'   scale_exposure = FALSE,
#'   not_lost_indicator_name = "not_lost",
#'   lost_indicator_name = NULL,
#'   remove_selected_columns = FALSE,
#'   time_point_prefixes = c("t0", "t1", "t2", "t3")
#' )
#' @export
margot_process_longitudinal_data_wider <- function(
    df_wide,
    ordinal_columns = NULL,
    continuous_columns_keep = NULL,
    exposure_var_name = NULL,
    scale_exposure = FALSE,
    not_lost_indicator_name = "not_lost",
    lost_indicator_name = NULL,
    remove_selected_columns = FALSE,
    time_point_prefixes = NULL,
    time_point_regex = NULL
) {
  cli::cli_h1("Longitudinal Data Processing")
  cli::cli_alert_info("Starting data processing for longitudinal data with multiple time points")

  # Identify time points
  if (is.null(time_point_prefixes)) {
    if (is.null(time_point_regex)) {
      time_point_regex <- "^(t\\d+)_.*$"
    }
    matched_cols <- grep(time_point_regex, colnames(df_wide), value = TRUE)
    time_points <- unique(gsub(time_point_regex, "\\1", matched_cols))
    time_points <- time_points[order(as.numeric(gsub("t", "", time_points)))]
  } else {
    time_points <- time_point_prefixes
  }
  num_time_points <- length(time_points)

  cli::cli_alert_info("Identified {num_time_points} time points: {paste(time_points, collapse = ', ')}")

  # Initialize the data frame for processing
  df_wide_use <- df_wide

  # Step 1: Create 'not_lost' indicators based on exposure variable
  cli::cli_h2("Step 1: Creating 'not_lost' indicators based on exposure variable")

  for (i in seq_len(num_time_points - 1)) {
    t_i <- time_points[i]
    t_i_plus1 <- time_points[i + 1]

    exposure_col <- paste0(t_i, "_", exposure_var_name)
    exposure_col_next <- paste0(t_i_plus1, "_", exposure_var_name)

    not_lost_col <- paste0(t_i, "_", not_lost_indicator_name)

    # Check if exposure columns exist
    if (!exposure_col %in% names(df_wide_use) || !exposure_col_next %in% names(df_wide_use)) {
      cli::cli_alert_warning("Exposure column '{exposure_col}' or '{exposure_col_next}' not found. Skipping 'not_lost' indicator for {t_i}.")
      next
    }

    # Create 'not_lost' indicator
    df_wide_use[[not_lost_col]] <- ifelse(is.na(df_wide_use[[exposure_col]]) & !is.na(df_wide_use[[exposure_col_next]]), 0, 1)

    cli::cli_alert_success("Created '{not_lost_col}' indicator")
  }

  # Step 2: Applying attrition logic
  cli::cli_h2("Step 2: Applying attrition logic")

  for (i in seq_along(time_points)) {
    t_i <- time_points[i]

    if (i > 1) {
      previous_not_lost_cols <- paste0(time_points[1:(i - 1)], "_", not_lost_indicator_name)
      existing_not_lost_cols <- previous_not_lost_cols[previous_not_lost_cols %in% names(df_wide_use)]

      if (length(existing_not_lost_cols) > 0) {
        lost_before_t_i <- rowSums(df_wide_use[, existing_not_lost_cols, drop = FALSE] == 0) > 0

        # Set exposure variable to NA if lost before this time point
        exposure_col <- paste0(t_i, "_", exposure_var_name)
        if (exposure_col %in% names(df_wide_use)) {
          df_wide_use[[exposure_col]][lost_before_t_i] <- NA
        } else {
          cli::cli_alert_warning("Exposure column '{exposure_col}' not found. Skipping attrition logic for {t_i}.")
        }
      } else {
        cli::cli_alert_warning("No existing 'not_lost' indicators found for time points before {t_i}. Skipping attrition logic.")
      }
    }

    cli::cli_alert_success("Applied attrition logic for time point {t_i}")
  }

  # Step 3: Scaling continuous variables across all waves
  cli::cli_h2("Step 3: Scaling continuous variables across all waves")

  # Identify continuous columns to scale
  continuous_cols <- names(df_wide_use)[sapply(df_wide_use, is.numeric)]
  continuous_cols <- continuous_cols[!continuous_cols %in% c(continuous_columns_keep, ordinal_columns)]
  continuous_cols <- continuous_cols[!grepl("_binary$|_na$|_weights$", continuous_cols)]

  # Remove exposure variable from scaling unless scale_exposure is TRUE
  if (!scale_exposure) {
    exposure_cols <- paste0(time_points, "_", exposure_var_name)
    continuous_cols <- setdiff(continuous_cols, exposure_cols)
  }

  # Remove not_lost and lost indicators from scaling
  not_lost_cols <- paste0(time_points, "_", not_lost_indicator_name)
  lost_cols <- if(!is.null(lost_indicator_name)) paste0(time_points, "_", lost_indicator_name) else character(0)
  continuous_cols <- setdiff(continuous_cols, c(not_lost_cols, lost_cols))

  # Scale continuous variables for all waves and remove original columns
  cols_to_remove <- character(0)
  for (col in continuous_cols) {
    df_wide_use[[paste0(col, "_z")]] <- scale(df_wide_use[[col]]) %>% as.vector()
    cols_to_remove <- c(cols_to_remove, col)
  }

  # Remove original columns that have been z-transformed
  df_wide_use <- df_wide_use[, !names(df_wide_use) %in% cols_to_remove]

  cli::cli_alert_success("Scaled continuous variables across all waves and removed original columns")

  # Step 4: Encoding ordinal columns
  cli::cli_h2("Step 4: Encoding ordinal columns")

  if (!is.null(ordinal_columns)) {
    df_wide_use <- fastDummies::dummy_cols(
      df_wide_use,
      select_columns = ordinal_columns,
      remove_first_dummy = FALSE,
      remove_most_frequent_dummy = FALSE,
      remove_selected_columns = remove_selected_columns,
      ignore_na = TRUE
    )

    # Rename dummy variables
    for (ordinal_col in ordinal_columns) {
      dummy_cols <- grep(paste0("^", ordinal_col, "_"), names(df_wide_use), value = TRUE)
      df_wide_use <- df_wide_use %>%
        rename_at(vars(all_of(dummy_cols)), ~ paste0(., "_binary"))
    }

    cli::cli_alert_success("Encoded ordinal columns")
  } else {
    cli::cli_alert_info("No ordinal columns to encode")
  }

  # Step 5: Handle missing outcomes in the final wave
  cli::cli_h2("Step 5: Handling missing outcomes in the final wave")

  final_wave <- time_points[num_time_points]
  penultimate_wave <- time_points[num_time_points - 1]
  final_wave_cols <- grep(paste0("^", final_wave, "_"), names(df_wide_use), value = TRUE)
  final_wave_cols <- setdiff(final_wave_cols, paste0(final_wave, "_", exposure_var_name))

  if (length(final_wave_cols) > 0) {
    missing_final_wave <- rowSums(is.na(df_wide_use[, final_wave_cols, drop = FALSE])) > 0
    not_lost_col_penultimate <- paste0(penultimate_wave, "_", not_lost_indicator_name)

    if (not_lost_col_penultimate %in% names(df_wide_use)) {
      df_wide_use[[not_lost_col_penultimate]][missing_final_wave] <- 0
      cli::cli_alert_success("Handled missing outcomes in the final wave")
    } else {
      cli::cli_alert_warning("'{not_lost_col_penultimate}' not found. Could not handle missing outcomes in the final wave.")
    }
  } else {
    cli::cli_alert_warning("No outcome columns found for the final wave. Skipping handling of missing outcomes.")
  }

  # New step: Create lost indicator if specified
  if (!is.null(lost_indicator_name)) {
    cli::cli_h2("Creating lost indicator")
    for (t in time_points) {
      not_lost_col <- paste0(t, "_", not_lost_indicator_name)
      lost_col <- paste0(t, "_", lost_indicator_name)
      if (not_lost_col %in% names(df_wide_use)) {
        df_wide_use[[lost_col]] <- 1 - df_wide_use[[not_lost_col]]
        cli::cli_alert_success("Created '{lost_col}' indicator")
      }
    }
  }

  # Step 6: Reordering columns
  cli::cli_h2("Step 6: Reordering columns")

  new_order <- c("id")  # Start with the ID column

  for (t in time_points) {
    t_cols <- grep(paste0("^", t, "_"), names(df_wide_use), value = TRUE)

    # Separate columns into different categories
    exposure_col <- paste0(t, "_", exposure_var_name)
    not_lost_col <- paste0(t, "_", not_lost_indicator_name)
    lost_col <- if(!is.null(lost_indicator_name)) paste0(t, "_", lost_indicator_name) else NULL

    # Identify z-transformed columns
    z_cols <- grep("_z$", t_cols, value = TRUE)

    # Other columns (excluding z-transformed, exposure, not_lost, and lost)
    other_cols <- setdiff(t_cols, c(z_cols, exposure_col, not_lost_col, lost_col))

    # Order: other columns, z-transformed columns, exposure, not_lost, lost (if exists)
    new_order <- c(new_order, other_cols, z_cols, exposure_col, not_lost_col, lost_col)
  }

  # Remove any NULL values (in case lost_col doesn't exist)
  new_order <- new_order[!sapply(new_order, is.null)]

  # Keep only columns that exist in the dataframe
  new_order <- intersect(new_order, names(df_wide_use))

  # Reorder the dataframe
  df_wide_use <- df_wide_use[, new_order]

  cli::cli_alert_success("Columns reordered successfully")

  # Final messages
  cli::cli_alert_success("Data processing completed successfully")
  cli::cli_h2("Summary")
  cli::cli_ul(c(
    "Total rows processed: {nrow(df_wide_use)}",
    "Total columns: {ncol(df_wide_use)}",
    "Time points processed: {length(time_points)}"
  ))

  return(df_wide_use)
}
