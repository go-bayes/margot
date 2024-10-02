#' Process Longitudinal Data for Multiple Waves
#'
#' @description
#' This function processes longitudinal data for an arbitrary number of waves (e.g., t0, t1, t2, ...).
#' It handles attrition, scales continuous variables, optionally encodes ordinal variables, and manages exposure variables.
#'
#' @param df_wide A wide-format dataframe containing longitudinal data for multiple waves.
#' @param ordinal_columns A character vector of column names to be treated as ordinal and dummy-coded.
#' @param continuous_columns_keep A character vector of continuous column names to keep without scaling.
#' @param exposure_var Name of the exposure variable. This variable will be used to determine attrition.
#' @param scale_exposure Logical. If TRUE, scales the exposure variable. Default is FALSE.
#' @param not_lost_in_following_wave Name of the 'not lost' indicator. Default is "not_lost_following_wave".
#' @param lost_in_following_wave Name of the 'lost' indicator. If NULL, no 'lost' indicator is created.
#' @param remove_selected_columns Logical. If TRUE, removes selected columns after encoding. Default is FALSE.
#' @param time_point_prefixes A character vector of time point prefixes. If NULL, they will be inferred from the data.
#' @param time_point_regex A regex pattern to identify time points. Used if time_point_prefixes is NULL.
#' @param save_observed_y Logical. If TRUE, retains observed outcome values in the final wave even if lost. Default is FALSE.
#' @param censored_if_any_lost Determines how to treat the 'not_lost_in_following_wave' indicator.
#' If TRUE, sets 'not_lost_in_following_wave' to 0 if any value is NA in the following wave.
#' If NA, sets 'not_lost_in_following_wave' to NA if one or more variables are observed in the final wave.
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
#' 6. Handles missing outcomes in the final wave based on 'save_observed_y'.
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
#'   exposure_var = "treatment",
#'   scale_exposure = FALSE,
#'   not_lost_in_following_wave = "not_lost",
#'   lost_in_following_wave = NULL,
#'   remove_selected_columns = FALSE,
#'   time_point_prefixes = c("t0", "t1", "t2", "t3"),
#'   save_observed_y = TRUE,
#'   censored_if_any_lost = TRUE
#' )
#' @export
margot_process_longitudinal_data_wider <- function(
    df_wide,
    ordinal_columns = NULL,
    continuous_columns_keep = NULL,
    exposure_var = NULL,
    scale_exposure = FALSE,
    not_lost_in_following_wave = "not_lost_following_wave",
    lost_in_following_wave = NULL,
    remove_selected_columns = FALSE,
    time_point_prefixes = NULL,
    time_point_regex = NULL,
    save_observed_y = FALSE,
    censored_if_any_lost = TRUE
) {
  cli::cli_h1("Longitudinal Data Processing")
  cli::cli_alert_info("Starting data processing for longitudinal data with multiple time points")

  # identify time points
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

  # init the data frame for processing
  df_wide_use <- df_wide

  # determine outcome var
  final_wave <- time_points[num_time_points]
  final_wave_cols <- grep(paste0("^", final_wave, "_"), names(df_wide_use), value = TRUE)
  outcome_vars <- gsub(paste0("^", final_wave, "_"), "", final_wave_cols)
  cli::cli_alert_info("Using all variables in the final wave as outcomes.")

  # Step 1: create 'not_lost_in_following_wave' indicators and handle missingness
  cli::cli_h2("Step 1: Creating 'not_lost_in_following_wave' indicators and handling missingness")

  for (i in seq_len(num_time_points - 1)) {
    t_i <- time_points[i]
    t_i_plus1 <- time_points[i + 1]

    exposure_col <- paste0(t_i, "_", exposure_var)
    exposure_col_next <- paste0(t_i_plus1, "_", exposure_var)

    not_lost_col <- paste0(t_i, "_", not_lost_in_following_wave)

    if (i == num_time_points - 1) {
      # For the penultimate wave, use outcome variables to determine not_lost status
      outcome_cols_next <- paste0(t_i_plus1, "_", outcome_vars)
      outcome_cols_next <- outcome_cols_next[outcome_cols_next %in% names(df_wide_use)]
      if (length(outcome_cols_next) == 0) {
        cli::cli_alert_warning("No outcome columns found for the final wave. Skipping 'not_lost' indicator for {t_i}.")
        next
      }
      if (censored_if_any_lost == TRUE) {
        df_wide_use[[not_lost_col]] <- ifelse(
          rowSums(is.na(df_wide_use[, outcome_cols_next, drop = FALSE])) == 0, 1, 0
        )
      } else if (is.na(censored_if_any_lost)) {
        observed_in_final_wave <- rowSums(!is.na(df_wide_use[, outcome_cols_next, drop = FALSE])) > 0
        df_wide_use[[not_lost_col]] <- ifelse(observed_in_final_wave, NA, 0)
      } else {
        cli::cli_alert_warning("Invalid value for 'censored_if_any_lost'. Using default TRUE.")
        df_wide_use[[not_lost_col]] <- ifelse(
          rowSums(is.na(df_wide_use[, outcome_cols_next, drop = FALSE])) == 0, 1, 0
        )
      }
    } else {
      # for other waves, use the exposure variable as before
      if (!exposure_col %in% names(df_wide_use) || !exposure_col_next %in% names(df_wide_use)) {
        cli::cli_alert_warning("Exposure column '{exposure_col}' or '{exposure_col_next}' not found. Skipping 'not_lost' indicator for {t_i}.")
        next
      }
      df_wide_use[[not_lost_col]] <- ifelse(!is.na(df_wide_use[[exposure_col_next]]), 1, 0)
    }

    # handle missingness for current and future waves
    rows_lost <- df_wide_use[[not_lost_col]] == 0
    if (any(rows_lost)) {
      future_waves <- time_points[(i+1):num_time_points]
      for (fw in future_waves) {
        future_cols <- grep(paste0("^", fw, "_"), names(df_wide_use), value = TRUE)
        if (save_observed_y && fw == final_wave) {
          # Exclude outcome variables from being set to NA
          outcome_cols_fw <- paste0(fw, "_", outcome_vars)
          future_cols <- setdiff(future_cols, outcome_cols_fw)
        }
        df_wide_use[rows_lost, future_cols] <- NA
      }
    }

    cli::cli_alert_success("Created '{not_lost_col}' indicator and handled missingness")
  }

  # Step 2: handle missing outcomes in the final wave
  cli::cli_h2("Step 2: Handling missing outcomes in the final wave")

  final_outcome_cols <- paste0(final_wave, "_", outcome_vars)
  final_outcome_cols <- final_outcome_cols[final_outcome_cols %in% names(df_wide_use)]

  if (length(final_outcome_cols) > 0) {
    if (!save_observed_y) {
      missing_final_wave <- rowSums(is.na(df_wide_use[, final_outcome_cols, drop = FALSE])) > 0
      df_wide_use[missing_final_wave, final_outcome_cols] <- NA
      cli::cli_alert_success("Set missing outcomes in the final wave to NA")
    } else {
      cli::cli_alert_info("'save_observed_y' is TRUE; retaining observed outcome values in the final wave")
    }
  } else {
    cli::cli_alert_warning("No outcome columns found for the final wave. Skipping handling of missing outcomes.")
  }

  # Step 3: create lost_in_following_wave indicator if specified
  if (!is.null(lost_in_following_wave)) {
    cli::cli_h2("Step 3: Creating lost_in_following_wave indicator")
    for (t in time_points[1:(num_time_points-1)]) {
      not_lost_col <- paste0(t, "_", not_lost_in_following_wave)
      lost_col <- paste0(t, "_", lost_in_following_wave)
      if (not_lost_col %in% names(df_wide_use)) {
        df_wide_use[[lost_col]] <- ifelse(is.na(df_wide_use[[not_lost_col]]), NA, 1 - df_wide_use[[not_lost_col]])
        cli::cli_alert_success("Created '{lost_col}' indicator")
      }
    }
  }

  # Step 4: scale continuous variables across all waves
  cli::cli_h2("Step 4: Scaling continuous variables across all waves")

  # identify continuous columns to scale
  continuous_cols <- names(df_wide_use)[sapply(df_wide_use, is.numeric)]
  continuous_cols <- continuous_cols[!continuous_cols %in% c(continuous_columns_keep, ordinal_columns)]
  continuous_cols <- continuous_cols[!grepl(paste0("_", not_lost_in_following_wave, "$|_", lost_in_following_wave, "$|_binary$|_na$|_weights$"), continuous_cols)]

  # remove exposure variable from scaling unless scale_exposure is TRUE
  if (!scale_exposure) {
    exposure_cols <- paste0(time_points, "_", exposure_var)
    continuous_cols <- setdiff(continuous_cols, exposure_cols)
  }

  # scale continuous variables for all waves and remove original columns
  cols_to_remove <- character(0)
  for (col in continuous_cols) {
    df_wide_use[[paste0(col, "_z")]] <- scale(df_wide_use[[col]]) %>% as.vector()
    cols_to_remove <- c(cols_to_remove, col)
  }

  # remove original columns that have been z-transformed
  df_wide_use <- df_wide_use[, !names(df_wide_use) %in% cols_to_remove]

  cli::cli_alert_success("Scaled continuous variables across all waves and removed original columns")

  # Step 5: encode ordinal columns
  cli::cli_h2("Step 5: Encoding ordinal columns")

  if (!is.null(ordinal_columns)) {
    df_wide_use <- fastDummies::dummy_cols(
      df_wide_use,
      select_columns = ordinal_columns,
      remove_first_dummy = FALSE,
      remove_most_frequent_dummy = FALSE,
      remove_selected_columns = remove_selected_columns,  # Use the parameter value
      ignore_na = TRUE
    )

    # rename dummy variables
    for (ordinal_col in ordinal_columns) {
      dummy_cols <- grep(paste0("^", ordinal_col, "_"), names(df_wide_use), value = TRUE)
      df_wide_use <- df_wide_use %>%
        rename_at(vars(all_of(dummy_cols)), ~ paste0(., "_binary"))
    }

    cli::cli_alert_success("Encoded ordinal columns and handled original categorical variables based on 'remove_selected_columns'")
  } else {
    cli::cli_alert_info("No ordinal columns to encode")
  }

  # Step 6: reordering columns
  cli::cli_h2("Step 6: Reordering columns")

  new_order <- c("id")  # Start with the ID column

  for (t in time_points) {
    t_cols <- grep(paste0("^", t, "_"), names(df_wide_use), value = TRUE)

    # separate columns into different categories
    exposure_col <- paste0(t, "_", exposure_var)
    not_lost_col <- paste0(t, "_", not_lost_in_following_wave)
    lost_col <- if(!is.null(lost_in_following_wave)) paste0(t, "_", lost_in_following_wave) else NULL

    # identify z-transformed columns
    z_cols <- grep("_z$", t_cols, value = TRUE)

    # other columns (excluding z-transformed, exposure, not_lost, and lost)
    other_cols <- setdiff(t_cols, c(z_cols, exposure_col, not_lost_col, lost_col))

    # order other columns, z-transformed columns, exposure, not_lost, lost (if exists)
    new_order <- c(new_order, other_cols, z_cols, exposure_col, not_lost_col, lost_col)
  }

  # remove any NULL values (in case lost_col doesn't exist)
  new_order <- new_order[!sapply(new_order, is.null)]

  # keep only columns that exist in the dataframe
  new_order <- intersect(new_order, names(df_wide_use))

  # reorder the dataframe
  df_wide_use <- df_wide_use[, new_order]

  cli::cli_alert_success("Columns reordered successfully \U0001F44D")

  # final messages
  cli::cli_alert_success("Data processing completed successfully \U0001F44D")
  cli::cli_h2("Summary")
  cli::cli_ul(c(
    "Total rows processed: {nrow(df_wide_use)}",
    "Total columns: {ncol(df_wide_use)}",
    "Time points processed: {length(time_points)}"
  ))

  return(df_wide_use)
}
# old function is working
# margot_process_longitudinal_data_wider <- function(
#     df_wide,
#     ordinal_columns = NULL,
#     continuous_columns_keep = NULL,
#     exposure_var = NULL,
#     scale_exposure = FALSE,
#     not_lost_in_following_wave = "not_lost_following_wave",
#     lost_in_following_wave = NULL,
#     remove_selected_columns = FALSE,
#     time_point_prefixes = NULL,
#     time_point_regex = NULL
# ) {
#   cli::cli_h1("Longitudinal Data Processing")
#   cli::cli_alert_info("Starting data processing for longitudinal data with multiple time points")
#
#   # identify time points
#   if (is.null(time_point_prefixes)) {
#     if (is.null(time_point_regex)) {
#       time_point_regex <- "^(t\\d+)_.*$"
#     }
#     matched_cols <- grep(time_point_regex, colnames(df_wide), value = TRUE)
#     time_points <- unique(gsub(time_point_regex, "\\1", matched_cols))
#     time_points <- time_points[order(as.numeric(gsub("t", "", time_points)))]
#   } else {
#     time_points <- time_point_prefixes
#   }
#   num_time_points <- length(time_points)
#
#   cli::cli_alert_info("Identified {num_time_points} time points: {paste(time_points, collapse = ', ')}")
#
#   # init the data frame for processing
#   df_wide_use <- df_wide
#
#   # determine outcome var
#   final_wave <- time_points[num_time_points]
#   final_wave_cols <- grep(paste0("^", final_wave, "_"), names(df_wide_use), value = TRUE)
#   outcome_vars <- gsub(paste0("^", final_wave, "_"), "", final_wave_cols)
#   cli::cli_alert_info("Using all variables in the final wave as outcomes.")
#
#   # Step 1: create 'not_lost_in_following_wave' indicators and handle missingness
#   cli::cli_h2("Step 1: Creating 'not_lost_in_following_wave' indicators and handling missingness")
#
#   for (i in seq_len(num_time_points - 1)) {
#     t_i <- time_points[i]
#     t_i_plus1 <- time_points[i + 1]
#
#     exposure_col <- paste0(t_i, "_", exposure_var)
#     exposure_col_next <- paste0(t_i_plus1, "_", exposure_var)
#
#     not_lost_col <- paste0(t_i, "_", not_lost_in_following_wave)
#
#     if (i == num_time_points - 1) {
#       # For the penultimate wave, use outcome variables to determine not_lost status
#       outcome_cols_next <- paste0(t_i_plus1, "_", outcome_vars)
#       outcome_cols_next <- outcome_cols_next[outcome_cols_next %in% names(df_wide_use)]
#       if (length(outcome_cols_next) == 0) {
#         cli::cli_alert_warning("No outcome columns found for the final wave. Skipping 'not_lost' indicator for {t_i}.")
#         next
#       }
#       df_wide_use[[not_lost_col]] <- ifelse(rowSums(is.na(df_wide_use[, outcome_cols_next, drop = FALSE])) == 0, 1, 0)
#     } else {
#       # for other waves, use the exposure variable as before
#       if (!exposure_col %in% names(df_wide_use) || !exposure_col_next %in% names(df_wide_use)) {
#         cli::cli_alert_warning("Exposure column '{exposure_col}' or '{exposure_col_next}' not found. Skipping 'not_lost' indicator for {t_i}.")
#         next
#       }
#       df_wide_use[[not_lost_col]] <- ifelse(!is.na(df_wide_use[[exposure_col_next]]), 1, 0)
#     }
#
#     # handle missingness for current and future waves
#     rows_lost <- df_wide_use[[not_lost_col]] == 0
#     if (any(rows_lost)) {
#       future_waves <- time_points[(i+1):num_time_points]
#       for (fw in future_waves) {
#         future_cols <- grep(paste0("^", fw, "_"), names(df_wide_use), value = TRUE)
#         df_wide_use[rows_lost, future_cols] <- NA
#       }
#     }
#
#     cli::cli_alert_success("Created '{not_lost_col}' indicator and handled missingness")
#   }
#
#   # Step 2: handle missing outcomes in the final wave
#   cli::cli_h2("Step 2: Handling missing outcomes in the final wave")
#
#   final_outcome_cols <- paste0(final_wave, "_", outcome_vars)
#   final_outcome_cols <- final_outcome_cols[final_outcome_cols %in% names(df_wide_use)]
#
#   if (length(final_outcome_cols) > 0) {
#     missing_final_wave <- rowSums(is.na(df_wide_use[, final_outcome_cols, drop = FALSE])) > 0
#     df_wide_use[missing_final_wave, final_outcome_cols] <- NA
#     cli::cli_alert_success("Handled missing outcomes in the final wave")
#   } else {
#     cli::cli_alert_warning("No outcome columns found for the final wave. Skipping handling of missing outcomes.")
#   }
#
#   # step 3: create lost_in_following_wave indicator if specified
#   if (!is.null(lost_in_following_wave)) {
#     cli::cli_h2("Step 3: Creating lost_in_following_wave indicator")
#     for (t in time_points[1:(num_time_points-1)]) {
#       not_lost_col <- paste0(t, "_", not_lost_in_following_wave)
#       lost_col <- paste0(t, "_", lost_in_following_wave)
#       if (not_lost_col %in% names(df_wide_use)) {
#         df_wide_use[[lost_col]] <- 1 - df_wide_use[[not_lost_col]]
#         cli::cli_alert_success("Created '{lost_col}' indicator")
#       }
#     }
#   }
#
#   # step 4: scale continuous variables across all waves
#   cli::cli_h2("Step 4: Scaling continuous variables across all waves")
#
#   # identify continuous columns to scale
#   continuous_cols <- names(df_wide_use)[sapply(df_wide_use, is.numeric)]
#   continuous_cols <- continuous_cols[!continuous_cols %in% c(continuous_columns_keep, ordinal_columns)]
#   continuous_cols <- continuous_cols[!grepl(paste0("_", not_lost_in_following_wave, "$|_", lost_in_following_wave, "$|_binary$|_na$|_weights$"), continuous_cols)]
#
#   # remove exposure variable from scaling unless scale_exposure is TRUE
#   if (!scale_exposure) {
#     exposure_cols <- paste0(time_points, "_", exposure_var)
#     continuous_cols <- setdiff(continuous_cols, exposure_cols)
#   }
#
#   # scale continuous variables for all waves and remove original columns
#   cols_to_remove <- character(0)
#   for (col in continuous_cols) {
#     df_wide_use[[paste0(col, "_z")]] <- scale(df_wide_use[[col]]) %>% as.vector()
#     cols_to_remove <- c(cols_to_remove, col)
#   }
#
#   # remove original columns that have been z-transformed
#   df_wide_use <- df_wide_use[, !names(df_wide_use) %in% cols_to_remove]
#
#   cli::cli_alert_success("Scaled continuous variables across all waves and removed original columns")
#
#   # Step 5: encode ordinal columns
#   cli::cli_h2("Step 5: Encoding ordinal columns")
#
#   if (!is.null(ordinal_columns)) {
#     df_wide_use <- fastDummies::dummy_cols(
#       df_wide_use,
#       select_columns = ordinal_columns,
#       remove_first_dummy = FALSE,
#       remove_most_frequent_dummy = FALSE,
#       remove_selected_columns = TRUE,  # Change this to TRUE to remove original columns
#       ignore_na = TRUE
#     )
#
#     # rename dummy variables
#     for (ordinal_col in ordinal_columns) {
#       dummy_cols <- grep(paste0("^", ordinal_col, "_"), names(df_wide_use), value = TRUE)
#       df_wide_use <- df_wide_use %>%
#         rename_at(vars(all_of(dummy_cols)), ~ paste0(., "_binary"))
#     }
#
#     cli::cli_alert_success("Encoded ordinal columns and removed original categorical variables")
#   } else {
#     cli::cli_alert_info("No ordinal columns to encode")
#   }
#
#   # Step 6: reordering columns
#   cli::cli_h2("Step 6: Reordering columns")
#
#   new_order <- c("id")  # Start with the ID column
#
#   for (t in time_points) {
#     t_cols <- grep(paste0("^", t, "_"), names(df_wide_use), value = TRUE)
#
#     # separate columns into different categories
#     exposure_col <- paste0(t, "_", exposure_var)
#     not_lost_col <- paste0(t, "_", not_lost_in_following_wave)
#     lost_col <- if(!is.null(lost_in_following_wave)) paste0(t, "_", lost_in_following_wave) else NULL
#
#     # identify z-transformed columns
#     z_cols <- grep("_z$", t_cols, value = TRUE)
#
#     # other columns (excluding z-transformed, exposure, not_lost, and lost)
#     other_cols <- setdiff(t_cols, c(z_cols, exposure_col, not_lost_col, lost_col))
#
#     # order other columns, z-transformed columns, exposure, not_lost, lost (if exists)
#     new_order <- c(new_order, other_cols, z_cols, exposure_col, not_lost_col, lost_col)
#   }
#
#   # remove any NULL values (in case lost_col doesn't exist)
#   new_order <- new_order[!sapply(new_order, is.null)]
#
#   # keep only columns that exist in the dataframe
#   new_order <- intersect(new_order, names(df_wide_use))
#
#   # reorder the dataframe
#   df_wide_use <- df_wide_use[, new_order]
#
#   cli::cli_alert_success("Columns reordered successfully  \U0001F44D")
#
#   # final messages
#   cli::cli_alert_success("Data processing completed successfully  \U0001F44D")
#   cli::cli_h2("Summary")
#   cli::cli_ul(c(
#     "Total rows processed: {nrow(df_wide_use)}",
#     "Total columns: {ncol(df_wide_use)}",
#     "Time points processed: {length(time_points)}"
#   ))
#
#   return(df_wide_use)
# }
