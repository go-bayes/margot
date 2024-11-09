#' Process Longitudinal Data for Multiple Waves
#'
#' @description
#' This function processes longitudinal data for an arbitrary number of waves (e.g., t0, t1, t2, ...).
#' It handles attrition, scales continuous variables, optionally encodes ordinal variables, and manages exposure variables.
#'
#' @param df_wide A wide-format dataframe containing longitudinal data for multiple waves.
#' @param ordinal_columns A character vector of column names to be treated as ordinal and dummy-coded.
#' @param continuous_columns_keep A character vector of continuous column names to keep without scaling.
#' @param exposure_vars A character vector of exposure variable names. These variables will be used to determine attrition.
#' @param scale_exposure Logical. If TRUE, scales the exposure variable(s). Default is FALSE.
#' @param not_lost_in_following_wave Name of the 'not lost' indicator. Default is "not_lost_following_wave".
#' @param lost_in_following_wave Name of the 'lost' indicator. If NULL, no 'lost' indicator is created.
#' @param remove_selected_columns Logical. If TRUE, removes selected columns after encoding. Default is TRUE.
#' @param time_point_prefixes A character vector of time point prefixes. If NULL, they will be inferred from the data.
#' @param time_point_regex A regex pattern to identify time points. Used if time_point_prefixes is NULL.
#' @param save_observed_y Logical. If TRUE, retains observed outcome values in the final wave even if lost. Default is FALSE.
#' @param censored_if_any_lost Logical. Determines how to treat the 'not_lost_in_following_wave' indicator.
#' If TRUE, sets 'not_lost_in_following_wave' to 0 if any value is NA in the following wave.
#' If FALSE, applies custom logic based on 'save_observed_y'.
#'
#' @return A processed dataframe suitable for use in longitudinal analyses with multiple waves.
#'
#' @details
#' The function performs the following steps:
#' 1. Identifies all time points in the dataset.
#' 2. Creates 'not_lost' indicators based on the exposure variable(s) in subsequent waves, excluding the final wave.
#' 3. Applies attrition logic across all waves.
#' 4. Scales continuous variables across all waves, removing original non-scaled columns.
#' 5. Optionally encodes ordinal columns.
#' 6. Handles missing outcomes in the final wave based on 'save_observed_y'.
#' 7. Reorders columns, placing exposure and 'not_lost' indicators appropriately.
#'
#' Censoring Behavior:
#' The function implements a recursive censoring mechanism across waves:
#' 1. For each wave t (from t=0 to τ-1), a "not_lost_in_following_wave" indicator is created
#'    based on missingness in wave t+1.
#' 2. If an observation has missing values at wave t+1:
#'    - The "not_lost_in_following_wave" indicator at wave t is set to 0
#'    - All data for this observation in waves > t are set to NA
#' 3. This censoring cascades forward: once an observation is censored at time t,
#'    it remains censored for all future waves.
#'
#' Example of censoring behavior:
#' ```r
#' # Input data
#' df <- data.frame(
#'   id = 1:3,
#'   t0_exposure = c(1, 1, 1),
#'   t1_exposure = c(1, NA, 1),
#'   t2_exposure = c(1, NA, NA),
#'   t0_outcome = c(10, 10, 10),
#'   t1_outcome = c(20, NA, 20),
#'   t2_outcome = c(30, NA, NA)
#' )
#'
#' # After processing:
#' # Row 1: Never censored, all data retained
#' # Row 2: Censored at t1, everything from t1 onward is NA
#' # Row 3: Censored at t2, everything from t2 onward is NA
#' ```
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
#'   exposure_vars = c("treatment"),
#'   scale_exposure = FALSE,
#'   not_lost_in_following_wave = "not_lost",
#'   lost_in_following_wave = NULL,
#'   remove_selected_columns = FALSE,
#'   time_point_prefixes = c("t0", "t1", "t2", "t3"),
#'   save_observed_y = TRUE,
#'   censored_if_any_lost = FALSE
#' )
#' @export
margot_process_longitudinal_data_wider <- function(
    df_wide,
    ordinal_columns = NULL,
    continuous_columns_keep = NULL,
    exposure_vars = NULL,
    scale_exposure = FALSE,
    not_lost_in_following_wave = "not_lost_following_wave",
    lost_in_following_wave = NULL,
    remove_selected_columns = TRUE,
    time_point_prefixes = NULL,
    time_point_regex = NULL,
    save_observed_y = FALSE,
    censored_if_any_lost = TRUE
) {
  cli::cli_h1("Longitudinal Data Processing")
  cli::cli_alert_info("Starting data processing for longitudinal data with multiple time points")

  # Validate censored_if_any_lost
  if (!is.logical(censored_if_any_lost) || length(censored_if_any_lost) != 1) {
    stop("The 'censored_if_any_lost' parameter must be either TRUE or FALSE.")
  }

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

  # Determine outcome variables
  final_wave <- time_points[num_time_points]
  final_wave_cols <- grep(paste0("^", final_wave, "_"), names(df_wide_use), value = TRUE)
  outcome_vars <- gsub(paste0("^", final_wave, "_"), "", final_wave_cols)
  cli::cli_alert_info("Using all variables in the final wave as outcomes.")

  # Step 1: Create 'not_lost_in_following_wave' indicators and handle missingness, excluding final wave
  cli::cli_h2("Step 1: Creating 'not_lost_in_following_wave' indicators and handling missingness")

  for (i in seq_len(num_time_points - 1)) {
    t_i <- time_points[i]
    t_i_plus1 <- time_points[i + 1]

    not_lost_col <- paste0(t_i, "_", not_lost_in_following_wave)

    # Generate exposure column names for current and next wave
    exposure_cols <- paste0(t_i, "_", exposure_vars)
    exposure_cols_next <- paste0(t_i_plus1, "_", exposure_vars)

    # Check if exposure columns exist in the next wave
    exposure_missing_next <- !exposure_cols_next %in% names(df_wide_use)

    if (any(exposure_missing_next)) {
      # If exposure variables are missing in the next wave, attempt to use outcome variables
      outcome_cols_next <- paste0(t_i_plus1, "_", outcome_vars)
      outcome_cols_next <- outcome_cols_next[outcome_cols_next %in% names(df_wide_use)]

      if (length(outcome_cols_next) == 0) {
        cli::cli_alert_warning("No exposure or outcome columns found for {t_i_plus1}. Skipping 'not_lost' indicator for {t_i}.")
        next
      }

      # Compute counts of observed and missing outcome variables
      num_observed <- rowSums(!is.na(df_wide_use[, outcome_cols_next, drop = FALSE]))
      num_missing <- rowSums(is.na(df_wide_use[, outcome_cols_next, drop = FALSE]))
      total_outcomes <- length(outcome_cols_next)

      all_observed <- num_observed == total_outcomes
      all_missing <- num_missing == total_outcomes
      some_observed <- num_observed > 0 & num_observed < total_outcomes

      # Initialize not_lost_col with NA
      df_wide_use[[not_lost_col]] <- NA

      # Apply the logic based on 'save_observed_y' and 'censored_if_any_lost'
      if (censored_if_any_lost == TRUE) {
        # Set to 1 if all outcomes observed, else 0
        df_wide_use[[not_lost_col]][all_observed] <- 1
        df_wide_use[[not_lost_col]][!all_observed] <- 0
      } else if (censored_if_any_lost == FALSE) {
        # When 'save_observed_y' is TRUE
        if (save_observed_y == TRUE) {
          df_wide_use[[not_lost_col]][all_observed] <- 1
          df_wide_use[[not_lost_col]][all_missing] <- 0
          df_wide_use[[not_lost_col]][some_observed] <- NA
        } else {
          # When 'save_observed_y' is FALSE
          df_wide_use[[not_lost_col]][all_observed] <- 1
          df_wide_use[[not_lost_col]][!all_observed] <- 0
        }
      }
    } else {
      # Determine 'not_lost' based on all exposure_vars_next being present
      not_lost_condition <- rowSums(!is.na(df_wide_use[, exposure_cols_next, drop = FALSE])) == length(exposure_vars)
      df_wide_use[[not_lost_col]] <- ifelse(not_lost_condition, 1, 0)
    }

    # Handle missingness for current and future waves
    rows_lost <- df_wide_use[[not_lost_col]] == 0
    rows_na <- is.na(df_wide_use[[not_lost_col]])

    if (any(rows_lost | rows_na)) {
      future_waves <- time_points[(i + 1):num_time_points]
      for (fw in future_waves) {
        future_cols <- grep(paste0("^", fw, "_"), names(df_wide_use), value = TRUE)
        if (save_observed_y && fw == final_wave) {
          # Exclude outcome variables from being set to NA
          outcome_cols_fw <- paste0(fw, "_", outcome_vars)
          future_cols <- setdiff(future_cols, outcome_cols_fw)
        }
        # Set future columns to NA for rows_lost and rows_na
        df_wide_use[rows_lost | rows_na, future_cols] <- NA
      }
    }

    cli::cli_alert_success("Created '{not_lost_col}' indicator and handled missingness for time point {t_i}")
  }

  # Step 2: Handle missing outcomes in the final wave
  cli::cli_h2("Step 2: Handling missing outcomes in the final wave")

  final_outcome_cols <- paste0(final_wave, "_", outcome_vars)
  final_outcome_cols <- final_outcome_cols[final_outcome_cols %in% names(df_wide_use)]

  if (length(final_outcome_cols) > 0) {
    if (!save_observed_y) {
      # Set missing outcomes in the final wave to NA
      missing_final_wave <- rowSums(is.na(df_wide_use[, final_outcome_cols, drop = FALSE])) > 0
      df_wide_use[missing_final_wave, final_outcome_cols] <- NA
      cli::cli_alert_success("Set missing outcomes in the final wave to NA")
    } else {
      cli::cli_alert_info("'save_observed_y' is TRUE; retaining observed outcome values in the final wave")
    }
  } else {
    cli::cli_alert_warning("No outcome columns found for the final wave. Skipping handling of missing outcomes.")
  }

  # Step 3: Create lost_in_following_wave indicator if specified, excluding final wave
  if (!is.null(lost_in_following_wave)) {
    cli::cli_h2("Step 3: Creating lost_in_following_wave indicator")
    for (i in seq_len(num_time_points - 1)) {
      t <- time_points[i]
      not_lost_col <- paste0(t, "_", not_lost_in_following_wave)
      lost_col <- paste0(t, "_", lost_in_following_wave)
      if (not_lost_col %in% names(df_wide_use)) {
        df_wide_use[[lost_col]] <- ifelse(is.na(df_wide_use[[not_lost_col]]), NA, 1 - df_wide_use[[not_lost_col]])
        cli::cli_alert_success("Created '{lost_col}' indicator for time point {t}")
      }
    }
  }

  # Step 4: Scale continuous variables across all waves
  cli::cli_h2("Step 4: Scaling continuous variables across all waves")

  # Identify continuous columns to scale
  continuous_cols <- names(df_wide_use)[sapply(df_wide_use, is.numeric)]
  continuous_cols <- continuous_cols[!continuous_cols %in% c(continuous_columns_keep, ordinal_columns)]
  continuous_cols <- continuous_cols[!grepl(paste0("_", not_lost_in_following_wave, "$|_", lost_in_following_wave, "$|_binary$|_na$|_weights$"), continuous_cols)]

  # Remove exposure variables from scaling unless scale_exposure is TRUE
  if (!scale_exposure && !is.null(exposure_vars)) {
    exposure_cols_all <- paste0(rep(time_points, each = length(exposure_vars)), "_", exposure_vars)
    continuous_cols <- setdiff(continuous_cols, exposure_cols_all)
  }

  # Scale continuous variables for all waves and remove original columns
  cols_to_remove <- character(0)
  for (col in continuous_cols) {
    df_wide_use[[paste0(col, "_z")]] <- scale(df_wide_use[[col]]) %>% as.vector()
    cols_to_remove <- c(cols_to_remove, col)
  }

  # Remove original columns that have been z-transformed
  df_wide_use <- df_wide_use[, !names(df_wide_use) %in% cols_to_remove]

  cli::cli_alert_success("Scaled continuous variables across all waves and removed original columns")

  # Step 5: Encode ordinal columns
  cli::cli_h2("Step 5: Encoding ordinal columns")

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
        dplyr::rename_at(vars(all_of(dummy_cols)), ~ paste0(., "_binary"))
    }

    cli::cli_alert_success("Encoded ordinal columns and handled original categorical variables based on 'remove_selected_columns'")
  } else {
    cli::cli_alert_info("No ordinal columns to encode")
  }

  # Step 6: Reordering columns
  cli::cli_h2("Step 6: Reordering columns")

  new_order <- c("id")  # Start with the ID column

  for (t in time_points) {
    t_cols <- grep(paste0("^", t, "_"), names(df_wide_use), value = TRUE)

    # Separate columns into different categories
    if (!is.null(exposure_vars)) {
      exposure_cols <- paste0(t, "_", exposure_vars)
    } else {
      exposure_cols <- character(0)
    }

    not_lost_col <- paste0(t, "_", not_lost_in_following_wave)
    lost_col <- if (!is.null(lost_in_following_wave) && t != final_wave) paste0(t, "_", lost_in_following_wave) else NULL

    # Identify z-transformed columns
    z_cols <- grep("_z$", t_cols, value = TRUE)

    # Other columns (excluding z-transformed, exposure, not_lost, and lost)
    other_cols <- setdiff(t_cols, c(z_cols, exposure_cols, not_lost_col, lost_col))

    # Order other columns, z-transformed columns, exposure, not_lost, lost (if exists)
    new_order <- c(new_order, other_cols, z_cols, exposure_cols, not_lost_col, lost_col)
  }

  # Remove any NULL values (in case lost_col doesn't exist)
  new_order <- new_order[!sapply(new_order, is.null)]

  # Keep only columns that exist in the dataframe
  new_order <- intersect(new_order, names(df_wide_use))

  # Reorder the dataframe
  df_wide_use <- df_wide_use[, new_order]

  cli::cli_alert_success("Columns reordered successfully \U0001F44D")

  # Final messages
  cli::cli_alert_success("Data processing completed successfully \U0001F44D")
  cli::cli_h2("Summary")
  cli::cli_ul(c(
    paste("Total rows processed:", nrow(df_wide_use)),
    paste("Total columns:", ncol(df_wide_use)),
    paste("Time points processed:", length(time_points))
  ))

  return(df_wide_use)
}
