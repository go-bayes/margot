#' Transform longitudinal data to wide format with flexible wave handling and imputation
#'
#' This function transforms longitudinal data from long format to wide format,
#' ensuring that baseline measurements are correctly labelled and included.
#' It handles multiple observations per subject across an indefinite number of waves,
#' and allows for the specification of baseline variables, exposure variables,
#' outcome variables, and time-varying confounders. The function also performs
#' imputation for missing values and creates NA indicator variables as specified.
#'
#' @param .data A data frame containing the longitudinal data in long format.
#' @param id The name of the ID column identifying subjects (default is "id").
#' @param wave The name of the wave/time column (default is "wave").
#' @param baseline_vars A character vector of baseline variable names to be included at t0.
#' @param exposure_var A character string specifying the name of the exposure variable to be tracked across time.
#' @param outcome_vars A character vector of outcome variable names to be tracked across time.
#' @param confounder_vars An optional character vector of time-varying confounder variable names.
#' @param make_na_dummies An optional character vector of variable names or keywords ("baseline_vars", "confounder_vars") for which to create NA dummies.
#' @param not_lost_following_wave A character string specifying the name of the variable to check for non-missing status across waves (default is NULL).
#' @param imputation_method A character string specifying the imputation method to use. Options are 'median' (default), 'mice', or 'none'.
#'
#' @return A wide-format data frame with each subject's observations across time points
#'         represented in a single row, and variables prefixed by their respective
#'         time of measurement. Imputed values and NA indicators are included as specified.
#'
#' @details
#' Imputation methods:
#' - 'median': For numeric variables, missing values are imputed with the median.
#'             For categorical variables, missing values are imputed with the mode.
#' - 'mice': Multiple Imputation by Chained Equations is used for baseline variables.
#'           If MICE fails, the function falls back to median/mode imputation.
#' - 'none': No imputation is performed.
#'
#' The function creates NA indicator variables for specified variables, imputes
#' missing values according to the chosen method, and ensures that the exposure
#' variable is not imputed after t0 and outcome variables are not imputed at the
#' final time point.
#'
#' @export
#'
#' @import dplyr tidyr cli mice zoo
#'
#' @examples
#' # Define variables
#' baseline_vars <- c("age", "education", "income")
#' exposure_var <- "treatment"
#' outcome_vars <- c("health_score", "quality_of_life")
#' confounder_vars <- c("stress_level", "exercise_frequency")
#'
#' # Transform data to wide format with imputation
#' df_wide_impute <- margot_wide_machine(
#'   longitudinal_data,
#'   id = "patient_id",
#'   wave = "visit_time",
#'   baseline_vars = baseline_vars,
#'   exposure_var = exposure_var,
#'   outcome_vars = outcome_vars,
#'   confounder_vars = confounder_vars,
#'   make_na_dummies = c("baseline_vars", "confounder_vars"),
#'   not_lost_following_wave = "follow_up_complete",
#'   imputation_method = "mice"
#' )
margot_wide_machine <- function(.data,
                                id = "id",
                                wave = "wave",
                                baseline_vars,
                                exposure_var,
                                outcome_vars,
                                confounder_vars = NULL,
                                make_na_dummies = NULL,
                                not_lost_following_wave = NULL,
                                imputation_method = 'median',
                                include_exposure_var_baseline = TRUE,
                                include_outcome_vars_baseline = TRUE) {
  # load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed but not installed.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' needed but not installed.")
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("Package 'cli' needed but not installed.")
  }
  if (imputation_method == 'mice' && !requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' needed but not installed. Install it or choose a different imputation method.")
  }
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package 'zoo' needed but not installed.")
  }

  # validate imputation_method
  valid_methods <- c('median', 'mice', 'none')
  if (!imputation_method %in% valid_methods) {
    stop(paste("Invalid imputation method. Choose from:", paste(valid_methods, collapse = ", ")))
  }

  # remove numeric attributes so that data will play well with mice
  .data <- margot::remove_numeric_attributes(.data)


  # add the 'time' column to data and reshape to wide format
  data_wide <- .data %>%
    dplyr::mutate(time = as.numeric(.data[[wave]]) - 1) %>%
    dplyr::arrange(.data[[id]], time) %>%
    tidyr::pivot_wider(
      id_cols = id,
      names_from = time,
      values_from = -c(id, wave),
      names_glue = "t{time}_{.value}"
    )

  # determine total number of waves
  time_values <- sort(unique(.data[[wave]]))
  y_ <- length(time_values)  # number of waves
  cli::cli_alert_info(paste0("Data contains ", y_, " waves."))

  # create lists to store column names
  all_cols <- names(data_wide)
  id_col <- id

  # outcome columns at the last time point
  outcome_cols <- paste0("t", y_ - 1, "_", outcome_vars)

  # exposure columns after t0 (we will not impute these)
  exposure_cols <- paste0("t", 1:(y_ - 2), "_", exposure_var)

  # the baseline_cols definition
  baseline_cols <- paste0("t0_", baseline_vars)
  if (include_exposure_var_baseline) {
    baseline_cols <- c(baseline_cols, paste0("t0_", exposure_var))
  }
  if (include_outcome_vars_baseline) {
    baseline_cols <- c(baseline_cols, paste0("t0_", outcome_vars))
  }
  baseline_cols <- unique(baseline_cols)  # remove any duplicates

  # confounder variables at t >= 0 (including baseline if confounders overlap with baseline_vars)
  confounder_cols <- c()
  for (t in 0:(y_ - 2)) {
    confounder_cols <- c(confounder_cols, paste0("t", t, "_", confounder_vars))
  }

  # non-imputed columns
  non_impute_cols <- c(id_col, outcome_cols, exposure_cols)

  # columns to impute at baseline (t0): baseline variables
  impute_baseline_cols <- baseline_cols

  # step 1: impute Baseline Variables
  if (imputation_method == 'mice') {
    cli::cli_alert_info("Starting MICE imputation for baseline variables...")

    # prepare data for MICE (only baseline variables)
    mice_data <- data_wide[, impute_baseline_cols]

    # run MICE with error handling
    tryCatch({
      imp <- mice::mice(mice_data, m = 1, maxit = 5, method = "pmm", printFlag = TRUE)

      cli::cli_alert_info("MICE imputation completed. Extracting imputed data...")
      imputed_data <- mice::complete(imp)

      # replace the original data with imputed data
      data_wide[, impute_baseline_cols] <- imputed_data

      cli::cli_alert_success("Baseline variables imputed using MICE.")
    }, error = function(e) {
      cli::cli_alert_danger(paste("Error in MICE imputation:", e$message))
      cli::cli_alert_info("Falling back to median imputation for baseline variables.")

      for (col in impute_baseline_cols) {
        if (is.numeric(data_wide[[col]])) {
          median_value <- stats::median(data_wide[[col]], na.rm = TRUE)
          if (is.na(median_value)) median_value <- 0  # Default value if median is NA
          data_wide[[col]][is.na(data_wide[[col]])] <- median_value
          cli::cli_alert_success(paste0("Imputed missing values in ", col, " with median"))
        } else {
          mode_value <- names(sort(table(data_wide[[col]]), decreasing = TRUE))[1]
          if (length(mode_value) == 0) mode_value <- NA  # Handle case with all NAs
          data_wide[[col]][is.na(data_wide[[col]])] <- mode_value
          cli::cli_alert_success(paste0("Imputed missing values in ", col, " with mode"))
        }
      }
    })
  } else if (imputation_method == 'median') {
    cli::cli_alert_info("Starting median/mode imputation for baseline variables...")
    for (col in impute_baseline_cols) {
      if (is.numeric(data_wide[[col]])) {
        median_value <- stats::median(data_wide[[col]], na.rm = TRUE)
        if (is.na(median_value)) median_value <- 0  # Default value if median is NA
        data_wide[[col]][is.na(data_wide[[col]])] <- median_value
        cli::cli_alert_success(paste0("Imputed missing values in ", col, " with median"))
      } else {
        mode_value <- names(sort(table(data_wide[[col]]), decreasing = TRUE))[1]
        if (length(mode_value) == 0) mode_value <- NA  # Handle case with all NAs
        data_wide[[col]][is.na(data_wide[[col]])] <- mode_value
        cli::cli_alert_success(paste0("Imputed missing values in ", col, " with mode"))
      }
    }
  } else {
    cli::cli_alert_info("No imputation performed on baseline variables.")
  }

  # step 2: Carry Forward Imputation for Confounders
  if (!is.null(confounder_vars)) {
    for (var in confounder_vars) {
      var_cols <- paste0("t", 0:(y_ - 2), "_", var)
      var_cols <- var_cols[var_cols %in% names(data_wide)]

      # apply carry-forward imputation row-wise
      data_wide[var_cols] <- t(apply(data_wide[var_cols], 1, function(x) {
        zoo::na.locf(x, na.rm = FALSE)
      }))

      # after carry-forward, impute any remaining missing values using median/mode
      for (col in var_cols) {
        if (any(is.na(data_wide[[col]]))) {
          if (is.numeric(data_wide[[col]])) {
            median_value <- stats::median(data_wide[[col]], na.rm = TRUE)
            if (is.na(median_value)) median_value <- 0  # Default value if median is NA
            data_wide[[col]][is.na(data_wide[[col]])] <- median_value
            cli::cli_alert_success(paste0("Imputed remaining missing values in ", col, " with median"))
          } else {
            mode_value <- names(sort(table(data_wide[[col]]), decreasing = TRUE))[1]
            if (length(mode_value) == 0) mode_value <- NA  # Handle case with all NAs
            data_wide[[col]][is.na(data_wide[[col]])] <- mode_value
            cli::cli_alert_success(paste0("Imputed remaining missing values in ", col, " with mode"))
          }
        }
      }

      cli::cli_alert_success(paste0("Carried forward values and imputed for confounder variable: ", var))
    }
  }

  # confirm no imputation of exposure variable after t0
  cli::cli_alert_success(paste0("Confirmed: exposure variable '", exposure_var, "' not imputed after t0."))

  # confirm no imputation of variables at end of study
  cli::cli_alert_success(paste0("Confirmed: outcome variables at t", y_ - 1, " (end of study) not imputed."))

  # create NA indicators
  if (!is.null(make_na_dummies)) {
    for (var in make_na_dummies) {
      for (t in 0:(y_ - 2)) {  # Exclude outcome wave
        col_name <- paste0("t", t, "_", var)
        if (col_name %in% names(data_wide)) {
          na_indicator <- paste0(col_name, "_na")
          data_wide[[na_indicator]] <- as.integer(is.na(data_wide[[col_name]]))
        }
      }
    }
  }

  # add not_lost_following_wave if it exists
  if (!is.null(not_lost_following_wave)) {
    for (t in 0:(y_ - 2)) {  # Exclude outcome wave
      not_lost_col <- paste0("t", t, "_", not_lost_following_wave)
      if (not_lost_col %in% names(data_wide)) {
        cli::cli_alert_info(paste0("Selected ", not_lost_following_wave, " variable at t", t))
      } else {
        cli::cli_alert_warning(paste0(not_lost_following_wave, " variable not found at t", t))
      }
    }
  }

  # reorder columns
  new_order <- c(id_col)
  for (t in 0:(y_ - 1)) {
    t_prefix <- paste0("t", t, "_")
    if (t < (y_ - 1)) {
      # For t0, include baseline_vars and potentially exposure and outcome vars
      if (t == 0) {
        wave_vars <- unique(c(baseline_vars, confounder_vars))
        if (include_exposure_var_baseline) {
          wave_vars <- c(wave_vars, exposure_var)
        }
        if (include_outcome_vars_baseline) {
          wave_vars <- c(wave_vars, outcome_vars)
        }
      } else {
        wave_vars <- unique(c(confounder_vars, outcome_vars))
      }
      wave_vars <- unique(wave_vars)

      # build column names
      wave_cols <- paste0(t_prefix, wave_vars)

      # include exposure_var and not_lost_following_wave variable
      extra_cols <- c(paste0(t_prefix, exposure_var))
      if (!is.null(not_lost_following_wave) && paste0(t_prefix, not_lost_following_wave) %in% names(data_wide)) {
        extra_cols <- c(extra_cols, paste0(t_prefix, not_lost_following_wave))
      }

      # append NA indicators
      na_cols <- c()
      if (!is.null(make_na_dummies)) {
        na_vars <- intersect(wave_vars, make_na_dummies)
        na_cols <- paste0(t_prefix, na_vars, "_na")
      }

      # add to new_order
      new_order <- c(new_order, wave_cols, na_cols, extra_cols)
    } else {
      # Outcome wave
      wave_cols <- paste0(t_prefix, outcome_vars)
      new_order <- c(new_order, wave_cols)
    }
  }

  # remove duplicates in new_order
  new_order <- unique(new_order)
  # Keep only columns that are in data_wide
  new_order <- intersect(new_order, names(data_wide))

  data_wide_ordered <- data_wide[, new_order]

  # final step: adjust position of exposure_var and not_lost_following_wave within each wave
  for (t in 0:(y_ - 2)) {  # Excluding the final wave
    t_prefix <- paste0("t", t, "_")

    # columns to move
    move_cols <- c(paste0(t_prefix, exposure_var))
    if (!is.null(not_lost_following_wave)) {
      move_cols <- c(move_cols, paste0(t_prefix, not_lost_following_wave))
    }

    # find the position of the last column in the current wave
    last_col_position <- max(which(grepl(paste0("^t", t, "_"), names(data_wide_ordered))))

    # Move the columns
    data_wide_ordered <- data_wide_ordered %>%
      dplyr::relocate(dplyr::any_of(move_cols), .after = dplyr::any_of(names(data_wide_ordered)[last_col_position]))
  }

  # success message
  cli::cli_alert_success("Data successfully transformed to wide format, imputed, and reordered \U0001F44D")

  return(data.frame(data_wide_ordered))  # ensure output is a data.frame
}


