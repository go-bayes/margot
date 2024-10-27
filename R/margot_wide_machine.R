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
#' @param imputation_method A character string specifying the imputation method to use. Options are 'median' (default), 'mice', or 'none'.
#' @param include_exposure_var_baseline Logical indicating whether to include the exposure variable at baseline (t0).
#' @param include_outcome_vars_baseline Logical indicating whether to include outcome variables at baseline (t0).
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
                                imputation_method = 'median',
                                include_exposure_var_baseline = TRUE,
                                include_outcome_vars_baseline = TRUE) {

  start_time <- Sys.time()
  cli::cli_alert_info("Starting data transformation...")

  # Load required packages
  required_packages <- c("dplyr", "tidyr", "cli", "zoo")
  if (imputation_method == 'mice') required_packages <- c(required_packages, "mice")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' needed but not installed.", pkg))
    }
  }

  # Pre-process data
  cli::cli_alert_info("Pre-processing data...")

  # Get number of waves first
  wave_values <- sort(unique(.data[[wave]]))
  y_ <- length(wave_values)
  cli::cli_alert_info(sprintf("Processing %d waves of data...", y_))

  # Ensure no list columns
  list_cols <- sapply(.data, is.list)
  if (any(list_cols)) {
    cli::cli_alert_warning("Converting list columns to character...")
    .data <- .data %>%
      dplyr::mutate(across(where(is.list), ~as.character(.x)))
  }

  # Convert factors to numeric where possible
  cols_to_process <- c(baseline_vars, unlist(exposure_var), outcome_vars, confounder_vars)
  cols_to_process <- unique(cols_to_process[cols_to_process %in% names(.data)])

  .data <- .data %>%
    dplyr::mutate(across(where(is.factor), ~{
      if (all(grepl("^[-]?[0-9]+\\.?[0-9]*$", levels(.x)))) {
        as.numeric(as.character(.x))
      } else {
        .x
      }
    }))

  # Convert wave years to 0-based sequential indices
  cli::cli_alert_info("Converting time indices...")
  wave_lookup <- setNames(seq_along(wave_values) - 1, wave_values)

  .data <- .data %>%
    dplyr::mutate(time = wave_lookup[as.character(.data[[wave]])])

  # Pre-allocate NA indicators if needed
  if (!is.null(make_na_dummies)) {
    cli::cli_alert_info("Creating NA indicators...")
    vars_to_process <- unique(unlist(lapply(make_na_dummies, function(var) {
      switch(var,
             "baseline_vars" = baseline_vars,
             "confounder_vars" = confounder_vars,
             var)
    })))
  }

  # Reshape to wide format
  cli::cli_alert_info("Reshaping to wide format...")
  data_wide <- .data %>%
    dplyr::select(-wave) %>%
    dplyr::arrange(.data[[id]], time) %>%
    tidyr::pivot_wider(
      id_cols = id,
      names_from = time,
      values_from = -c(id, time),
      names_glue = "t{time}_{.value}",
      values_fn = list
    )

  # Unlist all columns that became lists during pivot_wider
  cli::cli_alert_info("Processing list columns from pivot_wider...")
  list_cols <- sapply(data_wide, is.list)
  if (any(list_cols)) {
    data_wide <- data_wide %>%
      dplyr::mutate(across(where(is.list), ~{
        unlist(.x)
      }))
  }

  # Create NA indicators before any imputation
  if (!is.null(make_na_dummies)) {
    cli::cli_alert_info("Creating NA indicators...")

    # Ensure exposure_var is a character vector
    exposure_var <- unlist(exposure_var)

    # Create comprehensive list of variables that need NA indicators
    vars_to_process <- character(0)

    # Process each make_na_dummies entry
    for (dummy_type in make_na_dummies) {
      new_vars <- switch(dummy_type,
                         "baseline_vars" = {
                           # Start with baseline vars
                           vars <- baseline_vars
                           # Add exposure vars if requested
                           if (include_exposure_var_baseline) {
                             vars <- c(vars, exposure_var)
                           }
                           # Add outcome vars if requested
                           if (include_outcome_vars_baseline) {
                             vars <- c(vars, outcome_vars)
                           }
                           vars
                         },
                         "confounder_vars" = confounder_vars,
                         dummy_type)
      vars_to_process <- c(vars_to_process, new_vars)
    }

    # Remove duplicates and NULLs
    vars_to_process <- unique(vars_to_process[!is.null(vars_to_process)])

    cli::cli_alert_info(sprintf("Will create NA indicators for variables: %s",
                                paste(vars_to_process, collapse = ", ")))

    # Create NA indicators for each variable at each appropriate time point
    for (var in vars_to_process) {
      for (t in 0:(y_ - 2)) {  # Exclude final outcome wave
        col_name <- paste0("t", t, "_", var)

        # Only create indicators for existing columns
        if (col_name %in% names(data_wide)) {
          # Determine if this variable should have NA indicator at this time point
          create_indicator <- FALSE

          # Logic for when to create NA indicators
          if (var %in% baseline_vars && t == 0) {
            create_indicator <- TRUE
          } else if (var %in% exposure_var && include_exposure_var_baseline && t == 0) {
            create_indicator <- TRUE
          } else if (var %in% outcome_vars && include_outcome_vars_baseline && t == 0) {
            create_indicator <- TRUE
          } else if (var %in% confounder_vars) {
            create_indicator <- TRUE
          }

          if (create_indicator) {
            # Check if the variable has any missing values
            if (any(is.na(data_wide[[col_name]]))) {
              # Create the NA indicator
              na_col_name <- paste0(col_name, "_na")
              data_wide[[na_col_name]] <- as.integer(is.na(data_wide[[col_name]]))

              cli::cli_alert_success(sprintf("Created NA indicator for %s", col_name))
            }
          }
        }
      }
    }
  }

  # Generate column names
  cli::cli_alert_info("Generating column names...")
  baseline_cols <- paste0("t0_", baseline_vars)
  if (include_exposure_var_baseline) {
    baseline_cols <- c(baseline_cols, paste0("t0_", exposure_var))
  }
  if (include_outcome_vars_baseline) {
    baseline_cols <- c(baseline_cols, paste0("t0_", outcome_vars))
  }
  baseline_cols <- unique(baseline_cols)

  outcome_cols <- paste0("t", y_ - 1, "_", outcome_vars)
  exposure_cols <- paste0("t", 1:(y_ - 2), "_", exposure_var)

  # Perform imputation
  if (imputation_method != 'none') {
    cli::cli_alert_info(sprintf("Starting %s imputation...", imputation_method))

    # Prepare data for imputation
    impute_cols <- baseline_cols[baseline_cols %in% names(data_wide)]

    if (imputation_method == 'mice') {
      tryCatch({
        imp <- mice::mice(data_wide[, impute_cols, drop = FALSE],
                          m = 1,
                          maxit = 5,
                          method = "pmm",
                          printFlag = FALSE)

        data_wide[, impute_cols] <- mice::complete(imp)
        cli::cli_alert_success("MICE imputation completed successfully")
      }, error = function(e) {
        cli::cli_alert_warning(sprintf("MICE failed: %s. Falling back to median imputation.", e$message))
        imputation_method <- 'median'
      })
    }

    if (imputation_method == 'median') {
      for (col in impute_cols) {
        if (is.numeric(data_wide[[col]])) {
          data_wide[[col]][is.na(data_wide[[col]])] <- median(data_wide[[col]], na.rm = TRUE)
        } else {
          mode_val <- names(sort(table(data_wide[[col]]), decreasing = TRUE))[1]
          data_wide[[col]][is.na(data_wide[[col]])] <- mode_val
        }
      }
      cli::cli_alert_success("Median imputation completed")
    }
  }

  # Handle confounders
  if (!is.null(confounder_vars)) {
    cli::cli_alert_info("Processing confounders...")
    for (var in confounder_vars) {
      var_cols <- paste0("t", 0:(y_ - 2), "_", var)
      var_cols <- var_cols[var_cols %in% names(data_wide)]

      # Create a matrix for more efficient processing
      mat <- as.matrix(data_wide[, var_cols, drop = FALSE])

      # Process each row using apply and convert back to numeric
      mat <- t(apply(mat, 1, function(x) {
        x_num <- as.numeric(as.character(x))
        zoo::na.locf(x_num, na.rm = FALSE)
      }))

      # Assign back to data_wide
      data_wide[, var_cols] <- mat
    }
    cli::cli_alert_success("Confounder processing completed")
  }

  # Reorder columns
  cli::cli_alert_info("Reordering columns...")
  new_order <- c(id)

  for (t in 0:(y_ - 1)) {
    t_prefix <- paste0("t", t, "_")
    if (t == 0) {
      wave_vars <- unique(c(
        baseline_vars,
        if (include_exposure_var_baseline) exposure_var else NULL,
        if (include_outcome_vars_baseline) outcome_vars else NULL,
        confounder_vars
      ))
    } else if (t < (y_ - 1)) {
      wave_vars <- unique(c(confounder_vars, outcome_vars, exposure_var))
    } else {
      wave_vars <- outcome_vars
    }

    # Add main columns
    new_cols <- paste0(t_prefix, wave_vars)
    new_cols <- new_cols[new_cols %in% names(data_wide)]

    # Add NA indicator columns
    if (!is.null(make_na_dummies)) {
      for (col in new_cols) {
        # Add NA indicator immediately after its corresponding variable
        na_col <- paste0(col, "_na")
        if (na_col %in% names(data_wide)) {
          new_cols <- append(new_cols, na_col, after = which(new_cols == col))
        }
      }
    }

    new_order <- c(new_order, new_cols)
  }

  # Ensure all columns exist and are unique
  new_order <- unique(new_order[new_order %in% names(data_wide)])

  # Final reordering
  data_wide <- data_wide[, new_order]

  # Report execution time
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "secs")
  cli::cli_alert_success(sprintf("Processing completed in %.2f seconds", as.numeric(execution_time)))

  return(data.frame(data_wide))
}
# Example usage:
# Define variables
# baseline_vars <- c("age", "education", "income")
# exposure_var <- "treatment"
# outcome_vars <- c("health_score", "quality_of_life")
# confounder_vars <- c("stress_level", "exercise_frequency")

# Transform data to wide format with imputation
# df_wide_impute <- margot_wide_machine(
#   .data = longitudinal_data,
#   id = "patient_id",
#   wave = "visit_time",
#   baseline_vars = baseline_vars,
#   exposure_var = exposure_var,
#   outcome_vars = outcome_vars,
#   confounder_vars = confounder_vars,
#   make_na_dummies = c("baseline_vars", "confounder_vars"),
#   imputation_method = "mice"
# )
