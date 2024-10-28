# Transform longitudinal data to wide format with baseline imputation and NA indicators
#'
#' This function transforms longitudinal data from long format to wide format,
#' ensuring that baseline measurements are correctly labeled and included.
#' It handles multiple observations per subject across an indefinite number of waves,
#' and allows for the specification of baseline variables, exposure variables,
#' outcome variables, and time-varying confounders.
#'
#' @param .data A data frame containing the longitudinal data in long format.
#' @param id The name of the ID column identifying subjects (default is "id").
#' @param wave The name of the wave/time column (default is "wave").
#' @param baseline_vars A character vector of baseline variable names to be included at t0.
#' @param exposure_var A character string specifying the name of the exposure variable to be tracked across time.
#' @param outcome_vars A character vector of outcome variable names to be tracked across time.
#' @param confounder_vars An optional character vector of time-varying confounder variable names to be carried forward (default is NULL).
#' @param imputation_method A character string specifying the imputation method to use for baseline variables. Options are 'median' (default), 'mice', or 'none'.
#' @param include_exposure_var_baseline Logical indicating whether to include the exposure variable at baseline (t0).
#' @param include_outcome_vars_baseline Logical indicating whether to include outcome variables at baseline (t0).
#'
#' @return A wide-format data frame with each subject's observations across time points
#'         represented in a single row. Baseline variables, exposure variables at baseline,
#'         and outcome variables at baseline (if included) have missing values imputed as specified.
#'         NA indicators are created for variables at baseline only if they have missing values.
#'         Exposure variables are tracked across waves but are not imputed beyond baseline.
#'         Outcome variables are included only at the final wave
#'         unless `include_outcome_vars_baseline` is `TRUE`. Confounders (if any) are carried forward using last observation carried forward (LOCF).
#'
#' @details
#' Key functionalities:
#' - **Imputation at Baseline**: Missing values are imputed at baseline (`t0`) for:
#'   - `baseline_vars`
#'   - `exposure_var` (if `include_exposure_var_baseline = TRUE`)
#'   - `outcome_vars` (if `include_outcome_vars_baseline = TRUE`)
#'   - 'median': For numeric variables, missing values are imputed with the median.
#'     For categorical variables, missing values are imputed with the mode.
#'   - 'mice': Multiple Imputation by Chained Equations is used.
#'     If MICE fails, the function falls back to median/mode imputation.
#'   - 'none': No imputation is performed.
#' - **NA Indicators**: NA indicator variables are created **only** for baseline variables
#'   that have missing values. Each such variable at baseline will have a corresponding
#'   NA indicator with the suffix `_na`.
#' - **Exposure Variables**: Tracked across waves but never imputed beyond baseline.
#' - **Outcome Variables**: Included only at the final wave unless `include_outcome_vars_baseline` is `TRUE`.
#' - **Confounder Variables**: If specified, these are carried forward across waves using LOCF.
#' - **Variable Inclusion per Wave**:
#'   - **Baseline (`t0`)**: Includes `baseline_vars`, exposure variables (if included), outcome variables (if included), and their NA indicators (only if necessary).
#'   - **Waves `t1` to `t(y_-2)`**: Include only the exposure variables (and confounders if specified).
#'   - **Final Wave (`t(y_-1)`)**: Includes only the outcome variables.
#'
#' @export
#'
#' @import dplyr tidyr cli zoo
#'
#' @examples
#' # Define variables
#' baseline_vars <- c("age", "education", "income")
#' exposure_var <- "treatment"
#' outcome_vars <- c("health_score", "quality_of_life")
#' confounder_vars <- c("stress_level", "exercise_frequency")
#'
#' # Transform data to wide format with baseline imputation
#' df_wide_impute <- margot_wide_machine(
#'   data_long,
#'   id = "patient_id",
#'   wave = "visit_time",
#'   baseline_vars = baseline_vars,
#'   exposure_var = exposure_var,
#'   outcome_vars = outcome_vars,
#'   confounder_vars = confounder_vars,
#'   imputation_method = "mice",
#'   include_exposure_var_baseline = TRUE,
#'   include_outcome_vars_baseline = TRUE
#' )
margot_wide_machine <- function(.data,
                                id = "id",
                                wave = "wave",
                                baseline_vars,
                                exposure_var,
                                outcome_vars,
                                confounder_vars = NULL,
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
  cols_to_process <- c(baseline_vars, exposure_var, outcome_vars, confounder_vars)
  cols_to_process <- unique(cols_to_process[cols_to_process %in% names(.data)])

  .data <- .data %>%
    dplyr::mutate(across(all_of(cols_to_process), ~{
      if (is.factor(.x) && all(grepl("^[-]?[0-9]+\\.?[0-9]*$", levels(.x)))) {
        as.numeric(as.character(.x))
      } else {
        .x
      }
    }))

  # Convert wave values to 0-based sequential indices
  cli::cli_alert_info("Converting time indices...")
  wave_lookup <- setNames(seq_along(wave_values) - 1, wave_values)

  .data <- .data %>%
    dplyr::mutate(time = wave_lookup[as.character(.data[[wave]])])

  # Filter data for each time point
  data_subsets <- list()

  for (t in unique(.data$time)) {

    if (t == 0) {
      vars_to_select <- c(id, wave, "time", baseline_vars)
      if (include_exposure_var_baseline) vars_to_select <- c(vars_to_select, exposure_var)
      if (include_outcome_vars_baseline) vars_to_select <- c(vars_to_select, outcome_vars)
    } else if (t > 0 && t < (y_ - 1)) {
      vars_to_select <- c(id, wave, "time", exposure_var)
      if (!is.null(confounder_vars)) vars_to_select <- c(vars_to_select, confounder_vars)
    } else if (t == (y_ - 1)) {
      vars_to_select <- c(id, wave, "time", outcome_vars)
    }

    vars_in_data <- vars_to_select[vars_to_select %in% names(.data)]

    data_t <- .data %>%
      filter(time == t) %>%
      select(all_of(vars_in_data))

    data_subsets[[as.character(t)]] <- data_t
  }

  .data_filtered <- bind_rows(data_subsets)

  # Reshape to wide format
  cli::cli_alert_info("Reshaping to wide format...")
  data_wide <- .data_filtered %>%
    dplyr::select(-wave) %>%
    dplyr::arrange(.data[[id]], time) %>%
    tidyr::pivot_wider(
      id_cols = id,
      names_from = time,
      values_from = -c(id, time),
      names_glue = "t{time}_{.value}"
    )

  # Create NA indicators for all variables at baseline **only if they have missing values**
  cli::cli_alert_info("Creating NA indicators for variables at baseline (only if necessary)...")
  baseline_all_vars <- c(baseline_vars)
  if (include_exposure_var_baseline) {
    baseline_all_vars <- c(baseline_all_vars, exposure_var)
  }
  if (include_outcome_vars_baseline) {
    baseline_all_vars <- c(baseline_all_vars, outcome_vars)
  }
  baseline_all_vars <- unique(baseline_all_vars)
  baseline_cols <- paste0("t0_", baseline_all_vars)
  baseline_cols <- baseline_cols[baseline_cols %in% names(data_wide)]

  for (col in baseline_cols) {
    # **Check if the column has any NA values before creating the indicator**
    if (any(is.na(data_wide[[col]]))) {
      na_col_name <- paste0(col, "_na")
      data_wide[[na_col_name]] <- as.integer(is.na(data_wide[[col]]))
      cli::cli_alert_success(sprintf("Created NA indicator for %s", col))
    } else {
      cli::cli_alert_info(sprintf("No missing values in %s; skipping NA indicator creation.", col))
    }
  }

  # Prepare columns for imputation at baseline
  impute_cols <- baseline_cols  # All variables at baseline

  # Perform imputation only at baseline for specified variables
  if (imputation_method != 'none' && length(impute_cols) > 0) {
    cli::cli_alert_info(sprintf("Starting %s imputation at baseline...", imputation_method))

    if (imputation_method == 'mice') {
      tryCatch({
        imp <- mice::mice(data_wide[, impute_cols, drop = FALSE],
                          m = 1,
                          maxit = 5,
                          method = "pmm",
                          printFlag = FALSE)

        data_wide[, impute_cols] <- mice::complete(imp)
        cli::cli_alert_success("MICE imputation at baseline completed successfully")
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
      cli::cli_alert_success("Median imputation at baseline completed")
    }
  }

  # Carry forward confounders if specified
  if (!is.null(confounder_vars)) {
    cli::cli_alert_info("Carrying forward confounders...")
    for (var in confounder_vars) {
      var_cols <- paste0("t", 1:(y_ - 1), "_", var)  # Corrected to y_ -1 to include all relevant waves
      var_cols <- var_cols[var_cols %in% names(data_wide)]

      # Process each row
      data_wide[, var_cols] <- t(apply(data_wide[, var_cols, drop = FALSE], 1, function(x) {
        zoo::na.locf(x, na.rm = FALSE)
      }))
    }
    cli::cli_alert_success("Confounder carry forward completed")
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
        if (include_outcome_vars_baseline) outcome_vars else NULL
      ))
    } else if (t > 0 && t < (y_ - 1)) {
      wave_vars <- unique(c(exposure_var, confounder_vars))
    } else if (t == (y_ - 1)) {
      wave_vars <- outcome_vars
    }

    # Add main columns
    new_cols <- paste0(t_prefix, wave_vars)
    new_cols <- new_cols[new_cols %in% names(data_wide)]

    # Add NA indicator columns for baseline variables **only if they were created**
    if (t == 0) {
      na_cols <- paste0(new_cols, "_na")
      na_cols <- na_cols[na_cols %in% names(data_wide)]

      # Interleave new_cols and na_cols
      cols_with_na <- c()
      for (col in new_cols) {
        cols_with_na <- c(cols_with_na, col)
        na_col <- paste0(col, "_na")
        if (na_col %in% na_cols) {
          cols_with_na <- c(cols_with_na, na_col)
        }
      }
      new_order <- c(new_order, cols_with_na)
    } else {
      new_order <- c(new_order, new_cols)
    }
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
