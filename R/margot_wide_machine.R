#' Transform longitudinal data to wide format with baseline imputation and optional NA indicators
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
#' @param confounder_vars An optional character vector of time-varying confounder variable names to include without imputation (default is NULL).
#' @param imputation_method A character string specifying the imputation method to use for baseline variables. Options are 'median' (default), 'mice', or 'none'.
#' @param include_exposure_var_baseline Logical indicating whether to include the exposure variable at baseline (t0).
#' @param include_outcome_vars_baseline Logical indicating whether to include outcome variables at baseline (t0).
#' @param extend_baseline Logical indicating whether to include baseline_vars in all subsequent waves (default FALSE).
#' @param include_na_indicators Logical indicating whether to generate NA indicator columns for baseline variables (default TRUE).
#'
#' @return A wide-format data frame with each subject's observations across time points
#'         represented in a single row. Baseline variables, exposure variables at baseline,
#'         and outcome variables at baseline have missing values imputed as specified.
#'         NA indicators are created for variables at baseline only if include_na_indicators is TRUE.
#'         Exposure variables are tracked across waves but are not imputed beyond baseline.
#'         Outcome variables are included only at the final wave unless include_outcome_vars_baseline is TRUE.
#'         Confounders (if any) are included without imputation.
#'
#' @importFrom dplyr mutate across where all_of filter select arrange bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_abort
#' @importFrom mice mice complete
#' @export
margot_wide_machine <- function(.data,
                                id = "id",
                                wave = "wave",
                                baseline_vars,
                                exposure_var,
                                outcome_vars,
                                confounder_vars = NULL,
                                imputation_method = c("median", "mice", "none"),
                                include_exposure_var_baseline = TRUE,
                                include_outcome_vars_baseline = TRUE,
                                extend_baseline = FALSE,
                                include_na_indicators = TRUE) {
  imputation_method <- match.arg(imputation_method)
  start_time <- Sys.time()
  cli::cli_alert_info("starting data transformation...")

  # validate id and wave
  if (!id %in% names(.data) || !wave %in% names(.data)) {
    cli::cli_abort("`{id}` or `{wave}` not found in `.data`.")
  }

  # define waves
  waves <- sort(unique(.data[[wave]]))
  n_waves <- length(waves)
  wave_map <- setNames(seq_along(waves) - 1, waves)

  # add numeric time and sort
  df <- .data %>%
    dplyr::mutate(.time = wave_map[as.character(.data[[wave]])]) %>%
    dplyr::arrange(.data[[id]], .time)

  # split data by time
  df_list <- lapply(0:(n_waves - 1), function(t) {
    vars <- id
    if (t == 0) {
      vars <- c(
        vars, baseline_vars,
        if (include_exposure_var_baseline) exposure_var,
        if (include_outcome_vars_baseline) outcome_vars
      )
    } else if (t < (n_waves - 1)) {
      vars <- c(
        vars, exposure_var,
        if (!is.null(confounder_vars)) confounder_vars,
        if (extend_baseline) baseline_vars
      )
    } else {
      vars <- c(
        vars, outcome_vars,
        if (extend_baseline) baseline_vars
      )
    }
    vars <- unique(vars[vars %in% names(df)])
    df %>%
      dplyr::filter(.time == t) %>%
      dplyr::select(dplyr::all_of(vars)) %>%
      dplyr::mutate(time = t)
  })
  df_filt <- dplyr::bind_rows(df_list)

  # pivot to wide
  cli::cli_alert_info("reshaping to wide format...")
  data_wide <- df_filt %>%
    tidyr::pivot_wider(
      id_cols = id,
      names_from = time,
      values_from = -c(id, time),
      names_glue = "t{time}_{.value}"
    )

  # add NA indicators
  if (include_na_indicators) {
    cli::cli_alert_info("creating NA indicators at baseline...")
    base_vars <- unique(c(
      baseline_vars,
      if (include_exposure_var_baseline) exposure_var,
      if (include_outcome_vars_baseline) outcome_vars
    ))
    base_cols <- paste0("t0_", base_vars)
    for (col in intersect(base_cols, names(data_wide))) {
      if (any(is.na(data_wide[[col]]))) {
        na_col <- paste0(col, "_na")
        data_wide[[na_col]] <- as.integer(is.na(data_wide[[col]]))
        cli::cli_alert_success(sprintf("created NA indicator for %s", col))
      }
    }
  }

  # impute baseline if requested
  if (imputation_method != "none") {
    cli::cli_alert_info(sprintf("performing %s imputation at baseline...", imputation_method))
    imp_cols <- intersect(
      paste0("t0_", unique(c(
        baseline_vars,
        if (include_exposure_var_baseline) exposure_var,
        if (include_outcome_vars_baseline) outcome_vars
      ))),
      names(data_wide)
    )
    if (imputation_method == "mice") {
      # ensure mice prints its progress
      cli::cli_alert_info("running mice imputation; progress will appear below:")
      old_opt <- options(mice.trace = TRUE)
      on.exit(options(old_opt), add = TRUE)
      imp <- mice::mice(
        data_wide[imp_cols],
        m = 1,
        maxit = 5,
        method = "pmm",
        printFlag = TRUE
      )
      data_wide[imp_cols] <- mice::complete(imp)
      cli::cli_alert_success("mice imputation completed")
    }
    if (imputation_method == "median" || !exists("imp")) {
      for (col in imp_cols) {
        if (is.numeric(data_wide[[col]])) {
          data_wide[[col]][is.na(data_wide[[col]])] <- median(data_wide[[col]], na.rm = TRUE)
        } else {
          mode_val <- names(sort(table(data_wide[[col]]), decreasing = TRUE))[1]
          data_wide[[col]][is.na(data_wide[[col]])] <- mode_val
        }
      }
      cli::cli_alert_success("median imputation completed")
    }
  }

  # reorder columns
  cli::cli_alert_info("reordering columns...")
  final_order <- id
  for (t in 0:(n_waves - 1)) {
    prefix <- paste0("t", t, "_")
    if (t == 0) {
      cols <- intersect(
        paste0(prefix, unique(c(
          baseline_vars,
          if (include_exposure_var_baseline) exposure_var,
          if (include_outcome_vars_baseline) outcome_vars
        ))),
        names(data_wide)
      )
      if (include_na_indicators) {
        na_cols <- paste0(cols, "_na")
        interleaved <- unlist(lapply(cols, function(x) c(x, na_cols[na_cols %in% names(data_wide)])))
        final_order <- c(final_order, interleaved)
      } else {
        final_order <- c(final_order, cols)
      }
    } else if (t < (n_waves - 1)) {
      vars_t <- c(
        exposure_var,
        if (!is.null(confounder_vars)) confounder_vars,
        if (extend_baseline) baseline_vars
      )
      cols <- intersect(paste0(prefix, vars_t), names(data_wide))
      final_order <- c(final_order, cols)
    } else {
      cols <- intersect(paste0(prefix, outcome_vars), names(data_wide))
      final_order <- c(final_order, cols)
    }
  }
  final_order <- unique(final_order[final_order %in% names(data_wide)])
  data_wide <- data_wide[, final_order]

  cli::cli_alert_success(sprintf(
    "completed in %.2f seconds",
    difftime(Sys.time(), start_time, units = "secs")
  ))
  return(as.data.frame(data_wide))
}

# margot_wide_machine <- function(.data,
#                                 id = "id",
#                                 wave = "wave",
#                                 baseline_vars,
#                                 exposure_var,
#                                 outcome_vars,
#                                 confounder_vars = NULL,
#                                 imputation_method = c("median", "mice", "none"),
#                                 include_exposure_var_baseline = TRUE,
#                                 include_outcome_vars_baseline = TRUE,
#                                 extend_baseline = FALSE,
#                                 include_na_indicators = TRUE) {
#   imputation_method <- match.arg(imputation_method)
#   start_time <- Sys.time()
#   cli::cli_alert_info("starting data transformation...")
#
#   # validate id and wave columns
#   if (!id %in% names(.data) || !wave %in% names(.data)) {
#     cli::cli_abort("`{id}` or `{wave}` not found in `.data`.")
#   }
#
#   # identify waves and map to 0-based
#   waves <- sort(unique(.data[[wave]]))
#   n_waves <- length(waves)
#   wave_map <- setNames(seq_along(waves) - 1, waves)
#
#   # add numeric time variable
#   df <- .data %>%
#     dplyr::mutate(.time = wave_map[as.character(.data[[wave]])]) %>%
#     dplyr::arrange(.data[[id]], .time)
#
#   # build list of per-wave subsets
#   df_list <- lapply(0:(n_waves - 1), function(t) {
#     vars <- id
#     if (t == 0) {
#       vars <- c(vars, baseline_vars,
#                 if (include_exposure_var_baseline) exposure_var,
#                 if (include_outcome_vars_baseline) outcome_vars)
#     } else if (t < (n_waves - 1)) {
#       vars <- c(vars, exposure_var,
#                 if (!is.null(confounder_vars)) confounder_vars,
#                 if (extend_baseline) baseline_vars)
#     } else {
#       vars <- c(vars, outcome_vars,
#                 if (extend_baseline) baseline_vars)
#     }
#     vars <- unique(vars[vars %in% names(df)])
#     df %>%
#       dplyr::filter(.time == t) %>%
#       dplyr::select(dplyr::all_of(c(vars))) %>%
#       dplyr::mutate(time = t)
#   })
#   df_filt <- dplyr::bind_rows(df_list)
#
#   # pivot to wide format
#   cli::cli_alert_info("reshaping to wide format...")
#   data_wide <- df_filt %>%
#     tidyr::pivot_wider(
#       id_cols = id,
#       names_from = time,
#       values_from = -c(id, time),
#       names_glue = "t{time}_{.value}"
#     )
#
#   # baseline NA indicators if requested
#   if (include_na_indicators) {
#     cli::cli_alert_info("creating NA indicators at baseline...")
#     base_vars <- unique(c(baseline_vars,
#                           if (include_exposure_var_baseline) exposure_var,
#                           if (include_outcome_vars_baseline) outcome_vars))
#     base_cols <- paste0("t0_", base_vars)
#     for (col in intersect(base_cols, names(data_wide))) {
#       if (any(is.na(data_wide[[col]]))) {
#         na_col <- paste0(col, "_na")
#         data_wide[[na_col]] <- as.integer(is.na(data_wide[[col]]))
#         cli::cli_alert_success(sprintf("created NA indicator for %s", col))
#       }
#     }
#   }
#
#   # impute baseline vars if requested
#   if (imputation_method != 'none') {
#     cli::cli_alert_info(sprintf("performing %s imputation at baseline...", imputation_method))
#     imp_cols <- intersect(paste0("t0_", unique(c(baseline_vars,
#                                                  if (include_exposure_var_baseline) exposure_var,
#                                                  if (include_outcome_vars_baseline) outcome_vars))),
#                           names(data_wide))
#     if (imputation_method == 'mice') {
#       try({
#         imp <- mice::mice(data_wide[imp_cols], m = 1, maxit = 5, method = 'pmm', printFlag = FALSE)
#         data_wide[imp_cols] <- mice::complete(imp)
#         cli::cli_alert_success("mice imputation completed")
#       }, silent = TRUE)
#     }
#     if (imputation_method == 'median' || !exists('imp')) {
#       for (col in imp_cols) {
#         if (is.numeric(data_wide[[col]])) {
#           data_wide[[col]][is.na(data_wide[[col]])] <- median(data_wide[[col]], na.rm = TRUE)
#         } else {
#           mode_val <- names(sort(table(data_wide[[col]]), decreasing = TRUE))[1]
#           data_wide[[col]][is.na(data_wide[[col]])] <- mode_val
#         }
#       }
#       cli::cli_alert_success("median imputation completed")
#     }
#   }
#
#   # reorder columns: interleave baseline vars & indicators if included, then wave-specific
#   cli::cli_alert_info("reordering columns...")
#   final_order <- id
#   for (t in 0:(n_waves - 1)) {
#     prefix <- paste0("t", t, "_")
#     if (t == 0) {
#       cols <- intersect(paste0(prefix, unique(c(baseline_vars,
#                                                 if (include_exposure_var_baseline) exposure_var,
#                                                 if (include_outcome_vars_baseline) outcome_vars))),
#                         names(data_wide))
#       if (include_na_indicators) {
#         na_cols <- paste0(cols, "_na")
#         interleaved <- unlist(lapply(cols, function(x) c(x, na_cols[na_cols %in% names(data_wide)])))
#         final_order <- c(final_order, interleaved)
#       } else {
#         final_order <- c(final_order, cols)
#       }
#     } else if (t < (n_waves - 1)) {
#       vars_t <- c(exposure_var,
#                   if (!is.null(confounder_vars)) confounder_vars,
#                   if (extend_baseline) baseline_vars)
#       cols <- intersect(paste0(prefix, vars_t), names(data_wide))
#       final_order <- c(final_order, cols)
#     } else {
#       cols <- intersect(paste0(prefix, outcome_vars), names(data_wide))
#       final_order <- c(final_order, cols)
#     }
#   }
#   final_order <- unique(final_order[final_order %in% names(data_wide)])
#   data_wide <- data_wide[, final_order]
#
#   cli::cli_alert_success(sprintf("completed in %.2f seconds", difftime(Sys.time(), start_time, units='secs')))
#   return(as.data.frame(data_wide))
# }


# old
# margot_wide_machine <- function(.data,
#                                 id = "id",
#                                 wave = "wave",
#                                 baseline_vars,
#                                 exposure_var,
#                                 outcome_vars,
#                                 confounder_vars = NULL,
#                                 imputation_method = 'median',
#                                 include_exposure_var_baseline = TRUE,
#                                 include_outcome_vars_baseline = TRUE) {
#
#   start_time <- Sys.time()
#   cli::cli_alert_info("starting data transformation...")
#
#   # pre-process data
#   cli::cli_alert_info("pre-processing data...")
#
#   # get number of waves first
#   wave_values <- sort(unique(.data[[wave]]))
#   y_ <- length(wave_values)
#   cli::cli_alert_info(sprintf("processing %d waves of data...", y_))
#
#   # ensure no list columns
#   list_cols <- sapply(.data, is.list)
#   if (any(list_cols)) {
#     cli::cli_alert_warning("converting list columns to character...")
#     .data <- dplyr::mutate(.data, dplyr::across(dplyr::where(is.list), ~as.character(.x)))
#   }
#
#   # convert factors to numeric where possible
#   cols_to_process <- c(baseline_vars, exposure_var, outcome_vars, confounder_vars)
#   cols_to_process <- unique(cols_to_process[cols_to_process %in% names(.data)])
#
#   .data <- dplyr::mutate(.data, dplyr::across(dplyr::all_of(cols_to_process), ~{
#     if (is.factor(.x) && all(grepl("^[-]?[0-9]+\\.?[0-9]*$", levels(.x)))) {
#       as.numeric(as.character(.x))
#     } else {
#       .x
#     }
#   }))
#
#   # convert wave values to 0-based sequential indices
#   cli::cli_alert_info("converting time indices...")
#   wave_lookup <- setNames(seq_along(wave_values) - 1, wave_values)
#
#   .data <- dplyr::mutate(.data, time = wave_lookup[as.character(.data[[wave]])])
#
#   # filter data for each time point
#   data_subsets <- list()
#
#   for (t in unique(.data$time)) {
#
#     if (t == 0) {
#       vars_to_select <- c(id, wave, "time", baseline_vars)
#       if (include_exposure_var_baseline) vars_to_select <- c(vars_to_select, exposure_var)
#       if (include_outcome_vars_baseline) vars_to_select <- c(vars_to_select, outcome_vars)
#     } else if (t > 0 && t < (y_ - 1)) {
#       vars_to_select <- c(id, wave, "time", exposure_var)
#       if (!is.null(confounder_vars)) vars_to_select <- c(vars_to_select, confounder_vars)
#     } else if (t == (y_ - 1)) {
#       vars_to_select <- c(id, wave, "time", outcome_vars)
#     }
#
#     vars_in_data <- vars_to_select[vars_to_select %in% names(.data)]
#
#     data_t <- dplyr::filter(.data, time == t) %>%
#       dplyr::select(dplyr::all_of(vars_in_data))
#
#     data_subsets[[as.character(t)]] <- data_t
#   }
#
#   .data_filtered <- dplyr::bind_rows(data_subsets)
#
#   # reshape to wide format
#   cli::cli_alert_info("reshaping to wide format...")
#   data_wide <- dplyr::select(.data_filtered, -wave) %>%
#     dplyr::arrange(.data[[id]], time) %>%
#     tidyr::pivot_wider(
#       id_cols = id,
#       names_from = time,
#       values_from = -c(id, time),
#       names_glue = "t{time}_{.value}"
#     )
#
#   # make NA indicators for all variables at baseline **only if they have missing values**
#   cli::cli_alert_info("creating NA indicators for variables at baseline (only if necessary)...")
#   baseline_all_vars <- c(baseline_vars)
#   if (include_exposure_var_baseline) {
#     baseline_all_vars <- c(baseline_all_vars, exposure_var)
#   }
#   if (include_outcome_vars_baseline) {
#     baseline_all_vars <- c(baseline_all_vars, outcome_vars)
#   }
#   baseline_all_vars <- unique(baseline_all_vars)
#   baseline_cols <- paste0("t0_", baseline_all_vars)
#   baseline_cols <- baseline_cols[baseline_cols %in% names(data_wide)]
#
#   for (col in baseline_cols) {
#     # **check if the column has any NA values before creating the indicator**
#     if (any(is.na(data_wide[[col]]))) {
#       na_col_name <- paste0(col, "_na")
#       data_wide[[na_col_name]] <- as.integer(is.na(data_wide[[col]]))
#       cli::cli_alert_success(sprintf("created NA indicator for %s", col))
#     } else {
#       cli::cli_alert_info(sprintf("no missing values in %s; skipping NA indicator creation.", col))
#     }
#   }
#
#   # prepare columns for imputation at baseline
#   impute_cols <- baseline_cols  # all variables at baseline
#
#   # do imputation only at baseline for specified variables
#   if (imputation_method != 'none' && length(impute_cols) > 0) {
#     cli::cli_alert_info(sprintf("starting %s imputation at baseline...", imputation_method))
#
#     if (imputation_method == 'mice') {
#       tryCatch({
#         imp <- mice::mice(data_wide[, impute_cols, drop = FALSE],
#                           m = 1,
#                           maxit = 5,
#                           method = "pmm",
#                           printFlag = TRUE)
#
#         data_wide[, impute_cols] <- mice::complete(imp)
#         cli::cli_alert_success("mice imputation at baseline completed successfully")
#       }, error = function(e) {
#         cli::cli_alert_warning(sprintf("mice failed: %s. falling back to median imputation.", e$message))
#         imputation_method <- 'median'
#       })
#     }
#
#     if (imputation_method == 'median') {
#       for (col in impute_cols) {
#         if (is.numeric(data_wide[[col]])) {
#           data_wide[[col]][is.na(data_wide[[col]])] <- median(data_wide[[col]], na.rm = TRUE)
#         } else {
#           mode_val <- names(sort(table(data_wide[[col]]), decreasing = TRUE))[1]
#           data_wide[[col]][is.na(data_wide[[col]])] <- mode_val
#         }
#       }
#       cli::cli_alert_success("median imputation at baseline completed")
#     }
#   }
#
#   # reorder columns
#   cli::cli_alert_info("reordering columns...")
#   new_order <- c(id)
#
#   for (t in 0:(y_ - 1)) {
#     t_prefix <- paste0("t", t, "_")
#     if (t == 0) {
#       wave_vars <- unique(c(
#         baseline_vars,
#         if (include_exposure_var_baseline) exposure_var else NULL,
#         if (include_outcome_vars_baseline) outcome_vars else NULL
#       ))
#     } else if (t > 0 && t < (y_ - 1)) {
#       wave_vars <- unique(c(exposure_var, confounder_vars))
#     } else if (t == (y_ - 1)) {
#       wave_vars <- outcome_vars
#     }
#
#     # add main columns
#     new_cols <- paste0(t_prefix, wave_vars)
#     new_cols <- new_cols[new_cols %in% names(data_wide)]
#
#     # add NA indicator columns for baseline variables **only if they were created**
#     if (t == 0) {
#       na_cols <- paste0(new_cols, "_na")
#       na_cols <- na_cols[na_cols %in% names(data_wide)]
#
#       # interleave new_cols and na_cols
#       cols_with_na <- c()
#       for (col in new_cols) {
#         cols_with_na <- c(cols_with_na, col)
#         na_col <- paste0(col, "_na")
#         if (na_col %in% na_cols) {
#           cols_with_na <- c(cols_with_na, na_col)
#         }
#       }
#       new_order <- c(new_order, cols_with_na)
#     } else {
#       new_order <- c(new_order, new_cols)
#     }
#   }
#
#   # ensure all columns exist and are unique
#   new_order <- unique(new_order[new_order %in% names(data_wide)])
#
#   # final reordering
#   data_wide <- data_wide[, new_order]
#
#   # report execution time
#   end_time <- Sys.time()
#   execution_time <- difftime(end_time, start_time, units = "secs")
#   cli::cli_alert_success(sprintf("processing completed in %.2f seconds", as.numeric(execution_time)))
#
#   return(data.frame(data_wide))
# }
