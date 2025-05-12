#' process longitudinal dyadic data for multiple waves
#'
#' @description
#' this function processes longitudinal data (wide format) across multiple waves,
#' handling dyadic censoring: if one partner in a dyad is lost at wave $t$, the entire
#' dyad is set to lost at wave $t$. all subsequent wave data are then set to NA.
#'
#' @param df_wide a wide-format dataframe containing longitudinal data.
#' @param relationship_id a string naming the column that identifies dyads. defaults to "NULL".
#' @param ordinal_columns a character vector of column names to be treated as ordinal and dummy-coded.
#' @param continuous_columns_keep a character vector of continuous column names to keep without scaling.
#' @param exposure_vars a character vector of exposure variable names that determine attrition.
#' @param scale_exposure logical. if TRUE, scales the exposure variable(s). default is FALSE.
#' @param scale_continuous logical. if TRUE, scales continuous variables. if FALSE, no variables are scaled.
#'   default is TRUE.
#' @param not_lost_in_following_wave character string with the suffix for the "not lost" indicator.
#'   default is "not_lost_following_wave".
#' @param lost_in_following_wave character string with the suffix for the "lost" indicator.
#'   if NULL, no 'lost' indicator is created.
#' @param remove_selected_columns logical. if TRUE, removes selected columns after encoding. default is TRUE.
#' @param time_point_prefixes a character vector of time point prefixes. if NULL, inferred from the data.
#' @param time_point_regex a regex pattern for identifying time points. used if time_point_prefixes is NULL.
#' @param save_observed_y logical. if TRUE, retains observed outcome values in the final wave even if lost. default FALSE.
#' @param censored_if_any_lost logical. if TRUE, sets "not_lost_in_following_wave" = 0 if any data are NA in wave $t+1$.
#'
#' @return a processed dataframe suitable for longitudinal analyses.
#'
#' @details
#' the dyadic logic occurs after computing each wave's "not_lost" indicator.
#' if any person in a dyad is flagged lost (0), then all partners in that dyad
#' also get flagged lost at the same wave.
#'
#' the function prints CLI messages summarising how many dyads and how many participants
#' are lost at each wave, and which dyad IDs were forced lost.
#'
#' @importFrom dplyr group_by mutate ungroup select filter summarise if_else rename_at vars
#' @importFrom fastDummies dummy_cols
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_warning cli_alert_success cli_ul
#'
#' @examples
#' # see the tests in previous examples, or adapt your own.
#'
#' @export
margot_process_longitudinal_data_wider <- function(
    df_wide,
    relationship_id = "NULL",
    ordinal_columns = NULL,
    continuous_columns_keep = NULL,
    exposure_vars = NULL,
    scale_exposure = FALSE,
    scale_continuous = TRUE,
    not_lost_in_following_wave = "not_lost_following_wave",
    lost_in_following_wave = NULL,
    remove_selected_columns = TRUE,
    time_point_prefixes = NULL,
    time_point_regex = NULL,
    save_observed_y = FALSE,
    censored_if_any_lost = TRUE
) {
  cli::cli_h1("longitudinal dyadic data processing")

  # validate censored_if_any_lost
  if (!is.logical(censored_if_any_lost) || length(censored_if_any_lost) != 1) {
    stop("the 'censored_if_any_lost' parameter must be either TRUE or FALSE.")
  }

  # identify time points
  if (is.null(time_point_prefixes)) {
    if (is.null(time_point_regex)) {
      time_point_regex <- "^(t\\d+)_.*$"
    }
    matched_cols <- grep(time_point_regex, colnames(df_wide), value = TRUE)
    time_points <- unique(gsub(time_point_regex, "\\1", matched_cols))
    # sort by numeric wave number
    time_points <- time_points[order(as.numeric(gsub("t", "", time_points)))]
  } else {
    time_points <- time_point_prefixes
  }
  num_time_points <- length(time_points)

  cli::cli_alert_info("detected {num_time_points} time points: {paste(time_points, collapse = ', ')}")

  # prepare the dataframe for processing
  df_wide_use <- df_wide

  # determine outcome variables from the final wave
  final_wave <- time_points[num_time_points]
  final_wave_cols <- grep(paste0("^", final_wave, "_"), names(df_wide_use), value = TRUE)
  outcome_vars <- gsub(paste0("^", final_wave, "_"), "", final_wave_cols)
  cli::cli_alert_info("using all final-wave columns as potential outcomes: {paste0(outcome_vars, collapse=', ')}")

  # step 1: create 'not_lost_in_following_wave' and handle missingness, excluding final wave
  cli::cli_h2("step 1: creating 'not_lost_in_following_wave' indicators & applying dyadic censoring")

  for (i in seq_len(num_time_points - 1)) {
    t_i         <- time_points[i]
    t_i_plus1   <- time_points[i + 1]
    not_lost_col <- paste0(t_i, "_", not_lost_in_following_wave)

    # build next-wave exposure colnames
    if (!is.null(exposure_vars) && length(exposure_vars) > 0) {
      exposure_cols_next <- paste0(t_i_plus1, "_", exposure_vars)
      exposure_missing_next <- !exposure_cols_next %in% names(df_wide_use)
    } else {
      exposure_cols_next <- character(0)
      exposure_missing_next <- FALSE
    }

    # if next wave's exposure is missing, attempt outcome-based logic
    if (any(exposure_missing_next)) {
      outcome_cols_next <- paste0(t_i_plus1, "_", outcome_vars)
      outcome_cols_next <- outcome_cols_next[outcome_cols_next %in% names(df_wide_use)]

      if (length(outcome_cols_next) == 0) {
        cli::cli_alert_warning(
          "no next-wave exposure or outcome columns for {t_i_plus1}. skipping '{not_lost_col}'."
        )
        next
      }

      num_observed <- rowSums(!is.na(df_wide_use[, outcome_cols_next, drop = FALSE]))
      num_missing  <- rowSums(is.na(df_wide_use[, outcome_cols_next, drop = FALSE]))
      total_outcomes <- length(outcome_cols_next)

      all_observed  <- (num_observed == total_outcomes)
      all_missing   <- (num_missing  == total_outcomes)
      some_observed <- (num_observed >  0 & num_observed < total_outcomes)

      df_wide_use[[not_lost_col]] <- NA_real_

      if (censored_if_any_lost) {
        df_wide_use[[not_lost_col]][all_observed]  <- 1
        df_wide_use[[not_lost_col]][!all_observed] <- 0
      } else {
        if (save_observed_y) {
          df_wide_use[[not_lost_col]][all_observed]  <- 1
          df_wide_use[[not_lost_col]][all_missing]    <- 0
          df_wide_use[[not_lost_col]][some_observed]  <- NA
        } else {
          df_wide_use[[not_lost_col]][all_observed]  <- 1
          df_wide_use[[not_lost_col]][!all_observed] <- 0
        }
      }
    } else {
      if (length(exposure_cols_next) > 0) {
        not_lost_condition <- rowSums(!is.na(df_wide_use[, exposure_cols_next, drop = FALSE])) == length(exposure_vars)
        df_wide_use[[not_lost_col]] <- ifelse(not_lost_condition, 1, 0)
      } else {
        next_wave_cols <- grep(paste0("^", t_i_plus1, "_"), names(df_wide_use), value = TRUE)
        is_all_missing <- rowSums(!is.na(df_wide_use[, next_wave_cols, drop = FALSE])) == 0
        df_wide_use[[not_lost_col]] <- ifelse(is_all_missing, 0, 1)
      }
    }

    # dyadic logic: if any partner is lost, force entire dyad to lost
    if (!is.null(relationship_id) && relationship_id %in% names(df_wide_use)) {
      df_wide_use <- df_wide_use %>%
        dplyr::group_by(.data[[relationship_id]]) %>%
        dplyr::mutate(
          any_lost_in_dyad = any(.data[[not_lost_col]] == 0, na.rm = TRUE),
          !!not_lost_col := dplyr::if_else(any_lost_in_dyad, 0L, .data[[not_lost_col]])
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-any_lost_in_dyad)

      dyad_lost_summary <- df_wide_use %>%
        dplyr::group_by(.data[[relationship_id]]) %>%
        dplyr::summarise(
          all_lost = any(.data[[not_lost_col]] == 0, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(all_lost)

      n_dyads_lost <- nrow(dyad_lost_summary)
      n_participants_lost <- sum(df_wide_use[[not_lost_col]] == 0, na.rm = TRUE)

      if (n_dyads_lost > 0) {
        cli::cli_alert_info(
          "wave {t_i}: {n_dyads_lost} dyad(s) lost => {n_participants_lost} participant(s) flagged as lost.\n  dyad IDs lost: {paste(dyad_lost_summary[[relationship_id]], collapse=', ')}"
        )
      } else {
        cli::cli_alert_info("wave {t_i}: no dyads lost at this step.")
      }
    }

    # censor future-wave columns for rows with not_lost_col == 0 or NA
    rows_lost <- df_wide_use[[not_lost_col]] == 0
    rows_na   <- is.na(df_wide_use[[not_lost_col]])

    if (any(rows_lost | rows_na)) {
      future_waves <- time_points[(i + 1):num_time_points]
      for (fw in future_waves) {
        future_cols <- grep(paste0("^", fw, "_"), names(df_wide_use), value = TRUE)
        if (save_observed_y && fw == final_wave) {
          outcome_cols_fw <- paste0(fw, "_", outcome_vars)
          future_cols <- setdiff(future_cols, outcome_cols_fw)
        }
        df_wide_use[rows_lost | rows_na, future_cols] <- NA
      }
    }

    cli::cli_alert_success("created '{not_lost_col}' and applied future-wave censoring for wave {t_i}")
  }

  # step 2: handle missing outcomes in the final wave
  cli::cli_h2("step 2: handling missing outcomes in final wave")

  final_outcome_cols <- paste0(final_wave, "_", outcome_vars)
  final_outcome_cols <- final_outcome_cols[final_outcome_cols %in% names(df_wide_use)]

  if (length(final_outcome_cols) > 0) {
    if (!save_observed_y) {
      missing_final_wave <- rowSums(is.na(df_wide_use[, final_outcome_cols, drop = FALSE])) > 0
      df_wide_use[missing_final_wave, final_outcome_cols] <- NA
      cli::cli_alert_success("set partially-missing outcomes in the final wave to NA.")
    } else {
      cli::cli_alert_info("'save_observed_y' is TRUE => retaining observed outcome values in the final wave.")
    }
  } else {
    cli::cli_alert_warning("no outcome columns found for the final wave => skipping final-wave outcome handling.")
  }

  # step 3: create lost_in_following_wave indicator if requested
  if (!is.null(lost_in_following_wave)) {
    cli::cli_h2("step 3: creating lost_in_following_wave indicators")
    for (i in seq_len(num_time_points - 1)) {
      t   <- time_points[i]
      nlc <- paste0(t, "_", not_lost_in_following_wave)
      lc  <- paste0(t, "_", lost_in_following_wave)
      if (nlc %in% names(df_wide_use)) {
        df_wide_use[[lc]] <- ifelse(is.na(df_wide_use[[nlc]]), NA, 1 - df_wide_use[[nlc]])
        cli::cli_alert_success("created '{lc}' for wave {t}")
      }
    }
  }

  # step 4: scale continuous variables
  if (scale_continuous) {
    cli::cli_h2("step 4: scaling continuous variables")
    continuous_cols <- names(df_wide_use)[sapply(df_wide_use, is.numeric)]

    # exclude specified columns from scaling
    exclude_regex <- paste0(
      "_", not_lost_in_following_wave, "$|_",
      lost_in_following_wave, "$|_binary$|_na$|_weights$"
    )
    continuous_cols <- setdiff(continuous_cols, c(continuous_columns_keep, ordinal_columns))
    continuous_cols <- continuous_cols[!grepl(exclude_regex, continuous_cols)]

    # if not scaling exposures, exclude them
    if (!scale_exposure && !is.null(exposure_vars)) {
      all_exposures <- paste0(rep(time_points, each = length(exposure_vars)), "_", exposure_vars)
      continuous_cols <- setdiff(continuous_cols, all_exposures)
    }

    cols_to_remove <- character(0)
    for (col in continuous_cols) {
      df_wide_use[[paste0(col, "_z")]] <- scale(df_wide_use[[col]]) %>% as.vector()
      cols_to_remove <- c(cols_to_remove, col)
    }

    df_wide_use <- df_wide_use[, !names(df_wide_use) %in% cols_to_remove]
    cli::cli_alert_success("scaled {length(cols_to_remove)} continuous variables across all waves and removed originals.")
  } else {
    cli::cli_alert_info("scaling of continuous variables disabled by user.")
  }

  # step 5: encode ordinal columns
  cli::cli_h2("step 5: encoding ordinal columns")
  if (!is.null(ordinal_columns)) {
    df_wide_use <- fastDummies::dummy_cols(
      df_wide_use,
      select_columns = ordinal_columns,
      remove_first_dummy = FALSE,
      remove_most_frequent_dummy = FALSE,
      remove_selected_columns = remove_selected_columns,
      ignore_na = TRUE
    )
    for (oc in ordinal_columns) {
      oc_dummy <- grep(paste0("^", oc, "_"), names(df_wide_use), value = TRUE)
      df_wide_use <- df_wide_use %>%
        dplyr::rename_at(dplyr::vars(dplyr::all_of(oc_dummy)), ~ paste0(., "_binary"))
    }
    cli::cli_alert_success("encoded ordinal columns: {paste(ordinal_columns, collapse=', ')}")
  } else {
    cli::cli_alert_info("no ordinal columns to encode.")
  }

  # step 6: reorder columns
  cli::cli_h2("step 6: reordering columns")
  new_order <- character(0)
  if ("id" %in% names(df_wide_use)) {
    new_order <- c(new_order, "id")
  }
  if (!is.null(relationship_id) && relationship_id %in% names(df_wide_use) && relationship_id != "id") {
    new_order <- c(new_order, relationship_id)
  }
  for (t in time_points) {
    t_cols <- grep(paste0("^", t, "_"), names(df_wide_use), value = TRUE)
    if (!is.null(exposure_vars)) {
      exposure_cols <- paste0(t, "_", exposure_vars)
    } else {
      exposure_cols <- character(0)
    }
    not_lost_col <- paste0(t, "_", not_lost_in_following_wave)
    lost_col     <- if (!is.null(lost_in_following_wave) && t != final_wave) paste0(t, "_", lost_in_following_wave) else NULL
    z_cols    <- grep("_z$", t_cols, value = TRUE)
    other_cols <- setdiff(t_cols, c(z_cols, exposure_cols, not_lost_col, lost_col))
    new_order <- c(new_order, other_cols, z_cols, exposure_cols, not_lost_col, lost_col)
  }
  new_order <- new_order[!sapply(new_order, is.null)]
  new_order <- intersect(new_order, names(df_wide_use))
  df_wide_use <- df_wide_use[, new_order]
  cli::cli_alert_success("columns reordered successfully \U0001F44D")

  cli::cli_h2("data processing summary")
  cli::cli_ul(c(
    paste("total rows processed:", nrow(df_wide_use)),
    paste("total columns:", ncol(df_wide_use)),
    paste("time points processed:", length(time_points))
  ))
  cli::cli_alert_success("data processing completed \U0001F44D")

  return(df_wide_use)
}
#' Process longitudinal dyadic data in wide format with censoring by missing exposure
#'
#' @description
#' This function processes longitudinal data (wide format) across multiple waves,
#' handling dyadic censoring and optional censoring when the exposure is missing.
#'
#' If `censor_if_missing_exposure = TRUE`, any record with a missing exposure at wave t+1
#' is considered lost at wave t and all subsequent wave data are set to NA.  Dyadic logic
#' ensures that if any member of a dyad is lost, the entire dyad is censored.
#'
#' @param df_wide A data.frame in wide format containing time-point–prefixed columns (e.g., t0_x).
#' @param relationship_id Column name identifying dyads; if present, dyadic censoring is applied. Default: "NULL".
#' @param ordinal_columns Character vector of ordinal column bases to dummy-encode after processing.
#' @param continuous_columns_keep Character vector of continuous column names to retain without scaling.
#' @param exposure_vars Character vector of exposure base names (without time prefixes).
#' @param scale_exposure Logical; if TRUE, scales exposure variables. Default: FALSE.
#' @param scale_continuous Logical; if TRUE, scales continuous variables. Default: TRUE.
#' @param censor_if_missing_exposure Logical; if TRUE, missing exposures at next wave cause censoring. Default: TRUE.
#' @param not_lost_in_following_wave Suffix for the not-lost indicator. Default: "not_lost_following_wave".
#' @param lost_in_following_wave Suffix for the lost indicator; if NULL no lost indicator is added. Default: NULL.
#' @param remove_selected_columns Logical; if TRUE, removes original columns after dummy encoding. Default: TRUE.
#' @param time_point_prefixes Optional vector of time-point prefixes (e.g., c("t0","t1")). If NULL, inferred.
#' @param time_point_regex Regex pattern to identify time-point prefixes. Used if time_point_prefixes is NULL.
#' @param save_observed_y Logical; if TRUE, retains observed outcome in final wave even if censored. Default: FALSE.
#' @return A data.frame with processed and optionally scaled and encoded columns, ready for longitudinal analysis.
#' @importFrom dplyr group_by mutate ungroup select if_else rename_at vars relocate
#' @importFrom fastDummies dummy_cols
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_warning cli_alert_success
#' @export
margot_process_longitudinal_data_wider <- function(
    df_wide,
    relationship_id = "NULL",
    ordinal_columns = NULL,
    continuous_columns_keep = NULL,
    exposure_vars = NULL,
    scale_exposure = FALSE,
    scale_continuous = TRUE,
    censor_if_missing_exposure = TRUE,
    not_lost_in_following_wave = "not_lost_following_wave",
    lost_in_following_wave = NULL,
    remove_selected_columns = TRUE,
    time_point_prefixes = NULL,
    time_point_regex = NULL,
    save_observed_y = FALSE
) {
  cli::cli_h1("longitudinal dyadic data processing")

  # discover time points
  if (is.null(time_point_prefixes)) {
    if (is.null(time_point_regex)) time_point_regex <- "^(t\\d+)_.*$"
    matched_cols <- grep(time_point_regex, colnames(df_wide), value = TRUE)
    time_points <- unique(gsub(time_point_regex, "\\1", matched_cols))
    time_points <- time_points[order(as.numeric(sub("^t", "", time_points)))]
  } else {
    time_points <- time_point_prefixes
  }
  num_time_points <- length(time_points)
  cli::cli_alert_info("detected {num_time_points} time points: {paste(time_points, collapse = ', ')}")

  df <- df_wide
  final_wave <- time_points[num_time_points]
  outcome_cols <- grep(paste0("^", final_wave, "_"), names(df), value = TRUE)
  outcome_vars <- sub(paste0("^", final_wave, "_"), "", outcome_cols)

  # step 1: not_lost indicators & censoring
  cli::cli_h2("step 1: creating 'not_lost' indicators & applying censoring")
  for (i in seq_len(num_time_points - 1)) {
    t0 <- time_points[i]
    t1 <- time_points[i + 1]
    nl_col <- paste0(t0, "_", not_lost_in_following_wave)
    expo_cols <- if (!is.null(exposure_vars)) paste0(t1, "_", exposure_vars) else character(0)
    expo_exist <- all(expo_cols %in% names(df))

    # choose method: exposure-based or outcome-based
    if (expo_exist && censor_if_missing_exposure) {
      # censor by missing exposure at t1
      not_lost <- rowSums(!is.na(df[expo_cols])) == length(expo_cols)
      df[[nl_col]] <- as.integer(not_lost)
    } else {
      # fallback: outcome-based
      out_cols_next <- grep(paste0("^", t1, "_"), names(df), value = TRUE)
      observed <- rowSums(!is.na(df[out_cols_next, drop = FALSE]))
      total <- length(out_cols_next)
      df[[nl_col]] <- NA_integer_
      # if any observed at t1, not lost
      df[[nl_col]][observed == total] <- 1L
      df[[nl_col]][observed < total] <- 0L
    }

    # dyadic enforcement
    if (!is.null(relationship_id) && relationship_id %in% names(df)) {
      df <- df %>%
        group_by(.data[[relationship_id]]) %>%
        mutate(any_lost = any(.data[[nl_col]] == 0, na.rm = TRUE),
               !!nl_col := if_else(any_lost, 0L, .data[[nl_col]])) %>%
        ungroup() %>%
        select(-any_lost)
    }

    # censor future waves
    to_censor <- df[[nl_col]] == 0 | is.na(df[[nl_col]])
    if (any(to_censor)) {
      future_waves <- time_points[(i+1):num_time_points]
      for (fw in future_waves) {
        fcols <- grep(paste0("^", fw, "_"), names(df), value = TRUE)
        if (save_observed_y && fw == final_wave) {
          fcols <- setdiff(fcols, grep(paste0("^", fw, "_", paste(outcome_vars, collapse="|")), fcols, value=TRUE))
        }
        df[to_censor, fcols] <- NA
      }
    }
    cli::cli_alert_success("wave {t0}: created {nl_col} and censored future waves")
  }

  # step 2: final-outcome handling
  cli::cli_h2("step 2: handling final wave outcomes")
  final_cols <- paste0(final_wave, "_", outcome_vars)
  final_cols <- intersect(final_cols, names(df))
  if (!save_observed_y && length(final_cols)) {
    df[rowSums(is.na(df[final_cols, drop = FALSE])) > 0, final_cols] <- NA
    cli::cli_alert_success("set partially missing final outcomes to NA")
  }

  # step 3: lost indicators
  if (!is.null(lost_in_following_wave)) {
    cli::cli_h2("step 3: creating lost indicators")
    for (i in seq_len(num_time_points - 1)) {
      t0 <- time_points[i]; nl <- paste0(t0, "_", not_lost_in_following_wave);
      lc <- paste0(t0, "_", lost_in_following_wave)
      if (nl %in% names(df)) {
        df[[lc]] <- ifelse(is.na(df[[nl]]), NA_integer_, 1L - df[[nl]])
        cli::cli_alert_success("created {lc}")
      }
    }
  }

  # step 4: scale continuous
  if (scale_continuous) {
    cli::cli_h2("step 4: scaling continuous variables")
    num_cols <- names(df)[sapply(df, is.numeric)]
    excl <- paste0("_", not_lost_in_following_wave, "$|_", lost_in_following_wave, "$|_binary$|_na$|_weights$")
    to_scale <- setdiff(setdiff(num_cols, continuous_columns_keep), ordinal_columns)
    to_scale <- to_scale[!grepl(excl, to_scale)]
    if (!scale_exposure && !is.null(exposure_vars)) {
      expo_names <- paste0(rep(time_points, each=length(exposure_vars)), "_", exposure_vars)
      to_scale <- setdiff(to_scale, expo_names)
    }
    for (col in to_scale) {
      df[[paste0(col, "_z")]] <- as.vector(scale(df[[col]]))
    }
    df <- df[, !(names(df) %in% to_scale)]
    cli::cli_alert_success("scaled {length(to_scale)} continuous variables")
  }

  # step 5: encode ordinal
  cli::cli_h2("step 5: encoding ordinal columns")
  if (!is.null(ordinal_columns)) {
    df <- fastDummies::dummy_cols(df, select_columns = ordinal_columns,
                                  remove_selected_columns = remove_selected_columns,
                                  ignore_na = TRUE)
    for (oc in ordinal_columns) {
      rename_at_vars <- grep(paste0("^", oc, "_"), names(df), value = TRUE)
      df <- rename_at(df, vars(all_of(rename_at_vars)), ~ paste0(., "_binary"))
    }
    cli::cli_alert_success("encoded ordinal columns")
  }

  # step 6: reorder
  cli::cli_h2("step 6: reordering columns")
  order_cols <- c()
  if ("id" %in% names(df)) order_cols <- c(order_cols, "id")
  if (!is.null(relationship_id) && relationship_id %in% names(df) && relationship_id != "id") order_cols <- c(order_cols, relationship_id)
  for (t in time_points) {
    tcols <- grep(paste0("^", t, "_"), names(df), value = TRUE)
    zcols <- grep("_z$", tcols, value = TRUE)
    expo <- if (!is.null(exposure_vars)) paste0(t, "_", exposure_vars) else NULL
    nl <- paste0(t, "_", not_lost_in_following_wave)
    lc <- if (!is.null(lost_in_following_wave) && t != final_wave) paste0(t, "_", lost_in_following_wave) else NULL
    others <- setdiff(tcols, c(zcols, expo, nl, lc))
    order_cols <- c(order_cols, others, zcols, expo, nl, lc)
  }
  df <- df[, intersect(order_cols, names(df))]
  cli::cli_alert_success("reordered columns 👍")

  cli::cli_h2("summary")
  cli::cli_alert_info("rows: {nrow(df)}, cols: {ncol(df)}, waves: {length(time_points)} completed 👍")
  df
}
#' #' Impute Missing Values Using Carry Forward in Longitudinal Data
#' #'
#' #' @description
#' #' Imputes missing values in longitudinal data by carrying forward previous observations
#' #' up to a specified number of time points back. By default, it never imputes data for the final wave
#' #' (end-of-study). Optionally, it can create indicator variables for imputed values.
#' #'
#' #' @param df_wide a wide-format dataframe containing longitudinal data.
#' #' @param columns_to_impute character vector of base column names to impute (without time prefixes).
#' #' @param max_carry_forward maximum number of time points to look back for carrying forward values.
#' #' @param time_point_prefixes optional vector of time point prefixes (e.g., c("t0", "t1", "t2")).
#' #' @param time_point_regex optional regex pattern to identify time points. Overrides time_point_prefixes if provided.
#' #' @param require_one_observed logical. if TRUE, only impute if at least one value is observed in the present or a following wave.
#' #' @param columns_no_future_required character vector of columns that do not require future observations for imputation.
#' #'   defaults to all columns if require_one_observed = FALSE, or none if require_one_observed = TRUE.
#' #' @param create_na_indicator logical. if TRUE, creates indicator variables for imputed values.
#' #' @param indicator_suffix suffix to add to the original column name for the indicator variable (default is "_na").
#' #' @param indicator_as_suffix logical. if TRUE, the indicator suffix is added as a suffix; if FALSE, it's added as a prefix.
#' #' @param verbose logical. if TRUE, prints progress information.
#' #' @param impute_final_wave logical. if FALSE (default), the final wave (end-of-study) is never imputed.
#' #'   if TRUE, the final wave can be imputed like other waves.
#' #'
#' #' @return a dataframe with imputed values and optional indicator variables.
#' #'
#' #' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
#' margot_impute_carry_forward <- function(
    #'     df_wide,
#'     columns_to_impute,
#'     max_carry_forward          = 1,
#'     time_point_prefixes        = NULL,
#'     time_point_regex           = NULL,
#'     require_one_observed       = TRUE,
#'     columns_no_future_required = NULL,
#'     create_na_indicator        = TRUE,
#'     indicator_suffix           = "_na",
#'     indicator_as_suffix        = TRUE,
#'     verbose                    = TRUE,
#'     impute_final_wave          = FALSE
#' ) {
#'   # validate columns_no_future_required
#'   if (!is.null(columns_no_future_required)) {
#'     bad <- setdiff(columns_no_future_required, columns_to_impute)
#'     if (length(bad) > 0) stop(
#'       "columns_no_future_required not in columns_to_impute: ",
#'       paste(bad, collapse = ", ")
#'     )
#'   } else {
#'     columns_no_future_required <-
#'       if (!require_one_observed) columns_to_impute else character(0)
#'   }
#'
#'   if (verbose) {
#'     cli::cli_h1("Longitudinal data imputation")
#'     cli::cli_alert_info("Starting imputation process")
#'   }
#'
#'   stats <- list()
#'
#'   # detect time prefixes
#'   if (is.null(time_point_prefixes)) {
#'     if (is.null(time_point_regex)) time_point_regex <- "^(t\\d+)_.*$"
#'     cols        <- grep(time_point_regex, names(df_wide), value = TRUE)
#'     time_points <- unique(sub(time_point_regex, "\\1", cols))
#'   } else {
#'     time_points <- time_point_prefixes
#'   }
#'   if (length(time_points) == 0) stop("No time-points found; check regex or time_point_prefixes")
#'
#'   # order numerically
#'   nums        <- as.numeric(sub("^t", "", time_points))
#'   time_points <- time_points[order(nums)]
#'   n_tp        <- length(time_points)
#'
#'   if (verbose) {
#'     cli::cli_alert_success(
#'       "Found {n_tp} time points: {paste(time_points, collapse = ', ')}"
#'     )
#'   }
#'
#'   # warn if no matching columns for any base
#'   pattern    <- paste0("^(", paste(time_points, collapse = "|"), ")_")
#'   match_cols <- grep(pattern, names(df_wide), value = TRUE)
#'   bases      <- unique(sub("^[^_]+_(.*)$", "\\1", match_cols))
#'   missing    <- setdiff(columns_to_impute, bases)
#'   if (length(missing) > 0) warning("No columns found for: ", paste(missing, collapse = ", "))
#'
#'   # prepare output and indicator storage
#'   out        <- df_wide
#'   indicators <- list()
#'
#'   # main imputation loop
#'   for (base in columns_to_impute) {
#'     if (verbose) cli::cli_h2("Variable: {base}")
#'     stats[[base]] <- list()
#'     req_fut       <- require_one_observed && !(base %in% columns_no_future_required)
#'     max_t         <- if (impute_final_wave) n_tp else (n_tp - 1)
#'     max_t         <- max(0, max_t)
#'
#'     for (i in seq_len(max_t)) {
#'       tp  <- time_points[i]
#'       col <- paste0(tp, "_", base)
#'       stats[[base]][[tp]] <- list(total = 0L, imp = 0L, rem = 0L, src = list())
#'       if (!col %in% names(out)) next
#'
#'       # determine rows eligible: require present or future observation
#'       if (req_fut && i < n_tp) {
#'         future_cols <- paste0(time_points[(i+1):n_tp], "_", base)
#'         future_cols <- intersect(future_cols, names(out))
#'         # include current wave in eligibility check
#'         cols_check  <- c(col, future_cols)
#'         if (length(cols_check) == 0) next
#'         ok <- rowSums(!is.na(out[, cols_check, drop = FALSE])) > 0
#'       } else {
#'         ok <- rep(TRUE, nrow(out))
#'       }
#'
#'       miss          <- is.na(out[[col]]) & ok
#'       total_missing <- sum(miss)
#'       stats[[base]][[tp]]$total <- total_missing
#'
#'       if (total_missing > 0) {
#'         # create indicator
#'         if (create_na_indicator) {
#'           flag <- if (indicator_as_suffix) paste0(col, indicator_suffix) else paste0(indicator_suffix, col)
#'           if (!flag %in% names(indicators)) indicators[[flag]] <- integer(nrow(out))
#'           indicators[[flag]][miss] <- 1L
#'         }
#'         if (verbose) cli::cli_alert_info("{tp}: {total_missing} missing values")
#'
#'         # carry-forward: impute from the immediate previous wave(s)
#'         for (lag in seq_len(max_carry_forward)) {
#'           prev_i   <- i - lag
#'           if (prev_i < 1) break
#'           prev_col <- paste0(time_points[prev_i], "_", base)
#'           if (prev_col %in% names(out)) {
#'             still <- miss & is.na(out[[col]])
#'             if (!any(still)) break
#'             out[still, col] <- out[still, prev_col]
#'             new_imp <- sum(!is.na(out[still, col]))
#'             if (new_imp > 0) {
#'               stats[[base]][[tp]]$imp <- stats[[base]][[tp]]$imp + new_imp
#'               stats[[base]][[tp]]$src[[time_points[prev_i]]] <-
#'                 (stats[[base]][[tp]]$src[[time_points[prev_i]]] %||% 0L) + new_imp
#'             }
#'           }
#'         }
#'         # remaining missing
#'         stats[[base]][[tp]]$rem <- sum(is.na(out[[col]]) & miss)
#'       }
#'     }
#'   }
#'
#'   # bind indicators
#'   if (length(indicators) > 0) {
#'     out <- cbind(out, as.data.frame(indicators))
#'   }
#'
#'   # summary report
#'   if (verbose) {
#'     cli::cli_h1("Imputation summary report")
#'     for (base in names(stats)) {
#'       for (tp in names(stats[[base]])) {
#'         s <- stats[[base]][[tp]]
#'         if (s$total > 0) {
#'           cli::cli_h2("{base} @ {tp}")
#'           cli::cli_li("missing: {s$total}, imputed: {s$imp}, remaining: {s$rem}")
#'           if (length(s$src) > 0) {
#'             cli::cli_ul({
#'               for (src_tp in names(s$src)) {
#'                 pct <- round(s$src[[src_tp]] / s$imp * 100, 1)
#'                 cli::cli_li("from {src_tp}: {s$src[[src_tp]]} ({pct}% of imputed)")
#'               }
#'             })
#'           }
#'         }
#'       }
#'     }
#'     cli::cli_alert_success("Imputation done 👍")
#'   }
#'
#'   return(out)
#' }
#' #' Impute Missing Values Using Carry Forward in Longitudinal Data
#' #'
#' #' @description
#' #' Imputes missing values in longitudinal data by carrying forward previous observations
#' #' up to a specified number of time points back. By default, it never imputes data for the final wave
#' #' (end-of-study). Optionally, it can create indicator variables for imputed values.
#' #'
#' #' @param df_wide a wide-format dataframe containing longitudinal data.
#' #' @param columns_to_impute character vector of base column names to impute (without time prefixes).
#' #' @param max_carry_forward maximum number of time points to look back for carrying forward values.
#' #' @param time_point_prefixes optional vector of time point prefixes (e.g., c("t0", "t1", "t2")).
#' #' @param time_point_regex optional regex pattern to identify time points. Overrides time_point_prefixes if provided.
#' #' @param require_one_observed logical. if TRUE, only impute if at least one value is observed in the present or a following wave.
#' #' @param columns_no_future_required character vector of columns that do not require future observations for imputation.
#' #'   defaults to all columns if require_one_observed = FALSE, or none if require_one_observed = TRUE.
#' #' @param create_na_indicator logical. if TRUE, creates indicator variables for imputed values.
#' #' @param indicator_suffix suffix to add to the original column name for the indicator variable (default is "_na").
#' #' @param indicator_as_suffix logical. if TRUE, the indicator suffix is added as a suffix; if FALSE, it's added as a prefix.
#' #' @param verbose logical. if TRUE, prints progress information.
#' #' @param impute_final_wave logical. if FALSE (default), the final wave (end-of-study) is never imputed.
#' #'   if TRUE, the final wave can be imputed like other waves.
#' #'
#' #' @return a dataframe with imputed values and optional indicator variables.
#' #'
#' #' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
#' #' @export
#' margot_impute_carry_forward <- function(
    #'   df_wide,
#'   columns_to_impute,
#'   max_carry_forward         = 1,
#'   time_point_prefixes       = NULL,
#'   time_point_regex          = NULL,
#'   require_one_observed      = TRUE,
#'   columns_no_future_required= NULL,
#'   create_na_indicator       = TRUE,
#'   indicator_suffix          = "_na",
#'   indicator_as_suffix       = TRUE,
#'   verbose                   = TRUE,
#'   impute_final_wave         = FALSE
#' ) {
#'   # validate columns_no_future_required
#'   if (!is.null(columns_no_future_required)) {
#'     bad <- setdiff(columns_no_future_required, columns_to_impute)
#'     if (length(bad) > 0) {
#'       stop("columns_no_future_required not in columns_to_impute: ",
#'            paste(bad, collapse = ", "))
#'     }
#'   } else {
#'     columns_no_future_required <- if (!require_one_observed) columns_to_impute else character(0)
#'   }
#'
#'   if (verbose) {
#'     cli::cli_h1("Longitudinal data imputation")
#'     cli::cli_alert_info("Starting imputation process")
#'   }
#'
#'   stats <- list()
#'
#'   # detect time prefixes
#'   if (is.null(time_point_prefixes)) {
#'     if (is.null(time_point_regex)) {
#'       time_point_regex <- "^(t\\d+)_.*$"
#'     }
#'     cols        <- grep(time_point_regex, names(df_wide), value = TRUE)
#'     time_points <- unique(sub(time_point_regex, "\\1", cols))
#'   } else {
#'     time_points <- time_point_prefixes
#'   }
#'   if (length(time_points) == 0) {
#'     stop("No time-points found; check regex or time_point_prefixes")
#'   }
#'
#'   # order numerically
#'   nums        <- as.numeric(sub("^t", "", time_points))
#'   time_points <- time_points[order(nums)]
#'   n_tp        <- length(time_points)
#'
#'   if (verbose) {
#'     cli::cli_alert_success(
#'       "Found {n_tp} time points: {paste(time_points, collapse = ', ')}"
#'     )
#'   }
#'
#'   # warn if no matching columns for any base
#'   pattern    <- paste0("^(", paste(time_points, collapse = "|"), ")_")
#'   match_cols <- grep(pattern, names(df_wide), value = TRUE)
#'   bases      <- unique(sub("^[^_]+_(.*)$", "\\1", match_cols))
#'   missing    <- setdiff(columns_to_impute, bases)
#'   if (length(missing) > 0) {
#'     warning("No columns found for: ", paste(missing, collapse = ", "))
#'   }
#'
#'   # prepare output and indicator storage
#'   out        <- df_wide
#'   indicators <- list()
#'
#'   # main imputation loop
#'   for (base in columns_to_impute) {
#'     if (verbose) cli::cli_h2("Variable: {base}")
#'     stats[[base]] <- list()
#'     req_fut       <- require_one_observed && !(base %in% columns_no_future_required)
#'     max_t         <- if (impute_final_wave) n_tp else (n_tp - 1)
#'     max_t         <- max(0, max_t)
#'
#'     for (i in seq_len(max_t)) {
#'       tp    <- time_points[i]
#'       col   <- paste0(tp, "_", base)
#'       stats[[base]][[tp]] <- list(total = 0L, imp = 0L, rem = 0L, src = list())
#'       if (!col %in% names(out)) next
#'
#'       # determine rows eligible
#'       if (req_fut && i < n_tp) {
#'         future_cols <- paste0(time_points[(i+1):n_tp], "_", base)
#'         future_cols <- intersect(future_cols, names(out))
#'         if (length(future_cols) == 0) next
#'         ok <- rowSums(!is.na(out[, future_cols, drop = FALSE])) > 0
#'       } else {
#'         ok <- rep(TRUE, nrow(out))
#'       }
#'
#'       miss          <- is.na(out[[col]]) & ok
#'       total_missing <- sum(miss)
#'       stats[[base]][[tp]]$total <- total_missing
#'
#'       if (total_missing > 0) {
#'         # create indicator
#'         if (create_na_indicator) {
#'           flag <- if (indicator_as_suffix) paste0(col, indicator_suffix) else paste0(indicator_suffix, col)
#'           if (!flag %in% names(indicators)) indicators[[flag]] <- integer(nrow(out))
#'           indicators[[flag]][miss] <- 1L
#'         }
#'         if (verbose) cli::cli_alert_info("{tp}: {total_missing} missing values")
#'
#'         # carry-forward: impute from the immediate previous wave
#'         # carry-forward
#'         for (lag in seq_len(max_carry_forward)) {
#'           prev_i  <- i - lag
#'           if (prev_i < 1) break
#'           prev_col <- paste0(time_points[prev_i], "_", base)
#'           if (prev_col %in% names(out)) {
#'             still <- miss & is.na(out[[col]])
#'             if (!any(still)) break
#'             out[still, col] <- out[still, prev_col]
#'             new_imp <- sum(!is.na(out[still, col]))
#'             if (new_imp > 0) {
#'               stats[[base]][[tp]]$imp <- stats[[base]][[tp]]$imp + new_imp
#'               stats[[base]][[tp]]$src[[time_points[prev_i]]] <-
#'                 (stats[[base]][[tp]]$src[[time_points[prev_i]]] %||% 0L) + new_imp
#'             }
#'           }
#'         }
#'         # remaining missing
#'         stats[[base]][[tp]]$rem <- sum(is.na(out[[col]]) & miss)
#'       }
#'     }
#'   }
#'
#'   # bind indicators
#'   if (length(indicators) > 0) {
#'     out <- cbind(out, as.data.frame(indicators))
#'   }
#'
#'   # summary report
#'   if (verbose) {
#'     cli::cli_h1("Imputation summary report")
#'     for (base in names(stats)) {
#'       for (tp in names(stats[[base]])) {
#'         s <- stats[[base]][[tp]]
#'         if (s$total > 0) {
#'           cli::cli_h2("{base} @ {tp}")
#'           cli::cli_li("missing: {s$total}, imputed: {s$imp}, remaining: {s$rem}")
#'           if (length(s$src) > 0) {
#'             cli::cli_ul({
#'               for (src_tp in names(s$src)) {
#'                 pct <- round(s$src[[src_tp]] / s$imp * 100, 1)
#'                 cli::cli_li("from {src_tp}: {s$src[[src_tp]]} ({pct}% of imputed)")
#'               }
#'             })
#'           }
#'         }
#'       }
#'     }
#'     cli::cli_alert_success("Imputation done 👍")
#'   }
#'
#'   return(out)
#' }
