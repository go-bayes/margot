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

