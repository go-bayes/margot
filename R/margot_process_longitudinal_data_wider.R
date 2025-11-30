#' Process longitudinal dyadic data in wide format with censoring by missing exposure and silent dummy-coding
#'
#' @description
#' this function processes longitudinal data (wide format) across multiple waves,
#' handling dyadic censoring and optional censoring when the exposure is missing.
#'
#' if `censor_if_missing_exposure = TRUE`, any record with a missing exposure at wave t+1
#' is considered lost at wave t and all subsequent wave data are set to `NA`. dyadic logic
#' ensures that if any member of a dyad is lost, the entire dyad is censored.
#'
#' @param df_wide a wide-format `data.frame` containing time-prefixed columns (e.g., `t0_x`).
#' @param relationship_id column name identifying dyads; if present, dyadic censoring is applied. default: `"NULL"`.
#' @param ordinal_columns character vector of ordinal column bases to dummy-encode after processing.
#' @param continuous_columns_keep character vector of continuous column names to retain without scaling.
#' @param exposure_vars character vector of exposure base names (without time prefixes).
#' @param scale_exposure logical; if `TRUE`, scales exposure variables. default: `FALSE`.
#' @param scale_continuous logical; if `TRUE`, scales continuous variables. default: `TRUE`.
#' @param censor_if_missing_exposure logical; if `TRUE`, missing exposures at next wave cause censoring. default: `TRUE`.
#' @param not_lost_in_following_wave suffix for the not-lost indicator. default: `"not_lost_following_wave"`.
#' @param lost_in_following_wave suffix for the lost indicator; if `NULL`, no lost indicator is added.
#' @param remove_selected_columns logical; if `TRUE`, removes original columns after dummy encoding. default: `TRUE`.
#' @param time_point_prefixes optional vector of time-point prefixes (e.g., `c("t0","t1")`). if `NULL`, inferred from data.
#' @param time_point_regex regex pattern to identify time-point prefixes. used if `time_point_prefixes` is `NULL`.
#' @param save_observed_y logical; if `TRUE`, retains observed outcomes in final wave even if censored. default: `FALSE`.
#' @return a `data.frame` with processed and optionally scaled and encoded columns, ready for longitudinal analysis.
#' @importFrom dplyr group_by mutate ungroup select rename_at vars
#' @importFrom fastDummies dummy_cols
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
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
    save_observed_y = FALSE) {
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

  # identify final wave outcomes
  final_wave <- time_points[num_time_points]
  final_outcome_cols <- grep(paste0("^", final_wave, "_"), names(df), value = TRUE)
  outcome_vars <- sub(paste0("^", final_wave, "_"), "", final_outcome_cols)

  # step 1: create not-lost indicators and apply censoring
  cli::cli_h2("step 1: creating 'not_lost' indicators & applying censoring")
  for (i in seq_len(num_time_points - 1)) {
    t0 <- time_points[i]
    t1 <- time_points[i + 1]
    nl_col <- paste0(t0, "_", not_lost_in_following_wave)
    expo_cols <- if (!is.null(exposure_vars)) paste0(t1, "_", exposure_vars) else character(0)
    expo_exist <- all(expo_cols %in% names(df))

    if (expo_exist && censor_if_missing_exposure) {
      # censor by exposure missingness
      not_lost <- rowSums(!is.na(df[, expo_cols, drop = FALSE])) == length(expo_cols)
      df[[nl_col]] <- as.integer(not_lost)
    } else {
      # fallback: outcome-based
      out_cols_next <- grep(paste0("^", t1, "_"), names(df), value = TRUE)
      observed <- rowSums(!is.na(df[, out_cols_next, drop = FALSE]))
      total <- length(out_cols_next)
      df[[nl_col]] <- NA_integer_
      df[[nl_col]][observed == total] <- 1L
      df[[nl_col]][observed < total] <- 0L
    }

    # dyadic enforcement: if any partner lost, censor all
    if (!is.null(relationship_id) && relationship_id %in% names(df)) {
      df <- df %>%
        dplyr::group_by(.data[[relationship_id]]) %>%
        dplyr::mutate(
          any_lost = any(.data[[nl_col]] == 0, na.rm = TRUE),
          !!nl_col := dplyr::if_else(any_lost, 0L, .data[[nl_col]])
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-any_lost)
    }

    # censor future waves
    to_censor <- df[[nl_col]] == 0 | is.na(df[[nl_col]])
    if (any(to_censor)) {
      future_waves <- time_points[(i + 1):num_time_points]
      for (fw in future_waves) {
        fcols <- grep(paste0("^", fw, "_"), names(df), value = TRUE)
        if (save_observed_y && fw == final_wave) {
          outcome_fw <- paste0(fw, "_", outcome_vars)
          fcols <- setdiff(fcols, outcome_fw)
        }
        df[to_censor, fcols] <- NA
      }
    }

    cli::cli_alert_success("wave {t0}: created {nl_col} and censored future waves")
  }

  # step 2: final wave outcome handling
  cli::cli_h2("step 2: handling final wave outcomes")
  final_cols <- final_outcome_cols
  if (!save_observed_y && length(final_cols)) {
    # set partially missing final outcomes to NA
    df[rowSums(is.na(df[, final_cols, drop = FALSE])) > 0, final_cols] <- NA
    cli::cli_alert_success("set partially missing final outcomes to NA")
  }

  # step 3: lost indicators
  if (!is.null(lost_in_following_wave)) {
    cli::cli_h2("step 3: creating lost indicators")
    for (i in seq_len(num_time_points - 1)) {
      t0 <- time_points[i]
      nl <- paste0(t0, "_", not_lost_in_following_wave)
      lc <- paste0(t0, "_", lost_in_following_wave)
      if (nl %in% names(df)) {
        df[[lc]] <- ifelse(is.na(df[[nl]]), NA_integer_, 1L - df[[nl]])
        cli::cli_alert_success("created {lc}")
      }
    }
  }

  # step 4: scale continuous variables
  if (scale_continuous) {
    cli::cli_h2("step 4: scaling continuous variables")
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    excl_pattern <- paste0(
      "_", not_lost_in_following_wave, "$|_",
      lost_in_following_wave, "$|_binary$|_na$|_weights$"
    )
    to_scale <- setdiff(setdiff(num_cols, continuous_columns_keep), ordinal_columns)
    to_scale <- to_scale[!grepl(excl_pattern, to_scale)]
    if (!scale_exposure && !is.null(exposure_vars)) {
      expo_names <- paste0(rep(time_points, each = length(exposure_vars)), "_", exposure_vars)
      to_scale <- setdiff(to_scale, expo_names)
    }
    for (col in to_scale) {
      df[[paste0(col, "_z")]] <- as.vector(scale(df[[col]]))
    }
    df <- df[, !(names(df) %in% to_scale)]
    cli::cli_alert_success("scaled {length(to_scale)} continuous variables")
  }

  # step 5: encode ordinal columns quietly
  cli::cli_h2("step 5: encoding ordinal columns")
  if (!is.null(ordinal_columns)) {
    # filter to only columns that actually exist in the data
    existing_ordinal_cols <- intersect(ordinal_columns, names(df))

    if (length(existing_ordinal_cols) > 0) {
      df <- suppressWarnings(
        fastDummies::dummy_cols(
          df,
          select_columns          = existing_ordinal_cols,
          remove_selected_columns = remove_selected_columns,
          ignore_na               = TRUE
        )
      )
      for (oc in existing_ordinal_cols) {
        oc_vars <- grep(paste0("^", oc, "_"), names(df), value = TRUE)
        df <- df %>%
          dplyr::rename_at(dplyr::vars(dplyr::all_of(oc_vars)), ~ paste0(., "_binary"))
      }
    }
    cli::cli_alert_success("encoded ordinal columns")
  }

  # step 6: reorder columns
  cli::cli_h2("step 6: reordering columns")
  order_cols <- character()
  if ("id" %in% names(df)) order_cols <- c(order_cols, "id")
  if (!is.null(relationship_id) && relationship_id %in% names(df) && relationship_id != "id") {
    order_cols <- c(order_cols, relationship_id)
  }
  for (t in time_points) {
    tcols <- grep(paste0("^", t, "_"), names(df), value = TRUE)
    zcols <- grep("_z$", tcols, value = TRUE)
    expo <- if (!is.null(exposure_vars)) paste0(t, "_", exposure_vars) else NULL
    nl <- paste0(t, "_", not_lost_in_following_wave)
    lc <- if (!is.null(lost_in_following_wave) && t != final_wave) {
      paste0(t, "_", lost_in_following_wave)
    } else {
      NULL
    }
    others <- setdiff(tcols, c(zcols, expo, nl, lc))
    order_cols <- c(order_cols, others, zcols, expo, nl, lc)
  }
  df <- df[, intersect(order_cols, names(df))]
  cli::cli_alert_success("reordered columns")

  # summary
  cli::cli_h2("summary")
  cli::cli_alert_success("rows: {nrow(df)}, cols: {ncol(df)}, waves: {length(time_points)} completed")

  df
}
