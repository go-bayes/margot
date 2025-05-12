#' Impute Missing Values Using Carry Forward in Longitudinal Data
#'
#' @description
#' Imputes missing values in longitudinal data by carrying forward previous observations
#' up to a specified number of time points back. By default, it never imputes data for the final wave
#' (end-of-study). Optionally, it can create indicator variables for imputed values.
#'
#' @param df_wide a wide-format dataframe containing longitudinal data.
#' @param columns_to_impute character vector of base column names to impute (without time prefixes).
#' @param max_carry_forward maximum number of time points to look back for carrying forward values.
#' @param time_point_prefixes optional vector of time point prefixes (e.g., c("t0", "t1", "t2")).
#' @param time_point_regex optional regex pattern to identify time points. Overrides time_point_prefixes if provided.
#' @param require_one_observed logical. if TRUE, only impute if at least one value is observed in the present or a following wave.
#' @param columns_no_future_required character vector of columns that do not require future observations for imputation.
#'   defaults to all columns if require_one_observed = FALSE, or none if require_one_observed = TRUE.
#' @param create_na_indicator logical. if TRUE, creates indicator variables for imputed values.
#' @param indicator_suffix suffix to add to the original column name for the indicator variable (default is "_na").
#' @param indicator_as_suffix logical. if TRUE, the indicator suffix is added as a suffix; if FALSE, it's added as a prefix.
#' @param verbose logical. if TRUE, prints progress information.
#' @param impute_final_wave logical. if FALSE (default), the final wave (end-of-study) is never imputed.
#'   if TRUE, the final wave can be imputed like other waves.
#'
#' @return a dataframe with imputed values and optional indicator variables.
#'
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
#' @export
margot_impute_carry_forward <- function(
    df_wide,
    columns_to_impute,
    max_carry_forward          = 1,
    time_point_prefixes        = NULL,
    time_point_regex           = NULL,
    require_one_observed       = TRUE,
    columns_no_future_required = NULL,
    create_na_indicator        = TRUE,
    indicator_suffix           = "_na",
    indicator_as_suffix        = TRUE,
    verbose                    = TRUE,
    impute_final_wave          = FALSE
) {
  # validate columns_no_future_required
  if (!is.null(columns_no_future_required)) {
    bad <- setdiff(columns_no_future_required, columns_to_impute)
    if (length(bad) > 0) stop(
      "columns_no_future_required not in columns_to_impute: ",
      paste(bad, collapse = ", ")
    )
  } else {
    columns_no_future_required <-
      if (!require_one_observed) columns_to_impute else character(0)
  }

  if (verbose) {
    cli::cli_h1("Longitudinal data imputation")
    cli::cli_alert_info("Starting imputation process")
  }

  stats <- list()

  # detect time prefixes
  if (is.null(time_point_prefixes)) {
    if (is.null(time_point_regex)) time_point_regex <- "^(t\\d+)_.*$"
    cols        <- grep(time_point_regex, names(df_wide), value = TRUE)
    time_points <- unique(sub(time_point_regex, "\\1", cols))
  } else {
    time_points <- time_point_prefixes
  }
  if (length(time_points) == 0) stop("No time-points found; check regex or time_point_prefixes")

  # order numerically
  nums        <- as.numeric(sub("^t", "", time_points))
  time_points <- time_points[order(nums)]
  n_tp        <- length(time_points)

  if (verbose) {
    cli::cli_alert_success(
      "Found {n_tp} time points: {paste(time_points, collapse = ', ')}"
    )
  }

  # warn if no matching columns for any base
  pattern    <- paste0("^(", paste(time_points, collapse = "|"), ")_")
  match_cols <- grep(pattern, names(df_wide), value = TRUE)
  bases      <- unique(sub("^[^_]+_(.*)$", "\\1", match_cols))
  missing    <- setdiff(columns_to_impute, bases)
  if (length(missing) > 0) warning("No columns found for: ", paste(missing, collapse = ", "))

  # prepare output and indicator storage
  out        <- df_wide
  indicators <- list()

  # main imputation loop
  for (base in columns_to_impute) {
    if (verbose) cli::cli_h2("Variable: {base}")
    stats[[base]] <- list()
    req_fut       <- require_one_observed && !(base %in% columns_no_future_required)
    max_t         <- if (impute_final_wave) n_tp else (n_tp - 1)
    max_t         <- max(0, max_t)

    for (i in seq_len(max_t)) {
      tp  <- time_points[i]
      col <- paste0(tp, "_", base)
      stats[[base]][[tp]] <- list(total = 0L, imp = 0L, rem = 0L, src = list())
      if (!col %in% names(out)) next

      # determine rows eligible: require present or future observation
      if (req_fut && i < n_tp) {
        future_cols <- paste0(time_points[(i+1):n_tp], "_", base)
        future_cols <- intersect(future_cols, names(out))
        # include current wave in eligibility check
        cols_check  <- c(col, future_cols)
        if (length(cols_check) == 0) next
        ok <- rowSums(!is.na(out[, cols_check, drop = FALSE])) > 0
      } else {
        ok <- rep(TRUE, nrow(out))
      }

      miss          <- is.na(out[[col]]) & ok
      total_missing <- sum(miss)
      stats[[base]][[tp]]$total <- total_missing

      if (total_missing > 0) {
        # create indicator
        if (create_na_indicator) {
          flag <- if (indicator_as_suffix) paste0(col, indicator_suffix) else paste0(indicator_suffix, col)
          if (!flag %in% names(indicators)) indicators[[flag]] <- integer(nrow(out))
          indicators[[flag]][miss] <- 1L
        }
        if (verbose) cli::cli_alert_info("{tp}: {total_missing} missing values")

        # carry-forward: impute from the immediate previous wave(s)
        for (lag in seq_len(max_carry_forward)) {
          prev_i   <- i - lag
          if (prev_i < 1) break
          prev_col <- paste0(time_points[prev_i], "_", base)
          if (prev_col %in% names(out)) {
            still <- miss & is.na(out[[col]])
            if (!any(still)) break
            out[still, col] <- out[still, prev_col]
            new_imp <- sum(!is.na(out[still, col]))
            if (new_imp > 0) {
              stats[[base]][[tp]]$imp <- stats[[base]][[tp]]$imp + new_imp
              stats[[base]][[tp]]$src[[time_points[prev_i]]] <-
                (stats[[base]][[tp]]$src[[time_points[prev_i]]] %||% 0L) + new_imp
            }
          }
        }
        # remaining missing
        stats[[base]][[tp]]$rem <- sum(is.na(out[[col]]) & miss)
      }
    }
  }

  # bind indicators
  if (length(indicators) > 0) {
    out <- cbind(out, as.data.frame(indicators))
  }

  # summary report
  if (verbose) {
    cli::cli_h1("Imputation summary report")
    for (base in names(stats)) {
      for (tp in names(stats[[base]])) {
        s <- stats[[base]][[tp]]
        if (s$total > 0) {
          cli::cli_h2("{base} @ {tp}")
          cli::cli_li("missing: {s$total}, imputed: {s$imp}, remaining: {s$rem}")
          if (length(s$src) > 0) {
            cli::cli_ul({
              for (src_tp in names(s$src)) {
                pct <- round(s$src[[src_tp]] / s$imp * 100, 1)
                cli::cli_li("from {src_tp}: {s$src[[src_tp]]} ({pct}% of imputed)")
              }
            })
          }
        }
      }
    }
    cli::cli_alert_success("Imputation done 👍")
  }

  return(out)
}
# old
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
