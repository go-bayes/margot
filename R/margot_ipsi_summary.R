#' Tidy per-shift LMTP positivity summary
#'
#' Builds a single, tidy table per shift with practical-positivity diagnostics and
#' optional effect columns (ATT, CI, E-value) when supplied. Designed to compare
#' incremental propensity score interventions (IPSI) such as `ipsi_02`, `ipsi_05`,
#' `ipsi_10` without refitting, but works for any set of shifts available in an
#' LMTP run object.
#'
#' Metrics include:
#' - Cumulative density-ratio support across selected waves, summarised as the
#'   fraction of uncensored rows falling below and above a user-defined central
#'   band, plus a graded support screen, percent collapsing to zero including
#'   censoring (IPCW zeros), and the final cumulative ESS.
#' - ESS on uncensored rows overall and relative to person-time (ESS+/(N_pt)).
#' - Policy-implied exposure rates by wave (and overall) on uncensored rows when
#'   `exposure_by_wave` is attached to models (best-effort from `margot_lmtp()`).
#' - Optional effect columns merged from a user-supplied `effect_table`.
#'
#' @param x Result of `margot_lmtp()` (list with `$models`) or a list of models
#'   each exposing `$density_ratios` (matrix with waves in columns). Optionally
#'   models may include `$exposure_by_wave` to enable policy-rate summaries.
#' @param outcome Optional character outcome name. When `NULL`, the first stored
#'   outcome is used.
#' @param shifts Character vector of shifts to include (full names or cleaned suffixes).
#'   If NULL, includes all available for the outcome. When supplied, the output
#'   preserves the order provided here.
#' @param waves Optional integer vector of waves to include; defaults to all.
#' @param test_thresholds Named list controlling tests. Recognised names:
#'   - `prod_log10` (default -1) defining the central support band
#'     `[10^{prod_log10}, 10^{-prod_log10}]`; `-1` corresponds to `[0.1, 10]`.
#'   - `prod_frac_ok` (default 0.05) and `prod_frac_warn` (default 0.20),
#'     defining the graded support screen from the combined fraction outside the band.
#'   - `near_zero_median` (default 1e-3) and `near_zero_cv` (default 0.05) are computed but
#'     not included in the support screen; they are returned as counts of flagged waves per shift.
#' @param include_policy_rates Logical; if TRUE and exposure-by-wave aligned with density ratios
#'   is available, returns policy-implied Pr(A_t=1) by wave (uncensored rows).
#' @param effect_table Optional data.frame with effect columns to merge by (outcome, shift).
#'   Expected columns (case-insensitive, flexible names): `outcome`, `shift` (either full or
#'   cleaned), and any of `att`, `ci_low`, `ci_high`, `e_value`, `e_value_bound`.
#' @param digits Integer; rounding for numeric outputs.
#'
#' @return A tibble/data.frame with one row per shift containing: `outcome`, `shift_full`,
#'   `shift_clean`, cumulative density-ratio metrics, support screen,
#'   ESS metrics (including final cumulative ESS), optional policy rates
#'   (per-wave `p_hat_wave_k` and `p_hat_overall`), optional effect columns
#'   if provided.
#'
#' @examples
#' \dontrun{
#' # Given an LMTP run with IPSI shifts
#' tbl <- margot_positivity_summary(
#'   x = fit_ipsi,
#'   outcome = "t5_meaning_purpose_z",
#'   shifts = c("null", "ipsi_02", "ipsi_05", "ipsi_10"),
#'   test_thresholds = list(prod_log10 = -1, prod_frac_warn = 0.10),
#'   include_policy_rates = TRUE,
#'   digits = 3
#' )
#'
#' # Compact table for reporting (kable-friendly headers)
#' tbl_compact <- margot_positivity_summary(
#'   x = fit_ipsi,
#'   outcome = "t5_meaning_purpose_z",
#'   shifts = c("null", "ipsi_02", "ipsi_05", "ipsi_10"),
#'   test_thresholds = list(prod_log10 = -1, prod_frac_warn = 0.10),
#'   include_policy_rates = FALSE,
#'   digits = 3,
#'   compact = TRUE,
#'   include_explanation = TRUE
#' )
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   knitr::kable(tbl_compact, format = "markdown")
#' }
#' # Print non-specialist explanation
#' cat(attr(tbl_compact, "explanation"), "\n")
#' }
#'
#' @export
margot_positivity_summary <- function(x,
                                      outcome = NULL,
                                      shifts = NULL,
                                      waves = NULL,
                                      test_thresholds = list(prod_log10 = -1,
                                                             prod_frac_ok = 0.05,
                                                             prod_frac_warn = 0.20,
                                                             near_zero_median = 1e-3,
                                                             near_zero_cv = 0.05),
                                      include_policy_rates = FALSE,
                                      effect_table = NULL,
                                      digits = 3,
                                      compact = TRUE,
                                      include_explanation = TRUE) {
  if (!is.null(outcome)) stopifnot(is.character(outcome), length(outcome) == 1L)
  if (!is.null(shifts)) stopifnot(is.character(shifts))
  if (!is.null(waves)) stopifnot(is.numeric(waves))
  digits <- max(0L, as.integer(digits))

  selection <- margot_resolve_positivity_selection(
    x = x,
    outcome = outcome,
    shifts = shifts,
    caller = "margot_positivity_summary"
  )
  outcome <- selection$outcome
  out_models <- selection$outcome_models
  shift_df <- selection$shift_df
  if (!nrow(shift_df)) return(data.frame())

  thr <- margot_positivity_thresholds(test_thresholds)
  band_strings <- margot_positivity_band_strings(thr)

  ess_fun <- function(w) {
    w <- w[is.finite(w)]
    if (!length(w)) return(NA_real_)
    s1 <- sum(w)
    s2 <- sum(w^2)
    if (s2 <= 0) return(NA_real_)
    (s1^2) / s2
  }

  all_full <- names(out_models)
  all_clean <- vapply(
    all_full,
    margot_clean_positivity_shift,
    character(1),
    outcome = outcome
  )

  null_idx <- which(all_clean == "null")[1]
  null_rates <- NULL
  null_overall <- NA_real_
  if (!is.na(null_idx) && include_policy_rates) {
    mod0 <- out_models[[all_full[[null_idx]]]]
    dr0 <- mod0$density_ratios
    ex0 <- mod0$exposure_by_wave
    if (!is.null(dr0) && !is.null(ex0)) {
      if (inherits(dr0, "Matrix")) dr0 <- as.matrix(dr0)
      if (!is.matrix(dr0)) dr0 <- as.matrix(dr0)
      if (inherits(ex0, "Matrix")) ex0 <- as.matrix(ex0)
      if (!is.matrix(ex0)) ex0 <- as.matrix(ex0)
      cols <- if (is.null(waves)) seq_len(ncol(dr0)) else intersect(seq_len(ncol(dr0)), as.integer(waves))
      rates0 <- rep(NA_real_, length(cols))
      w_all_acc0 <- c()
      e_all_acc0 <- c()
      for (i in seq_along(cols)) {
        j <- cols[i]
        w <- as.numeric(dr0[, j])
        e <- as.numeric(ex0[, j])
        mask <- is.finite(w) & is.finite(e) & (w > 0)
        rates0[i] <- if (any(mask)) sum(w[mask] * e[mask]) / sum(w[mask]) else NA_real_
        w_all_acc0 <- c(w_all_acc0, w)
        e_all_acc0 <- c(e_all_acc0, e)
      }
      null_rates <- rates0
      names(null_rates) <- paste0("wave_", cols)
      mask0 <- is.finite(w_all_acc0) & is.finite(e_all_acc0) & (w_all_acc0 > 0)
      null_overall <- if (any(mask0)) sum(w_all_acc0[mask0] * e_all_acc0[mask0]) / sum(w_all_acc0[mask0]) else NA_real_
    }
  }

  rows <- vector("list", nrow(shift_df))
  for (k in seq_len(nrow(shift_df))) {
    sh_full <- shift_df$shift_full[[k]]
    sh_clean <- shift_df$shift_clean[[k]]
    mod <- out_models[[sh_full]]
    dr <- mod$density_ratios
    if (is.null(dr)) next
    if (inherits(dr, "Matrix")) dr <- as.matrix(dr)
    if (!is.matrix(dr)) dr <- as.matrix(dr)
    total_cols <- ncol(dr)
    cols <- if (is.null(waves)) seq_len(total_cols) else intersect(seq_len(total_cols), as.integer(waves))

    nz_count <- 0L
    for (j in cols) {
      w <- as.numeric(dr[, j])
      w_pos <- w[is.finite(w) & (w > 0)]
      if (!length(w_pos)) next
      med <- stats::median(w_pos)
      mu <- mean(w_pos)
      cv <- if (is.finite(mu) && mu != 0) stats::sd(w_pos) / mu else NA_real_
      if (is.finite(med) &&
          med < thr$near_zero_median &&
          is.finite(cv) &&
          cv < thr$near_zero_cv) {
        nz_count <- nz_count + 1L
      }
    }

    prod_metrics <- margot_positivity_product_metrics(dr = dr, cols = cols, thresholds = thr)
    support_status <- margot_positivity_support_status(prod_metrics$prod_frac_outside, thr)

    all_w <- as.numeric(dr[, cols, drop = FALSE])
    all_w <- all_w[is.finite(all_w)]
    n_all <- length(all_w)
    all_w_pos <- all_w[all_w > 0]
    n_pos <- length(all_w_pos)
    ess_pos <- if (n_pos > 0) ess_fun(all_w_pos) else NA_real_
    ess_pos_frac <- if (n_pos > 0) ess_pos / n_pos else NA_real_
    ess_pos_frac_pt <- if (n_all > 0 && is.finite(ess_pos)) ess_pos / n_all else NA_real_

    sub_dr <- dr[, cols, drop = FALSE]
    sub_dr_masked <- ifelse(is.finite(sub_dr) & (sub_dr > 0), sub_dr, NA_real_)
    if (!is.matrix(sub_dr_masked)) {
      sub_dr_masked <- matrix(sub_dr_masked, ncol = length(cols))
    }
    cum_raw <- sub_dr_masked
    if (ncol(cum_raw) >= 2) {
      for (j in 2:ncol(cum_raw)) {
        cum_raw[, j] <- ifelse(
          is.finite(cum_raw[, j - 1]) & is.finite(sub_dr_masked[, j]),
          cum_raw[, j - 1] * sub_dr_masked[, j],
          NA_real_
        )
      }
    }
    ess_cum_raw <- if (ncol(cum_raw)) ess_fun(cum_raw[, ncol(cum_raw)]) else NA_real_

    p_rates <- list()
    p_overall <- NA_real_
    dp_rates <- list()
    dp_overall <- NA_real_
    if (isTRUE(include_policy_rates) && !is.null(mod$exposure_by_wave)) {
      ex <- mod$exposure_by_wave
      if (inherits(ex, "Matrix")) ex <- as.matrix(ex)
      if (!is.matrix(ex)) ex <- try(as.matrix(ex), silent = TRUE)
      if (!inherits(ex, "try-error") && is.matrix(ex) && nrow(ex) == nrow(dr)) {
        p_vec <- rep(NA_real_, length(cols))
        w_all_acc <- c()
        e_all_acc <- c()
        for (i in seq_along(cols)) {
          j <- cols[i]
          wj <- as.numeric(dr[, j])
          ej <- as.numeric(ex[, j])
          mask <- is.finite(wj) & is.finite(ej) & (wj > 0)
          p_vec[i] <- if (any(mask)) sum(wj[mask] * ej[mask]) / sum(wj[mask]) else NA_real_
          w_all_acc <- c(w_all_acc, wj)
          e_all_acc <- c(e_all_acc, ej)
        }
        mask_all <- is.finite(w_all_acc) & is.finite(e_all_acc) & (w_all_acc > 0)
        p_overall <- if (any(mask_all)) sum(w_all_acc[mask_all] * e_all_acc[mask_all]) / sum(w_all_acc[mask_all]) else NA_real_
        names(p_vec) <- paste0("p_hat_wave_", cols)
        p_rates <- as.list(p_vec)
        if (!is.null(null_rates) && length(null_rates)) {
          dn <- numeric(length(cols))
          names(dn) <- paste0("wave_", cols)
          for (i in seq_along(cols)) {
            key <- paste0("wave_", cols[i])
            dn[i] <- if (!is.null(null_rates[[key]]) && is.finite(p_vec[i])) p_vec[i] - null_rates[[key]] else NA_real_
          }
          names(dn) <- paste0("dp_hat_wave_", cols)
          dp_rates <- as.list(dn)
          dp_overall <- if (is.finite(p_overall) && is.finite(null_overall)) p_overall - null_overall else NA_real_
        }
      }
    }

    rows[[k]] <- c(list(
      outcome = outcome,
      shift_full = sh_full,
      shift_clean = sh_clean,
      prod_log10_threshold = thr$prod_log10,
      prod_frac_below = prod_metrics$prod_frac_below,
      prod_frac_above = prod_metrics$prod_frac_above,
      prod_frac_outside = prod_metrics$prod_frac_outside,
      prod_frac_below_pct = if (is.finite(prod_metrics$prod_frac_below)) 100 * prod_metrics$prod_frac_below else NA_real_,
      prod_frac_above_pct = if (is.finite(prod_metrics$prod_frac_above)) 100 * prod_metrics$prod_frac_above else NA_real_,
      prod_frac_outside_pct = if (is.finite(prod_metrics$prod_frac_outside)) 100 * prod_metrics$prod_frac_outside else NA_real_,
      prop_zero_prod_pct = if (is.finite(prod_metrics$prop_zero_prod)) 100 * prod_metrics$prop_zero_prod else NA_real_,
      support_status = support_status,
      verdict = support_status,
      near_zero_wave_flags = nz_count,
      ess_pos = ess_pos,
      ess_pos_frac = ess_pos_frac,
      ess_pos_frac_pt = ess_pos_frac_pt,
      ess_cum_raw = ess_cum_raw,
      p_hat_overall = p_overall,
      dp_hat_overall = dp_overall
    ), p_rates, dp_rates)
  }

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, lapply(rows, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE)))
  if (is.null(out) || !nrow(out)) return(out)

  if (!is.null(effect_table) && is.data.frame(effect_table) && nrow(effect_table)) {
    ef <- effect_table
    cn <- tolower(names(ef))
    names(ef) <- cn
    if (!("outcome" %in% names(ef))) ef$outcome <- outcome
    if ("shift" %in% names(ef)) {
      ef$shift_clean <- vapply(
        ef$shift,
        margot_clean_positivity_shift,
        character(1),
        outcome = outcome
      )
      keep_cols <- intersect(
        names(ef),
        c("outcome", "shift_clean", "att", "2.5 %", "97.5 %", "ci_low", "ci_high", "e-value", "e_value", "e_value_bound")
      )
      if (length(keep_cols)) {
        ef2 <- ef[, keep_cols, drop = FALSE]
        if ("2.5 %" %in% names(ef2)) names(ef2)[names(ef2) == "2.5 %"] <- "ci_low"
        if ("97.5 %" %in% names(ef2)) names(ef2)[names(ef2) == "97.5 %"] <- "ci_high"
        if ("e-value" %in% names(ef2)) names(ef2)[names(ef2) == "e-value"] <- "e_value"
        out <- merge(out, ef2, by = c("outcome", "shift_clean"), all.x = TRUE, sort = FALSE)
      }
    }
  }

  if (nrow(out)) {
    key <- paste(out$outcome, out$shift_clean, sep = "::")
    if (any(duplicated(key))) {
      if (isTRUE(requireNamespace("dplyr", quietly = TRUE))) {
        out <- dplyr::distinct(out, outcome, shift_clean, .keep_all = TRUE)
      } else {
        keep <- !duplicated(key)
        out <- out[keep, , drop = FALSE]
      }
    }
    out$order_id <- match(out$shift_clean, shift_df$shift_clean)
    out <- out[order(out$order_id), , drop = FALSE]
    out$order_id <- NULL
  }

  num_cols <- vapply(out, is.numeric, logical(1))
  out[num_cols] <- lapply(out[num_cols], function(v) {
    ifelse(is.finite(v), round(as.numeric(v), digits = digits), v)
  })

  if (isTRUE(compact)) {
    fmt <- function(x) ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), as.numeric(x)), NA_character_)
    att_ci_str <- NULL
    if (all(c("att", "ci_low", "ci_high") %in% names(out))) {
      att_ci_str <- ifelse(
        is.finite(out$att) & is.finite(out$ci_low) & is.finite(out$ci_high),
        paste0(fmt(out$att), " [", fmt(out$ci_low), ", ", fmt(out$ci_high), "]"),
        ifelse(is.finite(out$att), fmt(out$att), NA_character_)
      )
    }

    Shift <- vapply(
      out$shift_clean,
      margot_pretty_positivity_shift,
      character(1),
      label_mapping = NULL,
      outcome = NULL
    )
    zero_attr <- setNames(out$prop_zero_prod_pct, out$shift_clean)

    compact_df <- data.frame(
      Shift = Shift,
      Support = out$support_status,
      stringsAsFactors = FALSE
    )
    compact_df[["Zero %"]] <- out$prop_zero_prod_pct
    compact_df[[paste0("Outside ", band_strings$interval_label)]] <- out$prod_frac_outside_pct
    compact_df[[paste0("Prod < ", band_strings$lower_label)]] <- out$prod_frac_below_pct
    compact_df[[paste0("Prod > ", band_strings$upper_label)]] <- out$prod_frac_above_pct
    compact_df[["Cum ESS"]] <- out$ess_cum_raw
    compact_df$p_hat <- out$p_hat_overall
    compact_df$Delta_p <- out$dp_hat_overall
    if (!is.null(att_ci_str)) compact_df$`ATT [CI]` <- att_ci_str

    if (!isTRUE(include_policy_rates) || (all(is.na(compact_df$p_hat)) && all(is.na(compact_df$Delta_p)))) {
      compact_df$p_hat <- NULL
      compact_df$Delta_p <- NULL
    }

    if (isTRUE(include_explanation)) {
      waves_txt <- if (is.null(waves)) "all available waves" else paste0("waves ", paste(waves, collapse = ", "))
      expl <- paste(
        "Notes:",
        "- 'Zero %' is the % of rows whose cumulative density ratio across the selected waves equals 0, usually because follow-up was censored.",
        paste0(
          "- 'Outside ", band_strings$interval_label, "' is the combined % of uncensored rows whose cumulative density ratio falls below ",
          band_strings$lower_label, " or above ", band_strings$upper_label, "."
        ),
        paste0(
          "- 'Prod < ", band_strings$lower_label, "' and 'Prod > ", band_strings$upper_label,
          "' are the % of uncensored rows whose cumulative density ratio across ",
          waves_txt, " falls outside the central band ", band_strings$interval_label, "."
        ),
        paste0(
          "- 'Support' summarises the combined share outside that band: Adequate <= ",
          round(100 * thr$prod_frac_ok, 1), "%; Caution <= ",
          round(100 * thr$prod_frac_warn, 1),
          "%; Limited > ", round(100 * thr$prod_frac_warn, 1),
          "%. This is a reporting screen, not a formal identification test."
        ),
        "- 'Cum ESS' is the final cumulative effective sample size across the selected waves.",
        if (isTRUE(include_policy_rates)) "- 'p_hat' = policy-implied Pr(A_t=1) on uncensored rows; 'Delta_p' = difference vs null." else NULL,
        "- 'ATT [CI]' = average treatment effect with 95% CI.",
        "- Precision is reported separately as ESS/N_pt on uncensored rows; it describes weight variability, not support.",
        sep = "\n"
      )
      attr(compact_df, "explanation") <- expl
    }

    attr(compact_df, "prop_zero_pct") <- zero_attr
    precision_tbl <- data.frame(
      Shift = Shift,
      stringsAsFactors = FALSE
    )
    precision_tbl[["ESS/N_pt (precision)"]] <- out$ess_pos_frac_pt
    attr(compact_df, "precision_table") <- precision_tbl
    attr(compact_df, "precision_note") <- paste(
      "ESS/N_pt (precision) summarises weight variability on uncensored rows.",
      "Small values indicate imprecision rather than support failure."
    )

    out <- compact_df
  }

  if (isTRUE(requireNamespace("tibble", quietly = TRUE))) tibble::as_tibble(out) else out
}

#' @rdname margot_positivity_summary
#' @export
margot_ipsi_summary <- function(...) {
  .Deprecated("margot_positivity_summary", package = "margot")
  margot_positivity_summary(...)
}
