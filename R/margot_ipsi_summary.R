#' Tidy per-shift LMTP positivity summary
#'
#' Builds a single, tidy table per shift with practical-positivity diagnostics and
#' optional effect columns (ATT, CI, E-value) when supplied. Designed to compare
#' incremental propensity score interventions (IPSI) such as `ipsi_02`, `ipsi_05`,
#' `ipsi_10` without refitting, but works for any set of shifts available in an
#' LMTP run object.
#'
#' Metrics include:
#' - Product-of-r collapse across selected waves (uncensored fraction below a user
#'   threshold on log10 scale, with Pass/Fail verdict), and percent collapsing to
#'   zero including censoring (IPCW zeros).
#' - ESS on uncensored rows overall and relative to person-time (ESS+/(N_pt)).
#' - Policy-implied exposure rates by wave (and overall) on uncensored rows when
#'   `exposure_by_wave` is attached to models (best-effort from `margot_lmtp()`).
#' - Optional effect columns merged from a user-supplied `effect_table`.
#'
#' @param x Result of `margot_lmtp()` (list with `$models`) or a list of models
#'   each exposing `$density_ratios` (matrix with waves in columns). Optionally
#'   models may include `$exposure_by_wave` to enable policy-rate summaries.
#' @param outcome Character outcome name (must exist under `x$models`).
#' @param shifts Character vector of shifts to include (full names or cleaned suffixes).
#'   If NULL, includes all available for the outcome. When supplied, the output
#'   preserves the order provided here.
#' @param waves Optional integer vector of waves to include; defaults to all.
#' @param test_thresholds Named list controlling tests. Recognised names:
#'   - `prod_log10` (default -1) threshold on log10(product of ratios) for uncensored rows.
#'   - `prod_frac_warn` (default 0.10) warning fraction; verdict is Pass when fraction ≤ this.
#'   - `near_zero_median` (default 1e-3) and `near_zero_cv` (default 0.05) are computed but
#'     not included in the verdict; they are returned as counts of flagged waves per shift.
#' @param include_policy_rates Logical; if TRUE and exposure-by-wave aligned with density ratios
#'   is available, returns policy-implied Pr(A_t=1) by wave (uncensored rows).
#' @param effect_table Optional data.frame with effect columns to merge by (outcome, shift).
#'   Expected columns (case-insensitive, flexible names): `outcome`, `shift` (either full or
#'   cleaned), and any of `att`, `ci_low`, `ci_high`, `e_value`, `e_value_bound`.
#' @param digits Integer; rounding for numeric outputs.
#'
#' @return A tibble/data.frame with one row per shift containing: `outcome`, `shift_full`,
#'   `shift_clean`, product-of-r metrics, verdict,
#'   ESS metrics, optional policy rates (per-wave `p_hat_wave_k` and `p_hat_overall`), optional
#'   effect columns if provided.
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
                                      outcome,
                                      shifts = NULL,
                                      waves = NULL,
                                      test_thresholds = list(prod_log10 = -1,
                                                             prod_frac_warn = 0.10,
                                                             near_zero_median = 1e-3,
                                                             near_zero_cv = 0.05),
                                      include_policy_rates = TRUE,
                                      effect_table = NULL,
                                      digits = 3,
                                      compact = TRUE,
                                      include_explanation = TRUE) {
  stopifnot(is.character(outcome), length(outcome) == 1L)
  if (!is.null(shifts)) stopifnot(is.character(shifts))
  if (!is.null(waves)) stopifnot(is.numeric(waves))
  digits <- max(0L, as.integer(digits))

  # normalize models structure
  models <- NULL
  if (is.list(x) && !is.null(x$models) && is.list(x$models)) {
    models <- x$models
  } else if (is.list(x) && !is.null(x$density_ratios)) {
    models <- list(`(outcome)` = list(`(shift)` = x))
  } else if (is.list(x) && length(x) &&
             all(vapply(x, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
    models <- list(`(outcome)` = x)
  } else {
    stop("Unsupported input to `margot_ipsi_summary()`. Pass a margot_lmtp() result or a list with $density_ratios.")
  }
  if (!(outcome %in% names(models))) stop("Outcome not found in models: ", outcome)
  out_models <- models[[outcome]]
  if (!length(out_models)) return(data.frame())

  # select shifts (accept full or cleaned)
  clean_shift <- function(nm) {
    pref <- paste0(outcome, "_")
    if (startsWith(nm, pref)) substring(nm, nchar(pref) + 1L) else nm
  }
  all_full  <- names(out_models)
  all_clean <- vapply(all_full, clean_shift, character(1))
  if (is.null(shifts)) {
    sel_idx <- seq_along(all_full)
  } else {
    sel_idx <- integer(0)
    missing <- character(0)
    for (sh in shifts) {
      hit <- which(all_full == sh | all_clean == sh)
      if (!length(hit)) {
        missing <- c(missing, sh)
      } else {
        sel_idx <- c(sel_idx, hit[1])
      }
    }
    if (length(missing)) {
      stop(
        "Requested shifts not found for outcome=", outcome, ": ",
        paste(unique(missing), collapse = ", ")
      )
    }
  }
  if (!length(sel_idx)) stop("Requested shifts not found for outcome=", outcome)

  # thresholds
  thr <- list(prod_log10 = -1, prod_frac_warn = 0.10, near_zero_median = 1e-3, near_zero_cv = 0.05)
  if (is.list(test_thresholds) && length(test_thresholds)) {
    for (nm in intersect(names(test_thresholds), names(thr))) thr[[nm]] <- test_thresholds[[nm]]
  }

  # helper: ESS on positive weights
  ess_fun <- function(w) {
    w <- w[is.finite(w)]
    if (!length(w)) return(NA_real_)
    s1 <- sum(w); s2 <- sum(w^2)
    if (s2 <= 0) return(NA_real_)
    (s1^2) / s2
  }

  # reference null for policy-rate deltas if available
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
      w_all_acc0 <- c(); e_all_acc0 <- c()
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

  rows <- vector("list", length(sel_idx))
  for (k in seq_along(sel_idx)) {
    idx <- sel_idx[k]
    sh_full  <- all_full[[idx]]
    sh_clean <- all_clean[[idx]]
    mod <- out_models[[sh_full]]
    dr  <- mod$density_ratios
    if (is.null(dr)) next
    if (inherits(dr, "Matrix")) dr <- as.matrix(dr)
    if (!is.matrix(dr)) dr <- as.matrix(dr)
    total_cols <- ncol(dr)
    cols <- if (is.null(waves)) seq_len(total_cols) else intersect(seq_len(total_cols), as.integer(waves))

    # near-zero flags per wave
    nz_count <- 0L
    for (j in cols) {
      w <- as.numeric(dr[, j])
      w_pos <- w[is.finite(w) & (w > 0)]
      if (!length(w_pos)) next
      med <- stats::median(w_pos)
      mu  <- mean(w_pos)
      cv  <- if (is.finite(mu) && mu != 0) stats::sd(w_pos)/mu else NA_real_
      if (is.finite(med) && med < thr$near_zero_median && is.finite(cv) && cv < thr$near_zero_cv) nz_count <- nz_count + 1L
    }

    # product-of-r across waves
    if (!length(cols)) {
      prop_zero_prod <- NA_real_; frac_below <- NA_real_
    } else {
      sub <- dr[, cols, drop = FALSE]
      prod_all <- apply(sub, 1L, function(row) {
        row <- row[is.finite(row)]
        if (!length(row)) return(NA_real_)
        if (any(row == 0)) return(0)
        exp(sum(log(row)))
      })
      prod_pos <- apply(sub, 1L, function(row) {
        row <- row[is.finite(row) & row > 0]
        if (!length(row)) return(NA_real_)
        exp(sum(log(row)))
      })
      prop_zero_prod <- mean(prod_all == 0, na.rm = TRUE)
      log10_pos <- suppressWarnings(log10(prod_pos))
      frac_below <- mean(is.finite(log10_pos) & (log10_pos < thr$prod_log10), na.rm = TRUE)
    }
    verdict <- if (is.finite(frac_below) && (frac_below <= thr$prod_frac_warn)) "Pass" else "Fail"

    # overall ESS on uncensored rows; also ESS relative to person-time
    all_w <- as.numeric(dr[, cols, drop = FALSE])
    all_w <- all_w[is.finite(all_w)]
    n_all <- length(all_w)
    all_w_pos <- all_w[all_w > 0]
    n_pos <- length(all_w_pos)
    ess_pos <- if (n_pos > 0) ess_fun(all_w_pos) else NA_real_
    ess_pos_frac <- if (n_pos > 0) ess_pos / n_pos else NA_real_
    ess_pos_frac_pt <- if (n_all > 0 && is.finite(ess_pos)) ess_pos / n_all else NA_real_

    # policy rates (uncensored) by wave and overall
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
        w_all <- NULL; e_all <- NULL; w_all_acc <- c(); e_all_acc <- c()
        for (i in seq_along(cols)) {
          j <- cols[i]
          wj <- as.numeric(dr[, j]); ej <- as.numeric(ex[, j])
          mask <- is.finite(wj) & is.finite(ej) & (wj > 0)
          p_vec[i] <- if (any(mask)) sum(wj[mask] * ej[mask]) / sum(wj[mask]) else NA_real_
          w_all_acc <- c(w_all_acc, wj)
          e_all_acc <- c(e_all_acc, ej)
        }
        mask_all <- is.finite(w_all_acc) & is.finite(e_all_acc) & (w_all_acc > 0)
        p_overall <- if (any(mask_all)) sum(w_all_acc[mask_all] * e_all_acc[mask_all]) / sum(w_all_acc[mask_all]) else NA_real_
        names(p_vec) <- paste0("p_hat_wave_", cols)
        p_rates <- as.list(p_vec)
        # delta vs null when available
        if (!is.null(null_rates) && length(null_rates)) {
          # align by names like wave_1, wave_2
          dn <- numeric(length(cols)); names(dn) <- paste0("wave_", cols)
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
      prod_frac_below = frac_below,
      prod_frac_below_pct = if (is.finite(frac_below)) 100*frac_below else NA_real_,
      prop_zero_prod_pct = if (is.finite(prop_zero_prod)) 100*prop_zero_prod else NA_real_,
      verdict = verdict,
      near_zero_wave_flags = nz_count,
      ess_pos = ess_pos,
      ess_pos_frac = ess_pos_frac,
      ess_pos_frac_pt = ess_pos_frac_pt,
      p_hat_overall = p_overall,
      dp_hat_overall = dp_overall
    ), p_rates, dp_rates)
  }

  out <- do.call(rbind, lapply(rows, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE)))
  if (is.null(out) || !nrow(out)) return(out)

  # Optional: merge effects
  if (!is.null(effect_table) && is.data.frame(effect_table) && nrow(effect_table)) {
    ef <- effect_table
    # standardise column names to lower
    cn <- tolower(names(ef))
    names(ef) <- cn
    # try to identify outcome and shift
    if (!("outcome" %in% names(ef))) ef$outcome <- outcome
    # derive cleaned shift if not present
    if (!("shift" %in% names(ef))) {
      # nothing to merge on
      ef <- NULL
    } else {
      ef$shift_clean <- vapply(ef$shift, function(s) {
        pref <- paste0(outcome, "_")
        if (startsWith(s, pref)) substring(s, nchar(pref) + 1L) else s
      }, character(1))
      keep_cols <- intersect(names(ef), c("outcome","shift_clean","att","2.5 %","97.5 %","ci_low","ci_high","e-value","e_value","e_value_bound"))
      if (length(keep_cols)) {
        ef2 <- ef[, keep_cols, drop = FALSE]
        # normalise effect column names
        if ("2.5 %" %in% names(ef2)) names(ef2)[names(ef2) == "2.5 %"] <- "ci_low"
        if ("97.5 %" %in% names(ef2)) names(ef2)[names(ef2) == "97.5 %"] <- "ci_high"
        if ("e-value" %in% names(ef2)) names(ef2)[names(ef2) == "e-value"] <- "e_value"
        # merge
        out <- merge(out, ef2, by = c("outcome","shift_clean"), all.x = TRUE, sort = FALSE)
      }
    }
  }

  # De-duplicate rows in case `effect_table` supplies multiple entries per (outcome, shift)
  # Keep the first non-NA values across effect columns when duplicates exist.
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
  }

  # rounding
  num_cols <- vapply(out, is.numeric, logical(1))
  out[num_cols] <- lapply(out[num_cols], function(v) {
    ifelse(is.finite(v), round(as.numeric(v), digits = digits), v)
  })

  # Produce a compact table with short column names for kbl("markdown")
  if (isTRUE(compact)) {
    # formatter
    fmt <- function(x) ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), as.numeric(x)), NA_character_)
    # Build combined ATT [low, high] if effect columns present
    att_ci_str <- NULL
    if (all(c("att","ci_low","ci_high") %in% names(out))) {
      att_ci_str <- ifelse(is.finite(out$att) & is.finite(out$ci_low) & is.finite(out$ci_high),
                           paste0(fmt(out$att), " [", fmt(out$ci_low), ", ", fmt(out$ci_high), "]"),
                           ifelse(is.finite(out$att), fmt(out$att), NA_character_))
    }

    # Simple pretty label for shift_clean
    pretty_shift <- function(s) {
      s <- gsub("_", " ", s)
      s <- gsub("ipsi ", "Ipsi ", s, ignore.case = TRUE)
      s <- gsub("null", "Null", s, ignore.case = TRUE)
      tools::toTitleCase(s)
    }

    Shift <- vapply(out$shift_clean, pretty_shift, character(1))
    k <- out$prod_log10_threshold[1]
    # Build human-readable description
    prod_label_print <- "Prod < 10^k"
    if (is.finite(k)) {
      if (abs(k + 1) < 1e-8) {
        prod_label_print <- "Prod < 10%"
      } else if (abs(k + 2) < 1e-8) {
        prod_label_print <- "Prod < 1%"
      } else {
        prod_label_print <- paste0("Prod < 10^{", k, "}")
      }
    }

    zero_attr <- setNames(out$prop_zero_prod_pct, out$shift_clean)

    compact_df <- data.frame(
      Shift = Shift,
      Verdict = out$verdict,
      Prod = out$prod_frac_below_pct,
      ESS_per_Npt = out$ess_pos_frac_pt,
      p_hat = out$p_hat_overall,
      Delta_p = out$dp_hat_overall,
      stringsAsFactors = FALSE
    )
    if (!is.null(att_ci_str)) compact_df$`ATT [CI]` <- att_ci_str

    # Rename columns to friendly labels
    names(compact_df)[names(compact_df) == "Prod"] <- prod_label_print
    ess_label_print <- "ESS per N%"
    names(compact_df)[names(compact_df) == "ESS_per_Npt"] <- ess_label_print

    # Optionally drop policy-rate columns when not requested or all NA
    if (!isTRUE(include_policy_rates) || (all(is.na(compact_df$p_hat)) && all(is.na(compact_df$Delta_p)))) {
      compact_df$p_hat <- NULL
      compact_df$Delta_p <- NULL
    }

    # Attach explanation as an attribute for users to print alongside the table
    if (isTRUE(include_explanation)) {
      thr_txt <- paste0("k = ", k, "; verdict Pass when fraction ≤ ", round(100*thr$prod_frac_warn, 1), "%.")
      waves_txt <- if (is.null(waves)) "all available waves" else paste0("waves ", paste(waves, collapse = ", "))
      expl <- paste(
        "Notes:",
        "- Censoring handled via IPCW; zeros are summarised once in the accompanying text.",
        paste0("- '", prod_label_print, "' = % uncensored rows with log10(product of ratios across ", waves_txt, ") below k (", thr_txt, ")."),
        "- '", ess_label_print, "' = effective sample size relative to person-time among uncensored rows.",
        if (isTRUE(include_policy_rates)) "- 'p_hat' = policy-implied Pr(A_t=1) (uncensored); 'Delta_p' = difference vs null." else NULL,
        "- 'ATT [CI]' = average treatment effect with 95% CI.",
        sep = "\n"
      )
      attr(compact_df, "explanation") <- expl
    }

    attr(compact_df, "prop_zero_pct") <- zero_attr

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
