#' Compact positivity panel (all vs positive-only)
#'
#' Produces a compact table of positivity diagnostics for a selected
#' outcome/shift from `margot_lmtp_positivity()`, showing both the
#' all-weights summary and the positive-only (uncensored) summary.
#'
#' The panel includes, when available: censoring mass, ESS and ESS/N,
#' positive-only counts and ESS+/N+, and tail probabilities for chosen
#' thresholds (both all and positive-only).
#'
#' @param pos A result from `margot_lmtp_positivity()` (list with by_wave, overall, flags).
#' @param outcome Optional character. If provided, filter to this outcome.
#' @param shift Optional character. If provided, filter to this shift/policy.
#' @param include_overall Logical; if TRUE, include pooled "overall" rows.
#' @param digits Optional integer; if provided, round numeric columns.
#'
#' @return A data.frame (tibble if available) with a compact positivity panel.
#' @export
margot_positivity_panel <- function(pos,
                                    outcome = NULL,
                                    shift = NULL,
                                    include_overall = TRUE,
                                    digits = 3) {

  stopifnot(is.list(pos), !is.null(pos$by_wave))

  as_tb <- function(x) if (isTRUE(requireNamespace("tibble", quietly = TRUE))) tibble::as_tibble(x) else x

  by_wave <- pos$by_wave
  overall <- pos$overall

  # Optional filtering
  if (!is.null(outcome)) {
    by_wave <- by_wave[by_wave$outcome == outcome, , drop = FALSE]
    if (is.data.frame(overall) && nrow(overall)) {
      overall <- overall[overall$outcome == outcome, , drop = FALSE]
    }
  }
  if (!is.null(shift)) {
    by_wave <- by_wave[by_wave$shift == shift, , drop = FALSE]
    if (is.data.frame(overall) && nrow(overall)) {
      overall <- overall[overall$shift == shift, , drop = FALSE]
    }
  }

  # Unique rows by outcome/shift/wave
  if (nrow(by_wave)) {
    by_wave <- unique(by_wave)
  }
  if (is.data.frame(overall) && nrow(overall)) {
    overall <- unique(overall)
  }

  # Columns of interest (keep only those that exist)
  qcols       <- grep('^q[0-9]+$', names(by_wave), value = TRUE)
  qcols_pos   <- paste0(qcols, "_pos")
  tails       <- grep('^p_gt_\\d+$', names(by_wave), value = TRUE)
  tails_pos   <- paste0(tails, "_pos")

  base_cols <- c("outcome", "shift", "wave", "n",
                 "n_pos", "prop_zero", "prop_nonzero",
                 "ess", "ess_frac", "ess_pos", "ess_pos_frac")

  want_cols <- unique(c(base_cols, qcols, qcols_pos, tails, tails_pos))
  have_cols <- intersect(want_cols, names(by_wave))
  panel <- by_wave[, have_cols, drop = FALSE]

  # Optionally append overall
  if (include_overall && is.data.frame(overall) && nrow(overall)) {
    have_cols_ov <- intersect(want_cols, names(overall))
    ov <- overall[, have_cols_ov, drop = FALSE]
    panel <- rbind(panel, ov)
  }

  # Order: by outcome, shift, wave (numeric waves first, then overall)
  if (nrow(panel)) {
    wave_num <- suppressWarnings(as.numeric(panel$wave))
    is_overall <- is.na(wave_num) & (tolower(as.character(panel$wave)) == "overall")
    wave_order <- ifelse(is_overall, max(wave_num, na.rm = TRUE) + 1, wave_num)
    ord <- order(panel$outcome, panel$shift, wave_order, na.last = TRUE)
    panel <- panel[ord, , drop = FALSE]
  }

  # Rounding (numeric columns only)
  if (!is.null(digits) && nrow(panel)) {
    num_cols <- vapply(panel, is.numeric, logical(1))
    panel[num_cols] <- lapply(panel[num_cols], round, digits = digits)
  }

  as_tb(panel)
}

