#' Detailed quantiles and tails (all vs positive-only)
#'
#' For a given outcome/shift and wave (or overall), return a compact
#' table of quantiles and tail probabilities from `margot_lmtp_positivity()`
#' side-by-side for all weights vs positive-only weights.
#'
#' @param pos A result from `margot_lmtp_positivity()`.
#' @param outcome Character. Outcome name to filter.
#' @param shift Character. Shift/policy name. Can be the full shift name
#'   (e.g., "t5_pwi_z_shift_up") or the cleaned suffix (e.g., "shift_up").
#' @param wave Character or numeric. A specific wave index or "overall".
#' @param digits Optional integer; if provided, round numeric columns.
#'
#' @return A list with two elements:
#'   - header: one-row tibble with outcome, shift, wave, n, n_pos, prop_zero,
#'             prop_nonzero, ess, ess_frac, ess_pos, ess_pos_frac
#'   - panel: tibble with columns metric, all, pos, diff (pos - all)
#' @export
margot_positivity_tails_panel <- function(pos,
                                          outcome,
                                          shift,
                                          wave = "overall",
                                          digits = 3) {
  stopifnot(is.list(pos), !is.null(pos$by_wave))

  as_tb <- function(x) if (isTRUE(requireNamespace("tibble", quietly = TRUE))) tibble::as_tibble(x) else x

  # Build data with cleaned shift to allow suffix matching
  add_clean <- function(df) {
    if (!nrow(df)) return(df)
    sh <- df$shift
    out <- df$outcome
    starts <- startsWith(sh, paste0(out, "_"))
    df$shift_clean <- ifelse(starts, substring(sh, nchar(out) + 2L), sh)
    df
  }

  by_wave <- add_clean(pos$by_wave)
  overall <- if (is.data.frame(pos$overall)) add_clean(pos$overall) else NULL

  # Select row by wave
  get_row <- function(df, outcome, shift, wave) {
    if (!nrow(df)) return(df)
    # wave matching: allow numeric or "overall"
    if (identical(wave, "overall")) {
      df <- df[df$wave %in% c("overall", NA_character_), , drop = FALSE]
    } else {
      # coerce wave to numeric for comparison
      suppressWarnings({ wnum <- as.numeric(wave) })
      df <- df[df$wave == wnum, , drop = FALSE]
    }
    # outcome + shift match (either exact shift or cleaned)
    keep <- (df$outcome == outcome) & (df$shift == shift | df$shift_clean == shift)
    df[keep, , drop = FALSE]
  }

  row <- NULL
  if (isTRUE(tolower(as.character(wave)) == "overall") && !is.null(overall)) {
    row <- get_row(overall, outcome, shift, "overall")
  }
  if (is.null(row) || !nrow(row)) {
    row <- get_row(by_wave, outcome, shift, wave)
  }
  if (is.null(row) || !nrow(row)) {
    stop("No matching row for outcome=", outcome, ", shift=", shift, ", wave=", wave)
  }
  # Deduplicate if needed
  row <- unique(row)
  row <- row[1, , drop = FALSE]

  # Header
  header_cols <- c("outcome","shift","shift_clean","wave","n","n_pos",
                   "prop_zero","prop_nonzero","ess","ess_frac","ess_pos","ess_pos_frac")
  header_cols <- intersect(header_cols, names(row))
  header <- row[, header_cols, drop = FALSE]

  # Gather quantiles and tails (all vs pos)
  q_all   <- grep('^q[0-9]+$', names(row), value = TRUE)
  q_pos   <- grep('^q[0-9]+_pos$', names(row), value = TRUE)
  t_all   <- grep('^p_gt_\\d+$', names(row), value = TRUE)
  t_pos   <- grep('^p_gt_\\d+_pos$', names(row), value = TRUE)

  # Align names
  strip_pos <- function(nm) sub("_pos$", "", nm)
  base_names <- unique(c(q_all, strip_pos(q_pos), t_all, strip_pos(t_pos)))

  # Build panel
  make_row <- function(nm) {
    all_val <- if (nm %in% names(row)) row[[nm]] else NA_real_
    pos_val <- if (paste0(nm, "_pos") %in% names(row)) row[[paste0(nm, "_pos")]] else NA_real_
    data.frame(metric = nm, all = as.numeric(all_val), pos = as.numeric(pos_val),
               diff = as.numeric(pos_val - all_val), check.names = FALSE)
  }
  panel <- do.call(rbind, lapply(base_names, make_row))

  # Order metrics: quantiles (numeric order), then tails (numeric order)
  is_q   <- grepl('^q[0-9]+$', panel$metric)
  is_tail<- grepl('^p_gt_\\d+$', panel$metric)
  q_num  <- suppressWarnings(as.numeric(sub('^q', '', panel$metric)))
  t_num  <- suppressWarnings(as.numeric(sub('^p_gt_', '', panel$metric)))
  ord <- order(!is_q, q_num, !is_tail, t_num, na.last = TRUE)
  panel <- panel[ord, , drop = FALSE]

  # Rounding
  if (!is.null(digits)) {
    panel$all  <- round(panel$all, digits)
    panel$pos  <- round(panel$pos, digits)
    panel$diff <- round(panel$diff, digits)
    num_cols <- vapply(header, is.numeric, logical(1))
    header[num_cols] <- lapply(header[num_cols], round, digits = digits)
  }

  list(header = as_tb(header), panel = as_tb(panel))
}

