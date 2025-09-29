#' One-stop LMTP positivity/overlap reporting for an analysis
#'
#' Builds manuscript-ready positivity diagnostics (overall and by-wave) and an
#' optional density-ratio grid for a selected outcome and set of shifts.
#'
#' - Computes summaries via `margot_lmtp_positivity()`
#' - Produces overall table (Estimand, N, Prop_zero, ESS, ESS/N)
#' - Produces combined by-wave tables (ESS/N by wave; ESS by wave)
#' - Optionally returns a wave-by-shift density-ratio grid using
#'   `margot_lmtp_overlap()` and `margot_lmtp_overlap_plot_grid()` with
#'   harmonised or user-specified histogram heights
#'
#' @param x LMTP run output with `$models` (e.g., result from `margot_lmtp()`),
#'   or a list compatible with `margot_lmtp_positivity()`.
#' @param outcome Character; outcome name to report (required).
#' @param shifts Optional character vector of shifts/policies to include;
#'   accepts full names (`t5_pwi_z_shift_up`) or cleaned (`shift_up`). If NULL,
#'   includes all available shifts for the outcome.
#' @param label_mapping Optional named list to prettify estimand labels via
#'   `transform_label()`; if missing, minimal cleaning is applied.
#' @param digits Integer rounding for numeric outputs.
#' @param include_plots Logical; if TRUE, returns `overlap_grid`.
#' @param ymax Optional numeric; if supplied, sets common y-limit for histogram
#'   counts in the density-ratio grid; otherwise a common limit is inferred.
#'
#' @return A list with elements:
#'   - overall: tibble with Estimand, N, Prop_zero, ESS, ESS/N
#'   - by_wave_ess_frac: tibble wide (Wave x Estimand) with ESS/N
#'   - by_wave_ess: tibble wide (Wave x Estimand) with ESS
#'   - flags: tibble of positivity flags (ESS+/(N+) removed)
#'   - overlap_grid: patchwork object (if include_plots = TRUE)
#'   - text_summary: brief prose summary
#' @export
margot_report_lmtp_positivity <- function(x,
                                          outcome,
                                          shifts = NULL,
                                          label_mapping = NULL,
                                          digits = 2,
                                          include_plots = TRUE,
                                          ymax = NULL) {

  stopifnot(is.character(outcome), length(outcome) == 1L)

  clean_shift_names <- function(df) {
    if (is.null(df) || !nrow(df)) return(df)
    prefix <- paste0(df$outcome, "_")
    df$shift_clean <- ifelse(startsWith(df$shift, prefix),
                             substring(df$shift, nchar(prefix) + 1L),
                             df$shift)
    df
  }

  # Compute positivity/overlap summaries
  pos <- margot_lmtp_positivity(x, digits = max(3L, digits))
  pos <- lapply(pos, clean_shift_names)

  # Filter outcome + shifts
  available <- unique(pos$overall$outcome)
  if (!(outcome %in% available)) {
    stop("Outcome not found in positivity summaries: ", outcome)
  }
  pos_overall <- pos$overall[pos$overall$outcome == outcome, , drop = FALSE]
  pos_by_wave <- pos$by_wave[pos$by_wave$outcome == outcome, , drop = FALSE]
  pos_flags   <- pos$flags[pos$flags$outcome == outcome, , drop = FALSE]

  if (!is.null(shifts)) {
    keep <- pos_overall$shift %in% shifts | pos_overall$shift_clean %in% shifts
    pos_overall <- pos_overall[keep, , drop = FALSE]
    keep <- pos_by_wave$shift %in% shifts | pos_by_wave$shift_clean %in% shifts
    pos_by_wave <- pos_by_wave[keep, , drop = FALSE]
    keep <- pos_flags$shift %in% shifts | pos_flags$shift_clean %in% shifts
    pos_flags   <- pos_flags[keep, , drop = FALSE]
  }

  # Build estimand labels
  estimand_label <- function(sh) {
    sh_clean <- sh
    if (exists("transform_label", mode = "function")) {
      out <- tryCatch(
        transform_label(
          label = sh_clean,
          label_mapping = label_mapping,
          options = list(
            remove_tx_prefix = TRUE,
            remove_z_suffix = TRUE,
            remove_underscores = TRUE,
            use_title_case = TRUE
          )
        ),
        error = function(e) sh_clean
      )
      if (!is.null(out) && !is.na(out)) return(out)
    }
    gsub("_", " ", tools::toTitleCase(sh_clean))
  }

  # Overall table (Estimand rows)
  ov <- pos_overall
  ov$Estimand <- vapply(ov$shift_clean, estimand_label, character(1L))
  overall <- ov[, c("Estimand", "n", "prop_zero", "ess", "ess_frac"), drop = FALSE]
  names(overall) <- c("Estimand", "N", "Prop_zero", "ESS", "ESS/N")
  if (isTRUE(digits >= 0)) {
    overall$Prop_zero <- round(overall$Prop_zero, digits)
    overall$`ESS/N`   <- round(overall$`ESS/N`, digits)
  }

  # By-wave wide tables
  bw <- pos_by_wave
  if (nrow(bw)) {
    bw$Estimand <- vapply(bw$shift_clean, estimand_label, character(1L))
    bw$Wave <- suppressWarnings(as.integer(bw$wave))
    by_wave_ess_frac <- tidyr::pivot_wider(
      bw[, c("Wave", "Estimand", "ess_frac"), drop = FALSE],
      names_from = "Estimand",
      values_from = "ess_frac"
    )
    by_wave_ess <- tidyr::pivot_wider(
      bw[, c("Wave", "Estimand", "ess"), drop = FALSE],
      names_from = "Estimand",
      values_from = "ess"
    )
    if (isTRUE(digits >= 0)) {
      by_wave_ess_frac[, -1] <- lapply(by_wave_ess_frac[, -1, drop = FALSE], round, digits = digits)
      by_wave_ess[, -1]      <- lapply(by_wave_ess[, -1, drop = FALSE], function(v) round(v))
    }
  } else {
    by_wave_ess_frac <- by_wave_ess <- tibble::tibble()
  }

  # Flags: drop ESS+/(N+) flags per default manuscript reporting
  if (nrow(pos_flags)) {
    pos_flags <- pos_flags[!grepl("^ESS\\+/$N\\+ <\\s*0.50$", pos_flags$flag_reason), , drop = FALSE]
  }

  # Optional overlap plot grid
  overlap_grid <- NULL
  if (isTRUE(include_plots)) {
    ol <- margot_lmtp_overlap(x, outcomes = outcome, shifts = unique(ov$shift), plot = TRUE, theme = "empty", scale = "linear", digits = 3, verbose = FALSE)
    overlap_grid <- tryCatch(
      margot_lmtp_overlap_plot_grid(ol, outcome = outcome, shifts = unique(ov$shift), title = paste0(outcome, " — density ratio grid"), label_mapping = label_mapping, annotate_zeros = "column", ymax = ymax),
      error = function(e) NULL
    )
  }

  list(
    overall = overall,
    by_wave_ess_frac = by_wave_ess_frac,
    by_wave_ess = by_wave_ess,
    flags = pos_flags,
    overlap_grid = overlap_grid,
    text_summary = sprintf("Across selected LMTP models: median zeros = %.1f%%, median ESS/N = %.3f.", 100*median(ov$prop_zero, na.rm = TRUE), median(ov$`ESS/N`, na.rm = TRUE))
  )
}

#' @export
margot_lmtp_positivity_report <- function(...) {
  # Backwards-compatible alias
  margot_report_lmtp_positivity(...)
}
