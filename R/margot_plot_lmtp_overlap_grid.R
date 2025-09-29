#' Plot LMTP density-ratio grid (waves x shifts)
#'
#' Thin wrapper around `margot_lmtp_overlap()` and
#' `margot_lmtp_overlap_plot_grid()` that returns a panel grid with optional
#' harmonised or user-specified y-axis limits for histogram counts.
#'
#' @param x LMTP run output with `$models` or any input accepted by
#'   `margot_lmtp_overlap()`.
#' @param outcome Character outcome to plot (required).
#' @param shifts Optional character vector of shifts to include (full or
#'   cleaned names).
#' @param label_mapping Optional label map for pretty titles.
#' @param scale Character, "log10" or "linear" for the ratio scale (passed to
#'   `margot_lmtp_overlap()`).
#' @param theme Character ggplot theme keyword passed to
#'   `margot_lmtp_overlap()`.
#' @param ymax Optional numeric y-axis maximum for histogram counts across
#'   panels (passed to `margot_lmtp_overlap_plot_grid()`).
#' @param digits Integer rounding for summaries (not used in plot aesthetics).
#' @return A patchwork grid object.
#' @export
margot_plot_lmtp_overlap_grid <- function(x,
                                          outcome,
                                          shifts = NULL,
                                          label_mapping = NULL,
                                          scale = "linear",
                                          theme = "empty",
                                          ymax = NULL,
                                          digits = 3) {
  ol <- margot_lmtp_overlap(
    x,
    outcomes = outcome,
    shifts = shifts,
    plot = TRUE,
    theme = theme,
    scale = scale,
    digits = digits,
    verbose = FALSE
  )
  margot_lmtp_overlap_plot_grid(
    ol,
    outcome = outcome,
    shifts = shifts,
    label_mapping = label_mapping,
    annotate_zeros = "column",
    ymax = ymax
  )
}

