#' Plot LMTP density-ratio grid (waves x shifts)
#'
#' Thin wrapper around `margot_lmtp_overlap()` and
#' `margot_lmtp_overlap_plot_grid()` that returns a panel grid with optional
#' harmonised or user-specified y-axis limits for histogram counts.
#'
#' By default, histograms show only **uncensored observations** (r > 0) as zeros
#' primarily reflect dropout/censoring. Use `show_censored = TRUE` to include zeros.
#'
#' @param x LMTP run output with `$models` or any input accepted by
#'   `margot_lmtp_overlap()`.
#' @param outcome Character outcome to plot (required).
#' @param shifts Optional character vector of shifts to include (full or
#'   cleaned names).
#' @param label_mapping Optional label map for pretty titles.
#' @param show_censored Logical; if FALSE (default), histograms exclude zeros (r = 0).
#'   If TRUE, includes zeros. Censoring rate always shown in titles.
#' @param scale Character, "log10" or "linear" for the ratio scale (passed to
#'   `margot_lmtp_overlap()`).
#' @param theme Character ggplot theme keyword passed to
#'   `margot_lmtp_overlap()`.
#' @param ymax Optional numeric y-axis maximum for histogram counts across
#'   panels (passed to `margot_lmtp_overlap_plot_grid()`).
#' @param digits Integer rounding for summaries (not used in plot aesthetics).
#' @param color_by Character; how histogram fills are coloured (`"wave"`,
#'   `"shift"`, or `"constant"`).
#' @param color_by_wave Legacy logical alias for `color_by` (`TRUE` = `"wave"`,
#'   `FALSE` = `"constant"`).
#' @param fill_palette Optional vector of colours (named or unnamed) used when colouring histograms.
#' @param annotate_graph Character; controls graph annotations: `"waves"` places wave labels at top,
#'   `"shifts"` places shift labels at top, `"none"` disables annotations (default: `"none"`).
#' @param annotate_zeros Logical; if TRUE, adds "zeros: X%" label to top-right of each panel.
#'   Default is FALSE.
#' @param waves Optional integer vector specifying which waves to include (e.g., `c(1, 2, 3)`). If NULL,
#'   includes all waves found for the outcome.
#' @param ymax_harmonize Character or named vector; controls y-axis harmonization: `"none"` (default) gives
#'   each plot independent y-scale, `"row"` harmonizes within rows, `"column"` harmonizes within columns,
#'   `"global"` harmonizes all plots. Can also be a named vector with custom values (e.g., `c(wave_1 = 1000)`).
#' @param xlim_harmonize Character or named vector; controls x-axis harmonization: `"none"` (default) gives
#'   each plot independent x-scale, `"row"` harmonizes within rows, `"column"` harmonizes within columns,
#'   `"global"` harmonizes all plots. Can also be a named vector with custom values.
#' @param text_size Numeric size for facet annotations (wave/shift/zeros labels).
#' @return A patchwork grid object.
#' @export
margot_plot_lmtp_overlap_grid <- function(x,
                                          outcome,
                                          shifts = NULL,
                                          label_mapping = NULL,
                                          show_censored = FALSE,
                                          scale = "linear",
                                          theme = "empty",
                                          ymax = NULL,
                                          digits = 3,
                                          annotate_graph = c("none", "waves", "shifts"),
                                          annotate_zeros = FALSE,
                                          waves = NULL,
                                          xlim = NULL,
                                          layout = c("waves_by_shifts","shifts_by_waves"),
                                          ymax_harmonize = "none",
                                          xlim_harmonize = "none",
                                          headroom = 0.12,
                                          color_by = c("wave", "shift", "constant"),
                                          color_by_wave = NULL,
                                          fill_palette = NULL,
                                          text_size = 3,
                                          bins = 40,
                                          binwidth = NULL) {
  stopifnot(is.logical(annotate_zeros), length(annotate_zeros) == 1L)
  stopifnot(is.logical(show_censored), length(show_censored) == 1L)
  annotate_graph <- match.arg(annotate_graph)
  layout <- match.arg(layout)
  color_by <- match.arg(color_by)
  if (!is.null(color_by_wave)) {
    color_by <- if (isTRUE(color_by_wave)) "wave" else "constant"
  }
  ol <- margot_lmtp_overlap(
    x,
    outcomes = outcome,
    shifts = shifts,
    plot = TRUE,
    show_censored = show_censored,
    theme = theme,
    scale = scale,
    digits = digits,
    verbose = FALSE,
    color_by = color_by,
    color_by_wave = color_by_wave,
    fill_palette = fill_palette,
    bins = bins,
    binwidth = binwidth,
    xlim = xlim
  )
  # build a pretty outcome label for the main title
  outcome_label <- tryCatch({
    if (exists("transform_label", mode = "function")) {
      out <- transform_label(
        label = outcome,
        label_mapping = label_mapping,
        options = list(
          remove_tx_prefix = TRUE,
          remove_z_suffix = TRUE,
          remove_underscores = TRUE,
          use_title_case = TRUE
        )
      )
      if (is.null(out) || is.na(out)) outcome else out
    } else outcome
  }, error = function(e) outcome)

  margot_lmtp_overlap_plot_grid(
    ol,
    outcome = outcome,
    shifts = shifts,
    title = paste0(outcome_label, " — density ratio grid"),
    label_mapping = label_mapping,
    annotate_zeros = annotate_zeros,
    ymax = ymax,
    annotate_graph = annotate_graph,
    waves = waves,
    xlim = xlim,
    layout = layout,
    ymax_harmonize = ymax_harmonize,
    xlim_harmonize = xlim_harmonize,
    headroom = headroom,
    text_size = text_size
  )
}
