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
                                          digits = 3,
                                          annotate_wave = TRUE,
                                          annotate_zeros = FALSE,
                                          waves = NULL,
                                          remove_waves = NULL,
                                          xlim = NULL,
                                          layout = c("waves_by_shifts","shifts_by_waves"),
                                          annotate_shift = TRUE,
                                          ymax_by_wave = NULL,
                                          headroom = 0.06,
                                          color_by_wave = TRUE,
                                          bins = 40,
                                          binwidth = NULL) {
  stopifnot(is.logical(annotate_zeros), length(annotate_zeros) == 1L)
  stopifnot(is.logical(annotate_wave),  length(annotate_wave)  == 1L)
  stopifnot(is.logical(annotate_shift), length(annotate_shift) == 1L)
  layout <- match.arg(layout)
  ol <- margot_lmtp_overlap(
    x,
    outcomes = outcome,
    shifts = shifts,
    plot = TRUE,
    theme = theme,
    scale = scale,
    digits = digits,
    verbose = FALSE,
    color_by_wave = color_by_wave,
    bins = bins,
    binwidth = binwidth,
    xlim = xlim
  )
  # Build a pretty outcome label for the main title
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
    annotate_wave = annotate_wave,
    annotate_shift = annotate_shift,
    waves = waves,
    remove_waves = remove_waves,
    xlim = xlim,
    layout = layout,
    ymax_by_wave = ymax_by_wave,
    headroom = headroom
  )
}
