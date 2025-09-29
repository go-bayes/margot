#' Plot LMTP positivity (overall and by-wave ESS/N)
#'
#' Generates two ggplots from `margot_report_lmtp_positivity()` results:
#' a bar plot of overall ESS/N by estimand, and a line plot of ESS/N by wave.
#'
#' @param x Either a report list from `margot_report_lmtp_positivity()` or a
#'   model object acceptable to `margot_report_lmtp_positivity()` when
#'   `outcome` is provided.
#' @param outcome Character outcome (required if `x` is a model object).
#' @param shifts Optional character vector of shifts/policies (full or cleaned).
#' @param label_mapping Optional label mapping passed to the reporter.
#' @param digits Integer rounding for the reporter.
#' @param ymax Unused here; exists for interface consistency (use
#'   `margot_plot_lmtp_overlap_grid()` to adjust histogram heights).
#'
#' @return A list with `overall_plot`, `by_wave_plot`, and the underlying
#'   `report`.
#' @export
margot_plot_lmtp_positivity <- function(x,
                                        outcome = NULL,
                                        shifts = NULL,
                                        label_mapping = NULL,
                                        digits = 2,
                                        ymax = NULL) {

  is_report <- is.list(x) && all(c("overall","by_wave_ess_frac") %in% names(x))
  if (!is_report) {
    stopifnot(!is.null(outcome))
    rep <- margot_report_lmtp_positivity(
      x,
      outcome = outcome,
      shifts = shifts,
      label_mapping = label_mapping,
      digits = digits,
      include_plots = FALSE
    )
  } else {
    rep <- x
  }

  ov <- rep$overall
  ov$Estimand <- factor(ov$Estimand, levels = ov$Estimand)
  p_overall <- ggplot2::ggplot(ov, ggplot2::aes(x = Estimand, y = `ESS/N`)) +
    ggplot2::geom_col(fill = "#4f88c6") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", `ESS/N`)),
                       vjust = -0.3, size = 3) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = "Overall ESS/N by Estimand",
                  x = NULL, y = "ESS/N") +
    ggplot2::ylim(0, max(1, max(ov$`ESS/N`, na.rm = TRUE) * 1.1)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

  bw <- rep$by_wave_ess_frac
  if (nrow(bw)) {
    bw_long <- tidyr::pivot_longer(bw, cols = -Wave, names_to = "Estimand", values_to = "ESS_N")
    p_by_wave <- ggplot2::ggplot(bw_long, ggplot2::aes(x = Wave, y = ESS_N, color = Estimand)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = "ESS/N by Wave", x = "Wave", y = "ESS/N") +
      ggplot2::scale_x_continuous(breaks = sort(unique(bw_long$Wave))) +
      ggplot2::ylim(0, max(1, max(bw_long$ESS_N, na.rm = TRUE) * 1.1))
  } else {
    p_by_wave <- NULL
  }

  list(
    overall_plot = p_overall,
    by_wave_plot = p_by_wave,
    report = rep
  )
}

