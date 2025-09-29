#' Batch plot/report LMTP positivity for multiple analyses
#'
#' Convenience wrapper to run `margot_report_lmtp_positivity()` and
#' `margot_plot_lmtp_positivity()` across a list of analyses.
#'
#' @param analyses A list of spec lists, each with `x`, `outcome`, optional
#'   `shifts`, and optional `name`.
#' @param label_mapping Optional label map passed through to reporters/plots.
#' @param digits Integer rounding for numeric outputs.
#' @param include_plots Logical; include overlap grid in the report.
#' @param ymax Optional numeric for histogram y-axis alignment in overlap grid.
#'
#' @return A named list; each element contains `report`, `plots`.
#' @export
margot_plot_lmtp_positivity_batch <- function(analyses,
                                              label_mapping = NULL,
                                              digits = 2,
                                              include_plots = TRUE,
                                              ymax = NULL) {
  stopifnot(is.list(analyses), length(analyses) >= 1L)
  out <- list()
  for (i in seq_along(analyses)) {
    spec <- analyses[[i]]
    nm <- spec$name %||% paste0("analysis_", i)
    rep <- margot_report_lmtp_positivity(
      spec$x,
      outcome = spec$outcome,
      shifts = spec$shifts %||% NULL,
      label_mapping = label_mapping,
      digits = digits,
      include_plots = include_plots,
      ymax = ymax
    )
    plots <- margot_plot_lmtp_positivity(rep)
    out[[nm]] <- list(report = rep, plots = plots)
  }
  out
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

