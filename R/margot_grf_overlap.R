#' Assess Covariate Overlap from Causal Forest Models
#'
#' Wrapper around the original GRF overlap diagnostics. This function exists
#' to provide a stable, descriptive name. It delegates to
#' `margot_assess_overlap()` for the implementation.
#'
#' @inheritParams margot_assess_overlap
#' @return A list with overlap summaries, plots, and text summary.
#' @export
margot_grf_overlap <- function(model_results,
                               model_names = NULL,
                               exposure_name = NULL,
                               label_mapping = NULL,
                               plot = TRUE,
                               save_plots = FALSE,
                               output_dir = NULL,
                               theme = "classic",
                               verbose = TRUE) {
  margot_assess_overlap(model_results = model_results,
                        model_names = model_names,
                        exposure_name = exposure_name,
                        label_mapping = label_mapping,
                        plot = plot,
                        save_plots = save_plots,
                        output_dir = output_dir,
                        theme = theme,
                        verbose = verbose)
}

