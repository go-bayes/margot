#' Batch Process and Plot RATE Curves for Multiple Models (New Function)
#'
#' This function processes a subset of models (or all models by default), creates RATE (Rank Average Treatment Effect)
#' plots for each model using the margot package, and optionally saves the plots.
#'
#' @param models_binary A list of model results, where each element contains a 'rate_result' component.
#' @param model_names Optional character vector of model names to process. Default NULL (all models).
#' @param dpi The resolution of the saved plots in dots per inch. Default is 300.
#' @param width The width of the saved plots in inches. Default is 12.
#' @param height The height of the saved plots in inches. Default is 8.
#' @param save_plots Logical indicating whether to save the plots to disk. Default is TRUE.
#' @param output_dir The directory where plots should be saved. Default is "rate_plots".
#'
#' @return A list containing the generated ggplot objects for each processed model.
#' @export
margot_plot_rate_batch <- function(models_binary,
                                   model_names     = NULL,
                                   dpi             = 300,
                                   width           = 12,
                                   height          = 8,
                                   save_plots      = TRUE,
                                   output_dir      = "rate_plots") {
  # ensure margot package is installed
  if (!requireNamespace("margot", quietly = TRUE)) {
    stop("Package 'margot' is required but not installed. Please install it first.")
  }

  # create output directory if it doesn't exist
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }

  # determine which models to process
  all_models <- names(models_binary$results)
  if (!is.null(model_names)) {
    missing <- setdiff(model_names, all_models)
    if (length(missing) > 0) {
      warning("Models not found: ", paste(missing, collapse = ", "))
    }
    selected_models <- intersect(model_names, all_models)
  } else {
    selected_models <- all_models
  }

  # initialise list to store plots
  rate_plots <- list()

  # loop through each selected model
  for (model_name in selected_models) {
    tryCatch({
      # extract rate result for this model
      rate_result <- models_binary$results[[model_name]]$rate_result

      # create RATE plot
      plot <- margot::margot_plot_rate(rate_result)

      # store plot in list
      rate_plots[[model_name]] <- plot

      # save plot to disk if requested
      if (save_plots) {
        file_name <- file.path(output_dir, paste0(model_name, "_rate_plot.png"))
        ggplot2::ggsave(file_name, plot, dpi = dpi, width = width, height = height)
        message("Saved ", file_name)
      }
    },
    error = function(e) {
      warning("Error processing model ", model_name, ": ", e$message)
    })
  }

  # return list of plots
  return(rate_plots)
}

#' @rdname margot_plot_rate_batch
#' @export
margot_plot_batch_rate <- function(models_binary,
                                   model_names     = NULL,
                                   dpi             = 300,
                                   width           = 12,
                                   height          = 8,
                                   save_plots      = TRUE,
                                   output_dir      = "rate_plots") {
  .Deprecated("margot_plot_rate_batch",
              msg = "margot_plot_batch_rate is deprecated. Use margot_plot_rate_batch instead.")
  margot_plot_rate_batch(models_binary,
                         model_names    = model_names,
                         dpi            = dpi,
                         width          = width,
                         height         = height,
                         save_plots     = save_plots,
                         output_dir     = output_dir)
}
