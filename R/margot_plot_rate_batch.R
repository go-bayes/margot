#' Batch Process and Plot RATE Curves for Multiple Models
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
#' @param remove_tx_prefix Logical value indicating whether to remove the "tx_" prefix from labels. Default is TRUE.
#' @param remove_z_suffix Logical value indicating whether to remove the "_z" suffix from labels. Default is TRUE.
#' @param use_title_case Logical value indicating whether to convert labels to title case. Default is TRUE.
#' @param remove_underscores Logical value indicating whether to remove underscores from labels. Default is TRUE.
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#'
#' @return A list containing the generated ggplot objects for each processed model.
#' @export
margot_plot_rate_batch <- function(models_binary,
                                   model_names = NULL,
                                   dpi = 300,
                                   width = 12,
                                   height = 8,
                                   save_plots = TRUE,
                                   output_dir = "rate_plots",
                                   remove_tx_prefix = TRUE,
                                   remove_z_suffix = TRUE,
                                   use_title_case = TRUE,
                                   remove_underscores = TRUE,
                                   label_mapping = NULL) {
  # ensure margot package is installed
  if (!requireNamespace("margot", quietly = TRUE)) {
    stop("Package 'margot' is required but not installed. Please install it first.")
  }

  cli::cli_h1("Margot Batch RATE Plots")

  # create output directory if it doesn't exist
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_success("Created output directory: {output_dir}")
  }

  # determine which models to process
  all_models <- names(models_binary$results)
  if (!is.null(model_names)) {
    missing <- setdiff(model_names, all_models)
    if (length(missing) > 0) {
      cli::cli_alert_warning("Models not found: {paste(missing, collapse = ', ')}")
    }
    selected_models <- intersect(model_names, all_models)
  } else {
    selected_models <- all_models
  }

  cli::cli_alert_info("Processing {length(selected_models)} models")

  # initialise list to store plots
  rate_plots <- list()

  # loop through each selected model
  for (model_name in selected_models) {
    cli::cli_h2("Processing model: {model_name}")

    tryCatch({
      # extract rate result for this model
      rate_result <- models_binary$results[[model_name]]$rate_result

      if (is.null(rate_result)) {
        cli::cli_alert_warning("No RATE result found for model: {model_name}")
        next
      }

      # create RATE plot with transformed labels
      plot <- margot_plot_rate(
        x = rate_result,
        outcome_var = model_name,
        remove_tx_prefix = remove_tx_prefix,
        remove_z_suffix = remove_z_suffix,
        use_title_case = use_title_case,
        remove_underscores = remove_underscores,
        label_mapping = label_mapping
      )

      # store plot in list
      rate_plots[[model_name]] <- plot

      # save plot to disk if requested
      if (save_plots) {
        # transform model name for filename
        transformed_model_name <- transform_var_name(
          model_name,
          label_mapping,
          remove_tx_prefix,
          remove_z_suffix,
          FALSE,  # don't use title case for filenames
          FALSE   # don't remove underscores for filenames
        )

        # clean up filename (replace spaces with underscores)
        file_name <- file.path(output_dir, paste0(
          gsub("[^a-zA-Z0-9_]", "", gsub(" ", "_", model_name)),
          "_rate_plot.png"
        ))

        ggplot2::ggsave(file_name, plot, dpi = dpi, width = width, height = height)
        cli::cli_alert_success("Saved {file_name}")
      }
    },
    error = function(e) {
      cli::cli_alert_danger("Error processing model {model_name}: {e$message}")
    })
  }

  cli::cli_alert_success("Completed processing {length(rate_plots)} models")

  # return list of plots
  return(rate_plots)
}

