#' Batch Process and Plot RATE Curves for Multiple Models
#'
#' This function processes a subset of models (or all models by default), creates RATE (Rank Average Treatment Effect)
#' plots for each model using the margot package.
#'
#' @param models_binary A list of model results, where each element contains a 'rate_result' component.
#' @param model_names Optional character vector of model names to process. Default NULL (all models).
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
    },
    error = function(e) {
      cli::cli_alert_danger("Error processing model {model_name}: {e$message}")
    })
  }

  cli::cli_alert_success("Completed processing {length(rate_plots)} models")

  # return list of plots
  return(rate_plots)
}

