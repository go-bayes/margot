#' Batch Process and Plot QINI Curves for Multiple Models
#'
#' This function processes a subset of models (or all models by default), creates QINI (Qini coefficient)
#' plots for each model using the margot package, and optionally saves the plots.
#'
#' @param mc_result A list containing the results from margot_causal_forest().
#' @param model_names Optional character vector of model names to process. Can be specified with or without
#'        the "model_" prefix. Default NULL (all models).
#' @param dpi The resolution of the saved plots in dots per inch. Default is 300.
#' @param width The width of the saved plots in inches. Default is 12.
#' @param height The height of the saved plots in inches. Default is 8.
#' @param save_plots Logical indicating whether to save the plots to disk. Default is TRUE.
#' @param output_dir The directory where plots should be saved. Default is "qini_plots".
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#' @param spend_levels Numeric vector of spend levels to show with vertical lines. Default is c(0.2, 0.5).
#' @param show_spend_lines Logical indicating whether to show vertical lines at spend levels. Default is TRUE.
#' @param spend_line_color Color for spend level lines. Default is "red".
#' @param spend_line_alpha Alpha transparency for spend lines. Default is 0.5.
#' @param theme Character string specifying the ggplot2 theme. Default is "classic".
#' @param show_ci Logical indicating whether to show confidence intervals. Default is FALSE.
#' @param ci_alpha Significance level for confidence intervals. Default is 0.05.
#' @param ci_n_points Number of points at which to compute confidence intervals. Default is 20.
#' @param ci_ribbon_alpha Alpha transparency for confidence interval ribbons. Default is 0.3.
#' @param ci_ribbon_color Color for confidence interval ribbons. If NULL (default), uses the curve color.
#' @param horizontal_line Logical indicating whether to draw horizontal lines where Qini curves plateau
#'   when the path is complete. Default is TRUE.
#' @param grid_step Integer specifying the step size for subsampling the curve data. If NULL (default),
#'   uses max(floor(nrow(qini_data) / 1000), 1). Set to 1 to plot all points.
#' @param ylim Numeric vector of length 2 specifying the y-axis limits c(min, max). Default is NULL (automatic scaling).
#' @param baseline_method Method for generating baseline: "auto" (default), "straight", 
#'   "maq_no_covariates", or "none". See details in margot_generate_qini_data().
#'
#' @return A list containing the generated ggplot objects for each processed model.
#' 
#' @examples
#' \dontrun{
#' # Process all models
#' qini_plots <- margot_plot_qini_batch(mc_result)
#' 
#' # Process specific models with confidence intervals
#' qini_plots <- margot_plot_qini_batch(
#'   mc_result,
#'   model_names = c("t2_belong_z", "t2_meaning_z"),
#'   show_ci = TRUE,
#'   ci_n_points = 50
#' )
#' 
#' # Custom label mapping
#' label_mapping <- list(
#'   "t2_belong_z" = "Belonging",
#'   "t2_meaning_z" = "Meaning in Life"
#' )
#' qini_plots <- margot_plot_qini_batch(
#'   mc_result,
#'   label_mapping = label_mapping
#' )
#' }
#' 
#' @export
margot_plot_qini_batch <- function(mc_result,
                                   model_names = NULL,
                                   dpi = 300,
                                   width = 12,
                                   height = 8,
                                   save_plots = TRUE,
                                   output_dir = "qini_plots",
                                   label_mapping = NULL,
                                   spend_levels = c(0.2, 0.5),
                                   show_spend_lines = TRUE,
                                   spend_line_color = "red",
                                   spend_line_alpha = 0.5,
                                   theme = "classic",
                                   show_ci = FALSE,
                                   ci_alpha = 0.05,
                                   ci_n_points = 20,
                                   ci_ribbon_alpha = 0.3,
                                   ci_ribbon_color = NULL,
                                   horizontal_line = TRUE,
                                   grid_step = NULL,
                                   ylim = NULL,
                                   baseline_method = "auto") {
  
  cli::cli_h1("Margot Batch QINI Plots")
  
  # create output directory if it doesn't exist
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_success("Created output directory: {output_dir}")
  }
  
  # determine which models to process
  all_models <- names(mc_result$results)
  
  if (!is.null(model_names)) {
    # handle model names with or without "model_" prefix
    model_names_with_prefix <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    
    missing <- setdiff(model_names_with_prefix, all_models)
    if (length(missing) > 0) {
      cli::cli_alert_warning("Models not found: {paste(gsub('model_', '', missing), collapse = ', ')}")
    }
    selected_models <- intersect(model_names_with_prefix, all_models)
  } else {
    selected_models <- all_models
  }
  
  # all selected models can now be processed since we generate QINI data on-demand
  models_to_process <- selected_models
  
  cli::cli_alert_info("Processing {length(models_to_process)} models")
  
  # initialise list to store plots
  qini_plots <- list()
  
  # loop through each selected model
  for (model_name in models_to_process) {
    cli::cli_h2("Processing model: {gsub('model_', '', model_name)}")
    
    tryCatch({
      # create QINI plot
      plot <- margot_plot_qini(
        mc_result = mc_result,
        outcome_var = model_name,
        label_mapping = label_mapping,
        spend_levels = spend_levels,
        show_spend_lines = show_spend_lines,
        spend_line_color = spend_line_color,
        spend_line_alpha = spend_line_alpha,
        theme = theme,
        show_ci = show_ci,
        ci_alpha = ci_alpha,
        ci_n_points = ci_n_points,
        ci_ribbon_alpha = ci_ribbon_alpha,
        ci_ribbon_color = ci_ribbon_color,
        horizontal_line = horizontal_line,
        grid_step = grid_step,
        ylim = ylim,
        baseline_method = baseline_method
      )
      
      # store plot in list
      qini_plots[[model_name]] <- plot
      
      # save plot to disk if requested
      if (save_plots) {
        # clean model name for filename
        clean_name <- gsub("^model_", "", model_name)
        file_name <- file.path(output_dir, paste0(
          gsub("[^a-zA-Z0-9_]", "", gsub(" ", "_", clean_name)),
          "_qini_plot.png"
        ))
        
        ggplot2::ggsave(file_name, plot, dpi = dpi, width = width, height = height)
        cli::cli_alert_success("Saved {file_name}")
      }
    },
    error = function(e) {
      cli::cli_alert_danger("Error processing model {model_name}: {e$message}")
    })
  }
  
  cli::cli_alert_success("Completed processing {length(qini_plots)} models")
  
  # return list of plots
  return(qini_plots)
}