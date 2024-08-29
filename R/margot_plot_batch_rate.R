#' Batch Process and Plot RATE Curves for Multiple Models
#'
#' This function processes multiple models, creates RATE (Rank Average Treatment Effect)
#' plots for each model using the margot package, and optionally saves the plots.
#'
#' @param models_list A list of model results, where each element contains a 'rate_result' component.
#' @param dpi The resolution of the saved plots in dots per inch. Default is 300.
#' @param width The width of the saved plots in inches. Default is 12.
#' @param height The height of the saved plots in inches. Default is 8.
#' @param save_plots Logical indicating whether to save the plots to disk. Default is TRUE.
#' @param output_dir The directory where plots should be saved. Default is "rate_plots".
#'
#' @return A list containing the generated ggplot objects for each model.
#'
#' @import margot
#' @import ggplot2
#' @import cli
#' @importFrom crayon red green bold
#'
#' @examples
#' \dontrun{
#' # Assuming 'models_binary' is your list of model results
#' models_to_process <- list(
#'   model_t2_env_climate_chg_cause_z = models_binary$result$model_t2_env_climate_chg_cause_z,
#'   model_t2_env_climate_chg_concern_z = models_binary$result$model_t2_env_climate_chg_concern_z,
#'   model_t2_envefficacy_z = models_binary$result$model_t2_envefficacy_z,
#'   model_t2_env_climate_chg_real_z = models_binary$result$model_t2_env_climate_chg_real_z,
#'   model_t2_env_sat_nz_environment_z = models_binary$result$model_t2_env_sat_nz_environment_z
#' )
#'
#' # Run the batch processing
#' rate_plot <- margot_plot_batch_rate(models_to_process, output_dir = "path/to/your/output/directory")
#'
#' # Access individual plots
#' rate_plot$model_t2_env_climate_chg_cause_z
#' }
#'
#' @export
margot_plot_batch_rate <- function(models_binary,
                                   dpi = 300,
                                   width = 12,
                                   height = 8,
                                   save_plots = TRUE,
                                   output_dir = "rate_plots") {
  # Check if margot package is installed
  if (!requireNamespace("margot", quietly = TRUE)) {
    stop("Package 'margot' is required but not installed. Please install it first.")
  }

  # Create output directory if it doesn't exist
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }

  # Get all model names
  model_names <- names(models_binary$results)

  # Initialize list to store plots
  rate_plots <- list()

  # Loop through each model
  for (model_name in model_names) {
    tryCatch({
      # Extract rate result
      rate_result <- models_binary$results[[model_name]]$rate_result

      # Create RATE plot
      plot <- margot::margot_plot_rate(rate_result)

      # Store plot in list
      rate_plots[[model_name]] <- plot

      # Save plot if requested
      if (save_plots) {
        file_name <- file.path(output_dir, paste0(model_name, "_rate_plot.png"))
        ggplot2::ggsave(file_name, plot, dpi = dpi, width = width, height = height)
        message("Saved ", file_name)
      }
    }, error = function(e) {
      warning("Error processing model ", model_name, ": ", e$message)
    })
  }

  return(rate_plots)
}
