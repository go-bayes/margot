#' Save Plot as PNG
#'
#' This function saves a plot object as a PNG image, supporting multiple plotting systems
#' including ggplot2, base R plots, and other plotting libraries.
#'
#' @param plot_output Either a plot object directly, or a list containing a plot element.
#'   Supported plot types include ggplot objects, base R plot objects, and others.
#' @param prefix Character string. A prefix to add to the filename. Default is NULL.
#' @param base_filename Character string. The base name for the saved file. Default is "plot".
#' @param save_path Character string. The directory path where the image will be saved.
#'   Default is here::here("push_mods").
#' @param width Numeric. The width of the saved image in inches. Default is 16.
#' @param height Numeric. The height of the saved image in inches. Default is 8.
#' @param dpi Numeric. The resolution of the saved image in dots per inch. Default is 500.
#'
#' @return Invisibly returns the path of the saved file.
#'
#' @details
#' This function handles different types of plot objects and saves them as PNG images:
#' - For ggplot2 objects: Uses ggsave()
#' - For other plot types: Uses png() and dev.off()
#' The final filename will be constructed as: `{prefix}_{base_filename}.png`.
#'
#' @examples
#' \dontrun{
#' # For a ggplot object
#' plot_result <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#' margot_save_png(plot_result, prefix = "study1", base_filename = "scatter")
#'
#' # For a base R plot
#' plot_result <- plot(mtcars$mpg, mtcars$wt)
#' margot_save_png(plot_result, prefix = "study1", base_filename = "scatter")
#'
#' # For a list containing a plot
#' plot_list <- list(plot = ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point())
#' margot_save_png(plot_list, prefix = "study1", base_filename = "scatter")
#' }
#'
#' @importFrom ggplot2 ggsave
#' @importFrom cli cli_alert_success cli_alert_danger
#' @export
margot_save_png <- function(plot_output,
                            prefix = NULL,
                            base_filename = "plot",
                            save_path = here::here("push_mods"),
                            width = 16,
                            height = 8,
                            dpi = 500) {

  # Extract plot object if it's in a list
  if (is.list(plot_output) && "plot" %in% names(plot_output)) {
    plot_obj <- plot_output$plot
  } else {
    plot_obj <- plot_output
  }

  # Create the directory if it doesn't exist
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }

  # Construct the filename
  filename <- paste0(
    ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
    base_filename,
    ".png"
  )

  # Construct the full file path
  file_path <- file.path(save_path, filename)

  # Save the plot based on its type
  tryCatch({
    if (inherits(plot_obj, "ggplot")) {
      # Save ggplot object using ggsave
      ggplot2::ggsave(
        filename = file_path,
        plot = plot_obj,
        width = width,
        height = height,
        dpi = dpi
      )
    } else {
      # For other plot types, use png device
      png(
        filename = file_path,
        width = width,
        height = height,
        units = "in",
        res = dpi
      )

      # If it's a recordedplot object, replot it
      if (inherits(plot_obj, "recordedplot")) {
        replayPlot(plot_obj)
      } else {
        # For other objects that might have a plot method
        plot(plot_obj)
      }

      dev.off()
    }

    cli::cli_alert_success("Plot saved successfully: {file_path}")

  }, error = function(e) {
    cli::cli_alert_danger("Error saving plot: {e$message}")
  })

  # Return the file path invisibly
  invisible(file_path)
}
