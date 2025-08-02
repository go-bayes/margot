#' Save Margot Plot as PNG
#'
#' This function takes the output of either `margot_plot()` or `margot_plot_multi_arm()`
#' and saves the plot as a PNG image using `ggsave()`.
#'
#' @param plot_output A list containing the output of `margot_plot()` or `margot_plot_multi_arm()`.
#'   This list should have a `plot` element that contains the ggplot object.
#' @param prefix Character string. A prefix to add to the filename. Default is NULL.
#' @param base_filename Character string. The base name for the saved file. Default is "margot_plot".
#' @param save_path Character string. The directory path where the image will be saved.
#'   Default is here::here("push_mods").
#' @param width Numeric. The width of the saved image in inches. Default is 16.
#' @param height Numeric. The height of the saved image in inches. Default is 8.
#' @param dpi Numeric. The resolution of the saved image in dots per inch. Default is 500.
#'
#' @return Invisibly returns the path of the saved file.
#'
#' @details
#' This function uses `ggsave()` to save the Margot plot as a PNG image.
#' If the save_path directory doesn't exist, it will be created.
#' The final filename will be constructed as: `{prefix}_{base_filename}.png`.
#'
#' @examples
#' \dontrun{
#' # Assuming you have already run margot_plot() or margot_plot_multi_arm()
#' plot_result <- margot_plot(your_data, your_options)
#'
#' # Save the plot as PNG
#' margot_save_png(
#'   plot_result,
#'   prefix = "study1",
#'   base_filename = "treatment_effects",
#'   save_path = here::here("output", "plots")
#' )
#' }
#'
#' @importFrom ggplot2 ggsave
#' @importFrom cli cli_alert_success cli_alert_danger
#' @export
margot_save_png <- function(plot_output,
                            prefix = NULL,
                            base_filename = "margot_plot",
                            save_path = here::here("push_mods"),
                            width = 16,
                            height = 8,
                            dpi = 500) {
  # check if the plot_output contains a plot
  if (!("plot" %in% names(plot_output) && inherits(plot_output$plot, "ggplot"))) {
    cli::cli_alert_danger("The provided plot_output does not contain a valid ggplot object.")
    return(invisible(NULL))
  }

  # create the directory if it doesn't exist
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }

  # construct the filename
  filename <- paste0(
    ifelse(!is.null(prefix), paste0(prefix, "_"), ""),
    base_filename,
    ".png"
  )

  # Construct the full file path
  file_path <- file.path(save_path, filename)

  # Save the plot
  tryCatch(
    {
      ggplot2::ggsave(
        filename = file_path,
        plot = plot_output$plot,
        width = width,
        height = height,
        dpi = dpi
      )
      cli::cli_alert_success("Plot saved successfully: {file_path}")
    },
    error = function(e) {
      cli::cli_alert_danger("Error saving plot: {e$message}")
    }
  )

  # return the file path invisibly
  invisible(file_path)
}
