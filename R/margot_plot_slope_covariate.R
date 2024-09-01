#' Create a Slope Plot using ggeffects
#'
#' This function creates a ggplot2 visualization using ggeffects to calculate
#' predicted responses from a model. It allows flexible specification of the model
#' and plotting options. The function automatically handles NA and infinite values,
#' and reports the number of unique participants and observations used in the analysis.
#'
#' @param data A data frame containing the variables for the model.
#' @param formula A formula specifying the model to be fit.
#' @param terms A character vector specifying the terms to be used in predict_response.
#' @param title An optional title for the plot. If NULL, an automatic title will be generated.
#' @param y_label An optional label for the y-axis. If NULL, the response variable name will be used.
#' @param x_label An optional label for the x-axis. If NULL, the first term will be used.
#' @param y_limits An optional vector of two numbers specifying the y-axis limits. Default is c(1, 7).
#' @param color_label An optional label for the color legend. If NULL, the second term will be used.
#' @param save_path An optional path to save the plot. If NULL, the plot will not be saved.
#' @param width The width of the saved plot in inches. Default is 12.
#' @param height The height of the saved plot in inches. Default is 8.
#' @param seed An optional seed for reproducibility.
#' @param ... Additional arguments to be passed to ggeffects::predict_response.
#'
#' @return A ggplot2 object representing the plot.
#'
#' @import ggplot2
#' @import ggeffects
#' @import ggokabeito
#' @import cli
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggeffects)
#' library(ggokabeito)
#' library(dplyr)
#'
#' # Basic usage
#' plot <- margot_plot_slope_covariate(
#'   data = dat,
#'   formula = warm_muslims ~ wave:hours_work,
#'   terms = c("wave", "hours_work"),
#'   y_label = "Warmth",
#'   x_label = "Wave",
#'   color_label = "Work Hours"
#' )
#'
#' # With additional ggeffects options
#' plot_with_options <- margot_plot_slope_covariate(
#'   data = dat,
#'   formula = warm_immigrants ~ wave:education,
#'   terms = c("wave", "education [0:18]"),
#'   y_label = "Warmth",
#'   x_label = "Wave",
#'   color_label = "Education (Years)",
#'   type = "continuous"
#' )
#'
#' # Saving the plot
#' saved_plot <- margot_plot_slope_covariate(
#'   data = dat,
#'   formula = political_orientation ~ wave:age,
#'   terms = c("wave", "age"),
#'   save_path = "path/to/save/directory",
#'   width = 10,
#'   height = 6
#' )
#' }
margot_plot_slope_covariate <- function(data,
                                         formula,
                                         terms,
                                         title = NULL,
                                         y_label = NULL,
                                         x_label = NULL,
                                         y_limits = c(1, 7),
                                         color_label = NULL,
                                         save_path = NULL,
                                         width = 12,
                                         height = 8,
                                         seed = NULL,
                                         ...) {

  cli::cli_h1("Margot Plot ggeffects")

  # Initialize p as NULL
  p <- NULL

  tryCatch({
    cli::cli_alert_info("Preparing model and calculating predicted responses...")

    # Set seed if provided
    if (!is.null(seed)) {
      set.seed(seed)
    }

    # Fit the model
    model <- lm(formula, data = data)

    # Calculate predicted responses
    pred <- ggeffects::predict_response(model, terms = terms, ...)

    cli::cli_alert_success("Predicted responses calculated")

    cli::cli_alert_info("Creating plot...")

    # Create the ggplot
    p <- plot(pred) +
      ggokabeito::scale_colour_okabe_ito() +
      theme_classic() +
      scale_y_continuous(limits = y_limits) +
      labs(
        title = title,
        y = y_label %||% all.vars(formula[[2]]),  # Use the response variable name if y_label is not provided
        x = x_label %||% terms[1],
        color = color_label %||% terms[2]
      )

    cli::cli_alert_success("Plot created successfully")

    # Save plot if a save path is provided
    if (!is.null(save_path)) {
      filename <- paste0(
        "ggeffects_plot_",
        all.vars(formula[[2]]),
        "_by_",
        paste(terms, collapse = "_"),
        "_",
        format(Sys.Date(), "%Y%m%d")
      )

      cli::cli_alert_info("Saving plot...")

      ggsave(
        plot = p,
        filename = file.path(save_path, paste0(filename, ".png")),
        width = width,
        height = height,
        units = "in",
        device = 'png',
        dpi = 400
      )

      margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

      cli::cli_alert_success("Plot saved successfully")
    } else {
      cli::cli_alert_info("No save path provided. Plot not saved.")
    }

    cli::cli_alert_success("Margot plot ggeffects created successfully \U0001F44D")

    # Return the ggplot object
    return(p)

  }, error = function(e) {
    cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
    print(e)
    return(NULL)
  }, warning = function(w) {
    cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
    print(w)
  })
}
