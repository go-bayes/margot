#' Create Plot Options for Margot Plot
#'
#' This utility function creates a list of options for use with the `margot_plot` function.
#' It allows for easy customisation of plot settings while maintaining a set of default values.
#'
#' @param subtitle Character string. The subtitle for the plot.
#' @param base_defaults List. The base default options to use. If not provided, uses a pre-defined set of defaults.
#' @param title Character string. The title for the plot. If NULL, uses the title from base_defaults.
#' @param filename_prefix Character string. Prefix for the filename. If NULL, uses a default prefix.
#' @param ... Additional named arguments to override or add to the default options.
#'
#' @return A list of plot options to be used with `margot_plot`.
#'
#' @import cli
#'
#' @examples
#' \dontrun{
#' health_options <- create_plot_options("Health Outcomes")
#' education_options <- create_plot_options("Education Outcomes",
#'                                         title = "Custom Title",
#'                                         filename_prefix = "edu_outcomes")
#' }
#'
#' @export
create_plot_options <- function(subtitle,
                                base_defaults = NULL,
                                title = NULL,
                                filename_prefix = NULL,
                                ...) {
  if (is.null(base_defaults)) {
    base_defaults <- list(
      type = "RD",
      title = "Default Title",
      colors = c(
        "positive" = "#E69F00",
        "not reliable" = "grey50",
        "negative" = "#56B4E9"
      ),
      text_size = 5,
      point_size = 5,
      base_size = 14,
      linewidth = 0.25,
      remove_tx_prefix = TRUE,
      remove_z_suffix = TRUE,
      use_title_case = TRUE,
      remove_underscores = TRUE,
      save_plot = TRUE,
      save_plot_options = list(
        width = 10,
        height = 6,
        dpi = 300
      ),
      push_mods = NULL
    )
  }

  if (is.null(filename_prefix)) {
    filename_prefix <- "margot_plot"
  }

  new_options <- list(
    subtitle = subtitle,
    save_plot_options = modifyList(
      base_defaults$save_plot_options,
      list(filename = paste0(filename_prefix, "_", tolower(gsub(" ", "_", subtitle)), ".png"))
    )
  )

  if (!is.null(title)) {
    new_options$title <- title
  }

  final_options <- modifyList(base_defaults, modifyList(new_options, list(...)))

  cli::cli_alert_success("Plot options created successfully for '{subtitle}' \U0001F44D")

  return(final_options)
}
