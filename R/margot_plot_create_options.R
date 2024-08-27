#' Create Plot Options for Margot Plot
#'
#' This function creates a list of options for use with the `margot_plot` function.
#' It allows for easy customization of plot settings while maintaining a set of default values.
#'
#' @param subtitle Character string. The subtitle for the plot.
#' @param base_defaults List. The base default options to use. If not provided, uses a pre-defined set of defaults.
#' @param title Character string. The title for the plot. If NULL, uses the title from base_defaults.
#' @param filename_prefix Character string. Prefix for the filename. If NULL, uses a default prefix.
#' @param label_mapping Named list. Mapping of original outcome labels to new labels.
#' @param ... Additional named arguments to override or add to the default options.
#'
#' @return A list of plot options to be used with `margot_plot`.
#'
#' @details
#' The function allows customization of various plot parameters, including color schemes,
#' text sizes, and label transformations. The `label_mapping` parameter enables custom
#' renaming of specific outcomes without affecting the default transformations for other labels.
#'
#' @import cli
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' health_options <- margot_plot_create_options("Health Outcomes")
#'
#' # Custom title and filename prefix
#' education_options <- margot_plot_create_options("Education Outcomes",
#'                                                 title = "Custom Title",
#'                                                 filename_prefix = "edu_outcomes")
#'
#' # Using label_mapping for custom outcome labels
#' trust_science_options <- margot_plot_create_options(
#'   subtitle = "Trust in Science Outcomes",
#'   title = "Science Trust Analysis",
#'   filename_prefix = "science_trust",
#'   label_mapping = list(
#'     "t2_trust_science_our_society_places_too_much_emphasis_reversed_z" = "Science Overemphasis"
#'   ),
#'   colors = c(
#'     "positive" = "#4CAF50",
#'     "not reliable" = "#FFC107",
#'     "negative" = "#F44336"
#'   ),
#'   base_size = 16,
#'   point_size = 4
#' )
#'
#' # Use the created options in margot_plot
#' result <- margot_plot(your_data, options = trust_science_options)
#' }
#'
#' @export
margot_plot_create_options <- function(subtitle,
                                       base_defaults = NULL,
                                       title = NULL,
                                       filename_prefix = NULL,
                                       label_mapping = NULL,
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
      linewidth = 0.4,
      estimate_scale = 1,
      base_size = 18,
      point_size = 5,
      title_size = 20,
      subtitle_size = 18,
      legend_text_size = 10,
      legend_title_size = 10,
      remove_tx_prefix = TRUE,
      remove_z_suffix = TRUE,
      use_title_case = TRUE,
      remove_underscores = TRUE,
      save_plot = FALSE,
      save_plot_options = list(
        width = 12,
        height = 8,
        dpi = 300
      ),
      push_mods = NULL,
      label_mapping = NULL  # Add default for label_mapping
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

  if (!is.null(label_mapping)) {
    new_options$label_mapping <- label_mapping
    cli::cli_alert_info("Label mapping added for {length(label_mapping)} outcome(s)")
  }

  final_options <- modifyList(base_defaults, modifyList(new_options, list(...)))

  # Validate label_mapping
  if (!is.null(final_options$label_mapping)) {
    if (!is.list(final_options$label_mapping) || is.null(names(final_options$label_mapping))) {
      cli::cli_alert_warning("Invalid label_mapping format. It should be a named list.")
    } else {
      cli::cli_alert_success("Label mapping validated successfully")
    }
  }

  cli::cli_alert_success("Plot options created successfully for '{subtitle}' \U0001F44D")
  return(final_options)
}
# margot_plot_create_options <- function(subtitle,
#                                        base_defaults = NULL,
#                                        title = NULL,
#                                        filename_prefix = NULL,
#                                        ...) {
#   if (is.null(base_defaults)) {
#     base_defaults <- list(
#       type = "RD",
#       title = "Default Title",
#       colors = c(
#         "positive" = "#E69F00",
#         "not reliable" = "grey50",
#         "negative" = "#56B4E9"
#       ),
#       text_size = 5,
#       linewidth = 0.4,
#       estimate_scale = 1,
#       base_size = 18,
#       point_size = 5,
#       title_size = 20,
#       subtitle_size = 18,
#       legend_text_size = 10,
#       legend_title_size = 10,
#       remove_tx_prefix = TRUE,
#       remove_z_suffix = TRUE,
#       use_title_case = TRUE,
#       remove_underscores = TRUE,
#       save_plot = FALSE,
#       save_plot_options = list(
#         width = 12,
#         height = 8,
#         dpi = 300
#       ),
#       push_mods = NULL
#     )
#   }
#
#   if (is.null(filename_prefix)) {
#     filename_prefix <- "margot_plot"
#   }
#
#   new_options <- list(
#     subtitle = subtitle,
#     save_plot_options = modifyList(
#       base_defaults$save_plot_options,
#       list(filename = paste0(filename_prefix, "_", tolower(gsub(" ", "_", subtitle)), ".png"))
#     )
#   )
#
#   if (!is.null(title)) {
#     new_options$title <- title
#   }
#
#   final_options <- modifyList(base_defaults, modifyList(new_options, list(...)))
#
#   cli::cli_alert_success("Plot options created successfully for '{subtitle}' \U0001F44D")
#
#   return(final_options)
# }
