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
#' @param id_col Name of the column containing unique identifiers (default is "id").
#' @param title An optional title for the plot. If NULL, an automatic title will be generated.
#' @param y_label An optional label for the y-axis. If NULL, the response variable name will be used.
#' @param x_label An optional label for the x-axis. If NULL, the first term will be used.
#' @param y_limits An optional vector of two numbers specifying the y-axis limits. Default is c(1, 7).
#' @param color_label An optional label for the color legend. If NULL, the second term will be used.
#' @param include_title Logical, whether to include the plot title (default is TRUE).
#' @param include_timestamp Logical, whether to include timestamp in plot title and filename (default is FALSE).
#' @param save_path An optional path to save the plot. If NULL, the plot will not be saved.
#' @param prefix Optional prefix for the saved file name (default is NULL).
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
#' @examples
#' \dontrun{
#' # Example usage remains the same
#' }
#' @export
margot_plot_slope_covariate <- function(data,
                                        formula,
                                        terms,
                                        id_col = "id",
                                        title = NULL,
                                        y_label = NULL,
                                        x_label = NULL,
                                        y_limits = c(1, 7),
                                        color_label = NULL,
                                        include_title = TRUE,
                                        include_timestamp = FALSE,
                                        save_path = NULL,
                                        prefix = NULL,
                                        width = 12,
                                        height = 8,
                                        seed = NULL,
                                        ...) {

  cli::cli_h1("Margot Plot ggeffects")

  p <- NULL
  total_unique <- NULL
  total_obs <- NULL
  filtered_data <- NULL

  tryCatch({
    cli::cli_alert_info("Preparing model and calculating predicted responses...")

    if (!is.null(seed)) {
      set.seed(seed)
    }

    # Filter the data for the outcome variable
    outcome_var <- all.vars(formula[[2]])
    filtered_data <- data %>%
      dplyr::filter(!is.na(!!sym(outcome_var)) & is.finite(!!sym(outcome_var)))

    total_unique <- dplyr::n_distinct(filtered_data[[id_col]])
    total_obs <- nrow(filtered_data)

    model <- lm(formula, data = filtered_data)

    # Prepare labels for ggeffects::predict_response
    pred_labels <- list()
    if (!is.null(y_label)) pred_labels$response <- y_label
    if (!is.null(x_label)) pred_labels[[terms[1]]] <- x_label
    if (!is.null(color_label)) pred_labels[[terms[2]]] <- color_label

    # Calculate predicted responses with labels
    pred <- ggeffects::predict_response(model, terms = terms, labels = pred_labels, ...)

    cli::cli_alert_success("Predicted responses calculated")

    cli::cli_alert_info("Creating plot...")

    # Function to convert to title case and remove underscores
    format_label <- function(x) {
      stringr::str_to_title(gsub("_", " ", x))
    }

    # Determine the title
    if (include_title) {
      if (is.null(title)) {
        outcome_label <- y_label %||% format_label(outcome_var)
        covariate_label <- color_label %||% format_label(terms[2])
        title <- sprintf(
          "%s by %s and %s\nTotal N = %d unique participants, %d observations",
          outcome_label, x_label %||% format_label(terms[1]), covariate_label, total_unique, total_obs
        )
      }

      if (include_timestamp) {
        title <- paste(title, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      }
    } else {
      title <- NULL
    }

    # Create the ggplot
    p <- plot(pred) +
      ggokabeito::scale_colour_okabe_ito() +
      theme_classic() +
      scale_y_continuous(limits = y_limits) +
      labs(
        title = title,
        y = y_label %||% format_label(outcome_var),
        x = x_label %||% format_label(terms[1]),
        color = color_label %||% format_label(terms[2])
      )

    cli::cli_alert_success("Plot created successfully")

    # Save plot if a save path is provided
    if (!is.null(save_path)) {
      filename <- "ggeffects_plot"
      if (!is.null(prefix) && nzchar(prefix)) {
        filename <- paste0(prefix, "_", filename)
      }
      filename <- paste0(
        filename, "_",
        outcome_var,
        "_by_",
        paste(terms, collapse = "_")
      )
      if (include_timestamp) {
        filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      }
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

    return(list(plot = p, total_unique = total_unique, total_obs = total_obs, ids = filtered_data[[id_col]]))

  }, error = function(e) {
    cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
    print(e)
    return(NULL)
  }, warning = function(w) {
    cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
    print(w)
  })
}
# margot_plot_slope_covariate <- function(data,
#                                         formula,
#                                         terms,
#                                         id_col = "id",
#                                         title = NULL,
#                                         y_label = NULL,
#                                         x_label = NULL,
#                                         y_limits = c(1, 7),
#                                         color_label = NULL,
#                                         include_timestamp = FALSE,
#                                         save_path = NULL,
#                                         prefix = NULL,
#                                         width = 12,
#                                         height = 8,
#                                         seed = NULL,
#                                         ...) {
#
#   cli::cli_h1("Margot Plot ggeffects")
#
#   p <- NULL
#
#   tryCatch({
#     cli::cli_alert_info("Preparing model and calculating predicted responses...")
#
#     if (!is.null(seed)) {
#       set.seed(seed)
#     }
#
#     data <- data %>%
#       dplyr::filter(!is.na(!!sym(all.vars(formula[[2]]))) & is.finite(!!sym(all.vars(formula[[2]]))))
#
#     total_unique <- dplyr::n_distinct(data[[id_col]])
#     total_obs <- nrow(data)
#
#     model <- lm(formula, data = data)
#
#     # Prepare labels for ggeffects::predict_response
#     pred_labels <- list()
#     if (!is.null(y_label)) pred_labels$response <- y_label
#     if (!is.null(x_label)) pred_labels[[terms[1]]] <- x_label
#     if (!is.null(color_label)) pred_labels[[terms[2]]] <- color_label
#
#     # Calculate predicted responses with labels
#     pred <- ggeffects::predict_response(model, terms = terms, labels = pred_labels, ...)
#
#     cli::cli_alert_success("Predicted responses calculated")
#
#     cli::cli_alert_info("Creating plot...")
#
#     # Function to convert to title case and remove underscores
#     format_label <- function(x) {
#       stringr::str_to_title(gsub("_", " ", x))
#     }
#
#     # Determine the title
#     if (is.null(title)) {
#       outcome_label <- y_label %||% format_label(all.vars(formula[[2]]))
#       covariate_label <- color_label %||% format_label(terms[2])
#       title <- sprintf(
#         "%s by %s and %s\nTotal N = %d unique participants, %d observations",
#         outcome_label, x_label %||% format_label(terms[1]), covariate_label, total_unique, total_obs
#       )
#     }
#
#     if (include_timestamp) {
#       title <- paste(title, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
#     }
#
#     # Create the ggplot
#     p <- plot(pred) +
#       ggokabeito::scale_colour_okabe_ito() +
#       theme_classic() +
#       scale_y_continuous(limits = y_limits) +
#       labs(
#         title = title,
#         y = y_label %||% format_label(all.vars(formula[[2]])),
#         x = x_label %||% format_label(terms[1]),
#         color = color_label %||% format_label(terms[2])
#       )
#
#     cli::cli_alert_success("Plot created successfully")
#
#     # Save plot if a save path is provided
#     if (!is.null(save_path)) {
#       filename <- "ggeffects_plot"
#       if (!is.null(prefix) && nzchar(prefix)) {
#         filename <- paste0(prefix, "_", filename)
#       }
#       filename <- paste0(
#         filename, "_",
#         all.vars(formula[[2]]),
#         "_by_",
#         paste(terms, collapse = "_")
#       )
#       if (include_timestamp) {
#         filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
#       }
#       cli::cli_alert_info("Saving plot...")
#       ggsave(
#         plot = p,
#         filename = file.path(save_path, paste0(filename, ".png")),
#         width = width,
#         height = height,
#         units = "in",
#         device = 'png',
#         dpi = 400
#       )
#       margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
#       cli::cli_alert_success("Plot saved successfully")
#     } else {
#       cli::cli_alert_info("No save path provided. Plot not saved.")
#     }
#
#     cli::cli_alert_success("Margot plot ggeffects created successfully \U0001F44D")
#
#     return(p)
#
#   }, error = function(e) {
#     cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
#     print(e)
#     return(NULL)
#   }, warning = function(w) {
#     cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
#     print(w)
#   })
# }

# margot_plot_slope_covariate <- function(data,
#                                         formula,
#                                         terms,
#                                         id_col = "id",
#                                         title = NULL,
#                                         y_label = NULL,
#                                         x_label = NULL,
#                                         y_limits = c(1, 7),
#                                         color_label = NULL,
#                                         include_timestamp = FALSE,
#                                         save_path = NULL,
#                                         prefix = NULL,
#                                         width = 12,
#                                         height = 8,
#                                         seed = NULL,
#                                         ...) {
#
#   cli::cli_h1("Margot Plot ggeffects")
#
#   p <- NULL
#
#   tryCatch({
#     cli::cli_alert_info("Preparing model and calculating predicted responses...")
#
#     if (!is.null(seed)) {
#       set.seed(seed)
#     }
#
#     data <- data %>%
#       dplyr::filter(!is.na(!!sym(all.vars(formula[[2]]))) & is.finite(!!sym(all.vars(formula[[2]]))))
#
#     total_unique <- dplyr::n_distinct(data[[id_col]])
#     total_obs <- nrow(data)
#
#     model <- lm(formula, data = data)
#
#     # Prepare labels for ggeffects::predict_response
#     pred_labels <- list()
#     if (!is.null(y_label)) pred_labels$response <- y_label
#     if (!is.null(x_label)) pred_labels[[terms[1]]] <- x_label
#     if (!is.null(color_label)) pred_labels[[terms[2]]] <- color_label
#
#     # Calculate predicted responses with labels
#     pred <- ggeffects::predict_response(model, terms = terms, labels = pred_labels, ...)
#
#     cli::cli_alert_success("Predicted responses calculated")
#
#     cli::cli_alert_info("Creating plot...")
#
#     # Function to convert to title case and remove underscores
#     format_label <- function(x) {
#       stringr::str_to_title(gsub("_", " ", x))
#     }
#
#     # Determine the title
#     if (is.null(title)) {
#       outcome_label <- y_label %||% format_label(all.vars(formula[[2]]))
#       covariate_label <- color_label %||% format_label(terms[2])
#       title <- sprintf(
#         "%s by %s and %s\nTotal N = %d unique participants, %d observations",
#         outcome_label, x_label %||% format_label(terms[1]), covariate_label, total_unique, total_obs
#       )
#     }
#
#     if (include_timestamp) {
#       title <- paste(title, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
#     }
#
#     # Create the ggplot
#     p <- plot(pred) +
#       ggokabeito::scale_colour_okabe_ito() +
#       theme_classic() +
#       scale_y_continuous(limits = y_limits) +
#       labs(title = title)
#
#     cli::cli_alert_success("Plot created successfully")
#
#     # Save plot if a save path is provided
#     if (!is.null(save_path)) {
#       filename <- "ggeffects_plot"
#       if (!is.null(prefix) && nzchar(prefix)) {
#         filename <- paste0(prefix, "_", filename)
#       }
#       filename <- paste0(
#         filename, "_",
#         all.vars(formula[[2]]),
#         "_by_",
#         paste(terms, collapse = "_")
#       )
#       if (include_timestamp) {
#         filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
#       }
#       cli::cli_alert_info("Saving plot...")
#       ggsave(
#         plot = p,
#         filename = file.path(save_path, paste0(filename, ".png")),
#         width = width,
#         height = height,
#         units = "in",
#         device = 'png',
#         dpi = 400
#       )
#       margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
#       cli::cli_alert_success("Plot saved successfully")
#     } else {
#       cli::cli_alert_info("No save path provided. Plot not saved.")
#     }
#
#     cli::cli_alert_success("Margot plot ggeffects created successfully \U0001F44D")
#
#     return(p)
#
#   }, error = function(e) {
#     cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
#     print(e)
#     return(NULL)
#   }, warning = function(w) {
#     cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
#     print(w)
#   })
# }
#
#
#
