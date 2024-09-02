#' Create a Boxplot with Facets using ggplot2
#'
#' This function creates a boxplot for one or more variables, faceted by a specified variable (usually time).
#' It offers various customisation options for the plot appearance and layout.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param y_vars A list of variable names to be plotted on the y-axis.
#' @param x_var The name of the variable to be used for faceting (default is "wave").
#' @param id_col Name of the column containing unique identifiers (default is "id").
#' @param title The title of the plot (optional, auto-generated if NULL).
#' @param y_label The label for the y-axis (optional).
#' @param x_label The label for the x-axis (optional, not typically used due to faceting).
#' @param data_fraction The fraction of data to use (default is 1, i.e., all data).
#' @param seed A seed for random sampling if data_fraction < 1 (optional).
#' @param show_points Logical, whether to show individual data points (default is FALSE).
#' @param point_alpha alpha value for data points if shown (default is 0.3).
#' @param point_size Size of data points if shown (default is 0.5).
#' @param save_path Path to save the plot (optional).
#' @param width Width of the saved plot in inches (default is 16).
#' @param height Height of the saved plot in inches (default is 8).
#' @param legend_position Position of the legend (default is "bottom").
#' @param y_limits Y-axis limits (optional).
#' @param facet_type Type of faceting: "grid" or "wrap" (default is "grid").
#' @param facet_scales Scales for facet panels (default is "free_x").
#' @param facet_space Space for facet panels (default is "free_x").
#' @param facet_ncol Number of columns for facet_wrap (optional).
#' @param facet_nrow Number of rows for facet_wrap (optional).
#' @param ... Additional arguments passed to geom_boxplot().
#'
#' @return A ggplot object representing the boxplot.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import cli
#' @import stringr
#'
#' @export
margot_plot_boxplot <- function(data,
                                y_vars,
                                x_var = "wave",
                                id_col = "id",
                                title = NULL,
                                y_label = NULL,
                                x_label = NULL,
                                data_fraction = 1,
                                seed = NULL,
                                show_points = FALSE,
                                point_alpha = 0.3,
                                point_size = 0.5,
                                save_path = NULL,
                                width = 16,
                                height = 8,
                                legend_position = "bottom",
                                y_limits = NULL,
                                facet_type = "grid",
                                facet_scales = "free_x",
                                facet_space = "free_x",
                                facet_ncol = NULL,
                                facet_nrow = NULL,
                                ...) {

  cli::cli_h1("Margot Plot Boxplot")

  tryCatch({
    cli::cli_alert_info("Preparing data...")
    # define color palette
    modified_okabe_ito_colors <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
                                   "#D55E00", "#CC79A7", "#000000", "#999999")

    # prepare the data
    df <- data

    # convert wave to factor
    df[[x_var]] <- as.factor(df[[x_var]])

    # sample the data if data_fraction < 1 (advise only if plotting points)
    if (data_fraction < 1) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      df <- df %>% sample_frac(data_fraction)
    }

    # ensure y_vars is a character vector
    if (!is.character(y_vars)) {
      y_vars <- as.character(y_vars)
    }

    # function to convert to title case and remove underscores
    format_label <- function(x) {
      stringr::str_to_title(gsub("_", " ", x))
    }

    # create a named vector for label formatting
    formatted_labels <- setNames(sapply(y_vars, format_label), y_vars)

    # reshape data for plotting multiple y variables
    df_long <- df %>%
      tidyr::pivot_longer(cols = all_of(y_vars),
                          names_to = "variable",
                          values_to = "value") %>%
      mutate(variable = factor(variable, levels = y_vars, labels = formatted_labels[y_vars]))

    # remove NAs and count participants who responded to at least one outcome of interest
    df_long <- df_long %>%
      filter(!is.na(value) & is.finite(value))

    # count total unique participants
    total_unique <- n_distinct(df_long[[id_col]])

    # calculate total observations
    total_obs <- nrow(df_long)

    # determine the title based on the number of waves and variables
    if (is.null(title)) {
      if (length(unique(df_long[[x_var]])) == 1) {
        title <- sprintf("Distribution of %s by %s = %s\nTotal N = %d unique participants, %d observations",
                         paste(formatted_labels, collapse = ", "),
                         x_var,
                         unique(df_long[[x_var]]),
                         total_unique, total_obs)
      } else {
        title <- sprintf("Distribution of %s by %s\nTotal N = %d unique participants, %d observations",
                         paste(formatted_labels, collapse = ", "),
                         x_var,
                         total_unique, total_obs)
      }
    }

    # determine y-axis limits
    if (is.null(y_limits)) {
      y_min <- min(df_long$value, na.rm = TRUE)
      y_max <- max(df_long$value, na.rm = TRUE)
      if (y_min >= 1 && y_max <= 7) {
        y_limits <- c(1, 7)
      } else {
        y_limits <- c(y_min, y_max)
      }
    }

    # determine the number of unique categories (waves or variables)
    n_categories <- ifelse(length(y_vars) > 1, length(y_vars), length(unique(df_long[[x_var]])))

    # explicitly recycle colors
    recycled_colors <- rep_len(modified_okabe_ito_colors, length.out = n_categories)

    cli::cli_alert_success("Data prepared successfully")

    cli::cli_alert_info("Creating plot...")

    # determine if we need to use faceting
    multiple_y_vars <- length(y_vars) > 1
    multiple_waves <- length(unique(df_long[[x_var]])) > 1

    # create the ggplot
    if (multiple_y_vars) {
      p <- ggplot(df_long, aes(x = variable, y = value, fill = variable))
    } else {
      p <- ggplot(df_long, aes(x = .data[[x_var]], y = value, fill = .data[[x_var]]))
    }

    p <- p +
      geom_boxplot(...) +
      theme_minimal() +
      scale_fill_manual(values = recycled_colors, labels = format_label) +
      theme(
        legend.position = legend_position,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text.x = element_text(size = 10),
        panel.spacing = unit(0.2, "lines")
      ) +
      labs(
        title = title,
        y = y_label %||% "Value",
        x = x_label %||% format_label(x_var),
        fill = format_label(ifelse(multiple_y_vars, "Variable", x_var))
      ) +
      scale_y_continuous(limits = y_limits)

    # add faceting based on user choice if necessary
    if (multiple_y_vars && multiple_waves) {
      if (facet_type == "grid") {
        p <- p + facet_grid(. ~ .data[[x_var]], scales = facet_scales, space = facet_space,
                            labeller = labeller(.cols = as_labeller(format_label)))
      } else if (facet_type == "wrap") {
        p <- p + facet_wrap(~ .data[[x_var]], scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow,
                            labeller = as_labeller(format_label))
      }
    } else if (!multiple_y_vars && !multiple_waves) {
      # For single variable and single wave, remove x-axis labels
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }

    # add points if requested
    if (show_points) {
      if (multiple_y_vars) {
        p <- p + geom_jitter(aes(color = variable), width = 0.2, alpha = point_alpha, size = point_size)
      } else {
        p <- p + geom_jitter(aes(color = .data[[x_var]]), width = 0.2, alpha = point_alpha, size = point_size)
      }
      p <- p + scale_color_manual(values = recycled_colors, labels = format_label)
    }

    cli::cli_alert_success("Plot created")

    # save plot if a save path is provided
    if (!is.null(save_path)) {
      filename <- paste0(
        "boxplot_",
        paste(y_vars, collapse = "_"),
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
        dpi = 300
      )

      margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

      cli::cli_alert_success("Plot saved successfully")
    } else {
      cli::cli_alert_info("No save path provided. Plot not saved.")
    }

    cli::cli_alert_success("Margot plot boxplot created successfully \U0001F44D")

    return(p)
  }, error = function(e) {
    cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
    print(e)
    return(NULL)
  })
}
# margot_plot_boxplot <- function(data,
#                                 y_vars,
#                                 x_var = "wave",
#                                 id_col = "id",
#                                 title = NULL,
#                                 y_label = NULL,
#                                 x_label = NULL,
#                                 data_fraction = 1,
#                                 seed = NULL,
#                                 show_points = FALSE,
#                                 point_alpha = 0.3,
#                                 point_size = 0.5,
#                                 save_path = NULL,
#                                 width = 16,
#                                 height = 8,
#                                 legend_position = "bottom",
#                                 y_limits = NULL,
#                                 facet_type = "grid",
#                                 facet_scales = "free_x",
#                                 facet_space = "free_x",
#                                 facet_ncol = NULL,
#                                 facet_nrow = NULL,
#                                 ...) {
#
#   cli::cli_h1("Margot Plot Boxplot")
#
#   tryCatch({
#     cli::cli_alert_info("Preparing data...")
#     # define color palette
#     modified_okabe_ito_colors <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
#                                    "#D55E00", "#CC79A7", "#000000", "#999999")
#
#     # prepare the data
#     df <- data
#
#     # Convert wave to factor
#     df[[x_var]] <- as.factor(df[[x_var]])
#
#     # Sample the data if data_fraction < 1
#     if (data_fraction < 1) {
#       if (!is.null(seed)) {
#         set.seed(seed)
#       }
#       df <- df %>% sample_frac(data_fraction)
#     }
#
#     # Ensure y_vars is a character vector
#     if (!is.character(y_vars)) {
#       y_vars <- as.character(y_vars)
#     }
#
#     # Reshape data for plotting multiple y variables
#     df_long <- df %>%
#       tidyr::pivot_longer(cols = all_of(y_vars),
#                           names_to = "variable",
#                           values_to = "value")
#
#     # Remove NAs and count participants who responded to at least one outcome of interest
#     df_long <- df_long %>%
#       filter(!is.na(value) & is.finite(value))
#
#     # Count total unique participants
#     total_unique <- n_distinct(df_long[[id_col]])
#
#     # Calculate total observations
#     total_obs <- nrow(df_long)
#
#     # Function to convert to title case and remove underscores
#     format_label <- function(x) {
#       stringr::str_to_title(gsub("_", " ", x))
#     }
#
#     # Determine the title based on the number of waves and variables
#     if (is.null(title)) {
#       if (length(unique(df_long[[x_var]])) == 1) {
#         title <- sprintf("Distribution of %s by %s = %s\nTotal N = %d unique participants, %d observations",
#                          paste(format_label(y_vars), collapse = ", "),
#                          x_var,
#                          unique(df_long[[x_var]]),
#                          total_unique, total_obs)
#       } else {
#         title <- sprintf("Distribution of %s by %s\nTotal N = %d unique participants, %d observations",
#                          paste(format_label(y_vars), collapse = ", "),
#                          x_var,
#                          total_unique, total_obs)
#       }
#     }
#
#     # Determine y-axis limits
#     if (is.null(y_limits)) {
#       y_min <- min(df_long$value, na.rm = TRUE)
#       y_max <- max(df_long$value, na.rm = TRUE)
#       if (y_min >= 1 && y_max <= 7) {
#         y_limits <- c(1, 7)
#       } else {
#         y_limits <- c(y_min, y_max)
#       }
#     }
#
#     # Determine the number of unique categories (waves or variables)
#     n_categories <- ifelse(length(y_vars) > 1, length(y_vars), length(unique(df_long[[x_var]])))
#
#     # Explicitly recycle colors
#     recycled_colors <- rep_len(modified_okabe_ito_colors, length.out = n_categories)
#
#     cli::cli_alert_success("Data prepared successfully")
#
#     cli::cli_alert_info("Creating plot...")
#
#     # Determine if we need to use faceting
#     multiple_y_vars <- length(y_vars) > 1
#     multiple_waves <- length(unique(df_long[[x_var]])) > 1
#
#     # create the ggplot
#     if (multiple_y_vars) {
#       p <- ggplot(df_long, aes(x = variable, y = value, fill = variable))
#     } else {
#       p <- ggplot(df_long, aes(x = .data[[x_var]], y = value, fill = .data[[x_var]]))
#     }
#
#     p <- p +
#       geom_boxplot(...) +
#       theme_minimal() +
#       scale_fill_manual(values = recycled_colors) +
#       theme(
#         legend.position = legend_position,
#         legend.text = element_text(size = 10),
#         legend.title = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         strip.text.x = element_text(size = 10),
#         panel.spacing = unit(0.2, "lines")
#       ) +
#       labs(
#         title = title,
#         y = y_label %||% "Value",
#         x = x_label %||% x_var
#       ) +
#       scale_y_continuous(limits = y_limits)
#
#     # Add faceting based on user choice if necessary
#     if (multiple_y_vars && multiple_waves) {
#       if (facet_type == "grid") {
#         p <- p + facet_grid(. ~ .data[[x_var]], scales = facet_scales, space = facet_space,
#                             labeller = labeller(.cols = as_labeller(format_label)))
#       } else if (facet_type == "wrap") {
#         p <- p + facet_wrap(~ .data[[x_var]], scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow,
#                             labeller = as_labeller(format_label))
#       }
#     } else if (!multiple_y_vars && !multiple_waves) {
#       # For single variable and single wave, remove x-axis labels
#       p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
#     }
#
#     # Add points if requested
#     if (show_points) {
#       if (multiple_y_vars) {
#         p <- p + geom_jitter(aes(color = variable), width = 0.2, alpha = point_alpha, size = point_size)
#       } else {
#         p <- p + geom_jitter(aes(color = .data[[x_var]]), width = 0.2, alpha = point_alpha, size = point_size)
#       }
#       p <- p + scale_color_manual(values = recycled_colors)
#     }
#
#     cli::cli_alert_success("Plot created")
#
#     # save plot if a save path is provided
#     if (!is.null(save_path)) {
#       filename <- paste0(
#         "boxplot_",
#         paste(y_vars, collapse = "_"),
#         "_",
#         format(Sys.Date(), "%Y%m%d")
#       )
#
#       cli::cli_alert_info("Saving plot...")
#
#       ggsave(
#         plot = p,
#         filename = file.path(save_path, paste0(filename, ".png")),
#         width = width,
#         height = height,
#         units = "in",
#         dpi = 300
#       )
#
#       margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
#
#       cli::cli_alert_success("Plot saved successfully")
#     } else {
#       cli::cli_alert_info("No save path provided. Plot not saved.")
#     }
#
#     cli::cli_alert_success("Margot plot boxplot created successfully \U0001F44D")
#
#     return(p)
#   }, error = function(e) {
#     cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
#     print(e)
#     return(NULL)
#   })
# }
