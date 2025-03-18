#' Create panel data Boxplots using ggplot2
#'
#' This function creates boxplots for one or more variables across specified panel waves.
#' It offers various customisation options for the plot appearance and layout.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param y_vars A list of variable names to be plotted on the y-axis.
#' @param waves A vector of wave values to include in the plot (default is NULL, which includes all waves).
#' @param id_col Name of the column containing unique identifiers (default is "id").
#' @param title The title of the plot (optional, auto-generated if NULL).
#' @param y_label The label for the y-axis (optional).
#' @param x_label The label for the x-axis (optional, defaults to "Wave").
#' @param show_points Logical, whether to show individual data points (default is FALSE).
#' @param point_alpha Alpha value for data points if shown (default is 0.3).
#' @param point_size Size of data points if shown (default is 0.5).
#' @param include_timestamp Logical, whether to include timestamp in plot title and filename (default is FALSE).
#' @param save_path Path to save the plot (optional).
#' @param prefix Optional prefix for the saved file name (default is NULL).
#' @param width Width of the saved plot in inches (default is 16).
#' @param height Height of the saved plot in inches (default is 8).
#' @param legend_position Position of the legend (default is "bottom").
#' @param y_limits Y-axis limits (optional).
#' @param facet_scales Scales for facet panels (default is "free_y").
#' @param facet_ncol Number of columns for facet_wrap (optional).
#' @param facet_nrow Number of rows for facet_wrap (optional).
#' @param coord_flip Logical, whether to flip the coordinates of the plot (default is FALSE).
#' @param ... Additional arguments passed to geom_boxplot().
#'
#' @return A ggplot object representing the boxplot.
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter theme_minimal scale_fill_manual
#'   scale_x_discrete scale_y_continuous scale_color_manual theme element_text unit
#'   labs facet_wrap coord_flip ggsave
#' @importFrom dplyr filter mutate n_distinct %>%
#' @importFrom tidyr pivot_longer all_of
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_danger
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{
#' # define outcome variables
#' outcome_vars <- c(
#'   "env_climate_chg_concern",
#'   "env_climate_chg_cause",
#'   "env_climate_chg_real",
#'   "env_sat_nz_environment",
#'   "envefficacy"
#' )
#'
#' # basic usage with all waves
#' p1 <- margot_plot_boxplot(
#'   data = your_data,
#'   y_vars = outcome_vars,
#'   id_col = "id"
#' )
#'
#' # plotting specific waves with points shown and coordinates flipped
#' p2 <- margot_plot_boxplot(
#'   data = your_data,
#'   y_vars = outcome_vars,
#'   waves = c(2021, 2022),
#'   show_points = TRUE,
#'   coord_flip = TRUE,
#'   id_col = "id"
#' )
#'
#' # saving the plot with a custom prefix
#' margot_plot_boxplot(
#'   data = your_data,
#'   y_vars = outcome_vars,
#'   waves = c(2021, 2022, 2023),
#'   save_path = "path/to/save",
#'   prefix = "climate_change",
#'   include_timestamp = TRUE,
#'   id_col = "id"
#' )
#'
#' # customizing the plot appearance with flipped coordinates
#' p3 <- margot_plot_boxplot(
#'   data = your_data,
#'   y_vars = c("env_climate_chg_concern", "envefficacy"),
#'   waves = c(2021, 2022),
#'   title = "Climate Change Concern and Efficacy",
#'   y_label = "Score",
#'   legend_position = "right",
#'   facet_scales = "free",
#'   coord_flip = TRUE,
#'   id_col = "id"
#' )
#' }
#'
#' @export
margot_plot_boxplot <- function(data,
                                y_vars,
                                waves = NULL,
                                id_col = "id",
                                title = NULL,
                                y_label = NULL,
                                x_label = "Wave",
                                show_points = FALSE,
                                point_alpha = 0.05,
                                point_size = 0.5,
                                include_timestamp = FALSE,
                                save_path = NULL,
                                prefix = NULL,
                                width = 16,
                                height = 8,
                                legend_position = "bottom",
                                y_limits = NULL,
                                facet_scales = "free_y",
                                facet_ncol = NULL,
                                facet_nrow = NULL,
                                coord_flip = FALSE,
                                ...) {
  cli::cli_h1("Margot Plot Boxplot")

  tryCatch(
    {
      cli::cli_alert_info("Preparing data...")
      # define color palette
      modified_okabe_ito_colors <- c(
        "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
        "#D55E00", "#CC79A7", "#000000", "#999999"
      )

      # prepare the data
      df <- data

      # ensure wave column is present
      if (!"wave" %in% names(df)) {
        stop("The 'wave' column is missing from the data.")
      }

      # convert wave to factor and ensure all specified waves are included
      if (!is.null(waves)) {
        df$wave <- factor(df$wave, levels = waves)
      } else {
        df$wave <- as.factor(df$wave)
      }

      # filter waves if specified
      if (!is.null(waves)) {
        df <- df %>% dplyr::filter(wave %in% waves)
      }

      # ensure y_vars is a character vector
      if (!is.character(y_vars)) {
        y_vars <- as.character(y_vars)
      }

      # function to convert to title case and remove underscores
      format_label <- function(x) {
        label <- stringr::str_to_title(gsub("_", " ", x))
        # preserve "NZ" capitalisation
        label <- gsub("Nz", "NZ", label)
        return(label)
      }

      # create a named vector for label formatting
      formatted_labels <- setNames(sapply(y_vars, format_label), y_vars)

      # reshape data for plotting multiple y variables
      df_long <- df %>%
        tidyr::pivot_longer(
          cols = tidyr::all_of(y_vars),
          names_to = "variable",
          values_to = "value"
        ) %>%
        dplyr::mutate(variable = factor(variable, levels = y_vars, labels = formatted_labels[y_vars]))

      # remove NAs and count participants who responded to at least one outcome of interest
      df_long <- df_long %>%
        dplyr::filter(!is.na(value) & is.finite(value))

      # count total unique participants
      total_unique <- dplyr::n_distinct(df_long[[id_col]])

      # calculate total observations
      total_obs <- nrow(df_long)

      # determine the title
      if (is.null(title)) {
        title <- sprintf(
          "Distribution of %s by Wave\nTotal N = %d unique participants, %d observations",
          paste(formatted_labels, collapse = ", "),
          total_unique, total_obs
        )
      }

      if (include_timestamp) {
        title <- paste(title, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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

      # determine the number of unique variables
      n_vars <- length(y_vars)

      # explicitly recycle colors
      recycled_colors <- rep_len(modified_okabe_ito_colors, length.out = max(n_vars, nlevels(df_long$wave)))

      cli::cli_alert_success("Data prepared successfully")

      cli::cli_alert_info("Creating plot...")

      # create the ggplot
      if (n_vars == 1) {
        p <- ggplot2::ggplot(df_long, ggplot2::aes(x = wave, y = value, fill = wave)) +
          ggplot2::geom_boxplot(...) +
          ggplot2::theme_minimal() +
          ggplot2::scale_fill_manual(values = recycled_colors) +
          ggplot2::scale_x_discrete(drop = FALSE) + # force all levels to be shown
          ggplot2::theme(
            legend.position = legend_position,
            legend.text = ggplot2::element_text(size = 10),
            legend.title = ggplot2::element_text(size = 12),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
            strip.text = ggplot2::element_text(size = 10),
            panel.spacing = ggplot2::unit(0.2, "lines")
          ) +
          ggplot2::labs(
            title = title,
            y = y_label %||% formatted_labels[y_vars],
            x = x_label,
            fill = "Wave"
          )
      } else {
        p <- ggplot2::ggplot(df_long, ggplot2::aes(x = wave, y = value, fill = variable)) +
          ggplot2::geom_boxplot(...) +
          ggplot2::theme_minimal() +
          ggplot2::scale_fill_manual(values = recycled_colors, labels = formatted_labels) +
          ggplot2::scale_x_discrete(drop = FALSE) + # force all levels to be shown
          ggplot2::theme(
            legend.position = legend_position,
            legend.text = ggplot2::element_text(size = 10),
            legend.title = ggplot2::element_text(size = 12),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
            strip.text = ggplot2::element_text(size = 10),
            panel.spacing = ggplot2::unit(0.2, "lines")
          ) +
          ggplot2::labs(
            title = title,
            y = y_label %||% "Value",
            x = x_label,
            fill = "Variable"
          ) +
          ggplot2::facet_wrap(~variable, scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
      }

      p <- p + ggplot2::scale_y_continuous(limits = y_limits)

      # add points if requested
      if (show_points) {
        if (n_vars == 1) {
          p <- p + ggplot2::geom_jitter(ggplot2::aes(color = wave), width = 0.2, alpha = point_alpha, size = point_size) +
            ggplot2::scale_color_manual(values = recycled_colors)
        } else {
          p <- p + ggplot2::geom_jitter(ggplot2::aes(color = variable), width = 0.2, alpha = point_alpha, size = point_size) +
            ggplot2::scale_color_manual(values = recycled_colors, labels = formatted_labels)
        }
      }

      # flip coordinates if requested
      if (coord_flip) {
        p <- p + ggplot2::coord_flip()

        # adjust text angle for better readability when coordinates are flipped
        p <- p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
          axis.text.y = ggplot2::element_text(angle = 0, hjust = 1, size = 8)
        )
      }

      cli::cli_alert_success("Plot created")

      # save plot if a save path is provided
      if (!is.null(save_path)) {
        filename <- "boxplot"

        # add the optional prefix
        if (!is.null(prefix) && nzchar(prefix)) {
          filename <- paste0(prefix, "_", filename)
        }

        filename <- paste0(filename, "_", paste(y_vars, collapse = "_"))

        if (include_timestamp) {
          filename <- paste0(filename, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        }

        cli::cli_alert_info("Saving plot...")

        ggplot2::ggsave(
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
    },
    error = function(e) {
      cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
      print(e)
      return(NULL)
    }
  )
}
