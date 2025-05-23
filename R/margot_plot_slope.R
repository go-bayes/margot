#' Create a Slope Plot for Multiple Variables
#'
#' This function creates a ggplot2 visualization to show trends in multiple variables over time.
#' It's possible to add vertical lines at significant events. The function now also counts and
#' reports the number of unique participants and observations. It includes options for faceting
#' to avoid overplotting when dealing with multiple variables.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param y_vars A list of variable names or a single variable name to be plotted on the y-axis.
#' @param event_dates An optional vector of dates representing the events.
#' @param event_names An optional vector of names for the events. If NULL, events will be labeled "Event 1", "Event 2", etc.
#' @param start_date An optional start date for the x-axis.
#' @param end_date An optional end date for the x-axis.
#' @param title An optional title for the plot. If NULL, an automatic title will be generated including the count of participants and observations.
#' @param y_label An optional label for the y-axis.
#' @param x_label An optional label for the x-axis.
#' @param data_fraction The fraction of data to use. Default is 1 (use all data).
#' @param seed An optional seed for reproducibility when sampling data.
#' @param plot_points Logical, whether to plot individual data points. Default is FALSE.
#' @param point_alpha The alpha (transparency) of the data points. Default is 0.03.
#' @param jitter_width The width of the jitter for the data points. Default is 1.
#' @param base_date The base date for the timeline. Default is "2009-06-30".
#' @param save_path An optional path to save the plot.
#' @param width The width of the saved plot in inches. Default is 12.
#' @param height The height of the saved plot in inches. Default is 8.
#' @param event_line_color The color of the event lines. Default is "darkred".
#' @param event_line_alpha The alpha of the event lines. Default is 0.7.
#' @param event_line_type The type of the event lines. Default is "dashed".
#' @param event_line_width The width of the event lines. Default is 0.5.
#' @param event_label_size The size of the event labels. Default is 3.
#' @param event_label_color The color of the event labels. Default is "darkred".
#' @param legend_position The position of the legend. Default is "bottom".
#' @param use_title_case Logical, whether to use title case for labels. Default is TRUE.
#' @param remove_underscores Logical, whether to remove underscores from labels. Default is TRUE.
#' @param y_limits An optional vector of two numbers specifying the y-axis limits.
#' @param color_palette An optional custom color palette. If NULL, a default palette will be used.
#' @param use_facets Logical, whether to use faceting for multiple variables. Default is TRUE.
#' @param facet_scales The scales parameter for facet_wrap. Default is "free_y".
#' @param facet_ncol The number of columns for facet_wrap. Default is NULL.
#' @param facet_nrow The number of rows for facet_wrap. Default is NULL.
#'
#' @return A ggplot2 object representing the slope plot.
#'
#' @importFrom ggplot2 ggplot aes geom_smooth theme_classic scale_color_manual theme element_text labs scale_y_continuous geom_jitter facet_wrap as_labeller geom_vline annotate ggsave
#' @importFrom dplyr mutate filter select n_distinct sample_frac all_of
#' @importFrom tidyr pivot_longer
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom rlang sym
#' @importFrom stringr str_to_title
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(tidyr)
#' library(here)
#'
#' # Basic usage with a single variable
#' single_var_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = "warm_muslims",
#'   start_date = "2012-06-06",
#'   y_label = "Warmth",
#'   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)"
#' )
#'
#' # Multiple variables with events and custom y-axis limits
#' multi_var_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = list("warm_muslims", "warm_immigrants"),
#'   event_dates = c("2019-03-15", "2021-01-01"),
#'   event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
#'   start_date = "2012-06-06",
#'   y_label = "Warmth",
#'   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
#'   y_limits = c(1, 7),
#'   use_facets = TRUE
#' )
#'
#' # Plot with points, using a subset of data and custom facet layout
#' point_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = list("warm_asians", "warm_pacific", "warm_immigrants"),
#'   plot_points = TRUE,
#'   point_alpha = 0.05,
#'   data_fraction = 0.1,
#'   seed = 123,
#'   y_label = "Warmth",
#'   use_facets = TRUE,
#'   facet_ncol = 2
#' )
#'
#' # Save the plot
#' saved_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = list("political_orientation", "social_dominance_orientation"),
#'   save_path = here::here("outputs", "plots"),
#'   width = 10,
#'   height = 6,
#'   use_facets = TRUE
#' )
#'
#' # Custom styling and color palette
#' custom_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = list("sat_government", "sat_nz_econ_conditions"),
#'   event_dates = "2017-10-26",
#'   event_names = "2017 Election",
#'   y_label = "Satisfaction Level (0-10)",
#'   y_limits = c(0, 10),
#'   event_line_color = "blue",
#'   event_label_color = "blue",
#'   legend_position = "top",
#'   color_palette = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
#'   use_facets = TRUE
#' )
#' }
margot_plot_slope <- function(data,
                              y_vars,
                              event_dates = NULL,
                              event_names = NULL,
                              start_date = NULL,
                              end_date = NULL,
                              title = NULL,
                              y_label = NULL,
                              x_label = NULL,
                              data_fraction = 1,
                              seed = NULL,
                              plot_points = FALSE,
                              point_alpha = 0.03,
                              jitter_width = 1,
                              base_date = as.Date("2009-06-30"),
                              save_path = NULL,
                              width = 12,
                              height = 8,
                              event_line_color = "darkred",
                              event_line_alpha = 0.7,
                              event_line_type = "dashed",
                              event_line_width = 0.5,
                              event_label_size = 3,
                              event_label_color = "darkred",
                              legend_position = "bottom",
                              use_title_case = TRUE,
                              remove_underscores = TRUE,
                              y_limits = NULL,
                              color_palette = NULL,
                              use_facets = TRUE,
                              facet_scales = "free_y",
                              facet_ncol = NULL,
                              facet_nrow = NULL) {
  cli::cli_h1("Margot Plot Slope")

  # initialize p as NULL
  p <- NULL

  tryCatch(
    {
      # null coalescing operator replacement
      `%||%` <- function(x, y) if (is.null(x)) y else x

      # function to transform labels
      # transform_label <- function(label) {
      #   if (remove_underscores) {
      #     label <- gsub("_", " ", label)
      #   }
      #   if (use_title_case) {
      #     label <- stringr::str_to_title(label)
      #     # preserve "NZ" capitalisation
      #     label <- gsub("Nz", "NZ", label)
      #   }
      #   return(label)
      # }

      cli::cli_alert_info("Preparing data...")
      # define color palette
      if (is.null(color_palette)) {
        color_palette <- c(
          "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7", "#000000", "#999999"
        )
      }

      # prepare the data
      df <- data %>%
        dplyr::mutate(year = as.numeric(as.character(wave))) %>%
        dplyr::mutate(timeline = base_date + tscore)

      # filter date range if specified
      if (!is.null(start_date)) {
        df <- df %>% dplyr::filter(timeline >= as.Date(start_date))
      }
      if (!is.null(end_date)) {
        df <- df %>% dplyr::filter(timeline <= as.Date(end_date))
      }

      # ensure y_vars is a list
      if (!is.list(y_vars)) {
        y_vars <- list(y_vars)
      }

      # check if all specified y variables exist in the data
      missing_vars <- y_vars[!y_vars %in% names(df)]
      if (length(missing_vars) > 0) {
        cli::cli_alert_warning("The following variables are not present in the data: {paste(missing_vars, collapse = ', ')}")
        y_vars <- y_vars[y_vars %in% names(df)]
        if (length(y_vars) == 0) {
          cli::cli_alert_danger("No valid y variables remain. Cannot create plot.")
          return(NULL)
        }
      }

      # remove rows with NA or infinite values in any y_var
      for (var in y_vars) {
        df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var)) & is.finite(!!rlang::sym(var)))
      }

      # calculate unique participants and observations after cleaning
      n_participants <- df %>%
        dplyr::select(id) %>%
        dplyr::n_distinct()
      n_observations <- nrow(df)

      # sample the data if data_fraction < 1
      if (data_fraction < 1) {
        if (!is.null(seed)) {
          set.seed(seed)
        }
        df <- df %>% dplyr::sample_frac(data_fraction)

        # recalculate counts after sampling
        n_participants <- df %>%
          dplyr::select(id) %>%
          dplyr::n_distinct()
        n_observations <- nrow(df)
      }

      cli::cli_alert_success("Data prepared successfully")

      # warning for plotting points with multiple y variables
      if (plot_points && length(y_vars) > 1) {
        cli::cli_alert_warning("Plotting points with multiple y variables may result in a cluttered plot.")
      }

      cli::cli_alert_info("Creating base plot...")

      # reshape data for plotting multiple y variables
      df_long <- df %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(unlist(y_vars)),
          names_to = "variable",
          values_to = "value"
        )

      # create a named vector for label formatting
      formatted_labels <- setNames(sapply(unlist(y_vars), transform_label), unlist(y_vars))

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

      # generate automatic title if not provided
      if (is.null(title)) {
        year_range <- range(df$year, na.rm = TRUE)
        title <- sprintf(
          "Slope Plot for %s\nN = %s participants, %s observations; years %.0f - %.0f",
          paste(formatted_labels, collapse = ", "),
          format(n_participants, big.mark = ","),
          format(n_observations, big.mark = ","),
          year_range[1], year_range[2] + 1
        )
      }

      # recycle colors
      n_vars <- length(unique(df_long$variable))
      recycled_colors <- rep_len(color_palette, n_vars)

      # create the ggplot
      p <- ggplot2::ggplot(df_long, ggplot2::aes(x = timeline, y = value, color = variable)) +
        ggplot2::geom_smooth(method = "lm", se = FALSE) + # add linear trend lines
        ggplot2::theme_classic() +
        ggplot2::scale_color_manual(
          values = recycled_colors,
          name = "Variables",
          labels = formatted_labels
        ) +
        ggplot2::theme(
          legend.position = legend_position,
          legend.text = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 12)
        ) +
        ggplot2::labs(
          title = title,
          y = transform_label(y_label %||% "Value"),
          x = transform_label(x_label %||% "Timeline")
        ) +
        ggplot2::scale_y_continuous(limits = y_limits)

      # add points if specified
      if (plot_points) {
        p <- p + ggplot2::geom_jitter(alpha = point_alpha, width = jitter_width)
      }

      # add faceting if specified
      if (use_facets && length(y_vars) > 1) {
        p <- p + ggplot2::facet_wrap(~variable,
                                     scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow,
                                     labeller = ggplot2::as_labeller(formatted_labels)
        )
      }

      cli::cli_alert_success("Base plot created")

      if (!is.null(event_dates)) {
        cli::cli_alert_info("Adding event lines and labels...")
        # add vertical lines for event dates
        for (i in seq_along(event_dates)) {
          event_date <- as.Date(event_dates[i])
          event_name <- if (!is.null(event_names) && length(event_names) >= i) event_names[i] else paste("Event", i)

          p <- p + ggplot2::geom_vline(
            xintercept = event_date,
            color = event_line_color,
            alpha = event_line_alpha,
            linetype = event_line_type,
            linewidth = event_line_width
          )

          # calculate y-position for the label to be at the top of the data points
          y_max <- max(y_limits)
          y_min <- min(y_limits)
          y_range <- y_max - y_min
          label_height <- nchar(event_name) * 0.015 * y_range # adjust this multiplier as needed
          y_position <- y_max

          # calculate x-position slightly to the left of the event line
          x_offset <- 5 # adjust this value to move labels further left or right
          x_position <- event_date - x_offset

          # add white rectangle with grey border behind the event label
          p <- p + ggplot2::annotate("rect",
                                     xmin = x_position - 0.5,
                                     xmax = x_position + 0.5,
                                     ymin = y_position - label_height,
                                     ymax = y_position,
                                     fill = "white",
                                     color = "grey50",
                                     alpha = 0.9,
                                     size = 0.25
          )

          # add text label for the event
          p <- p + ggplot2::annotate("text",
                                     x = x_position,
                                     y = y_position,
                                     label = transform_label(event_name),
                                     color = event_label_color,
                                     size = event_label_size,
                                     angle = 90,
                                     vjust = 1, # align text to the top of the label box
                                     hjust = 1  # align text to the right of the label box
          )
        }
        cli::cli_alert_success("Event lines and labels added")
      }

      cli::cli_alert_info("Saving plot...")

      # save plot if a save path is provided
      if (!is.null(save_path)) {
        # generate filename
        filename <- paste0(
          "slope_plot_",
          paste(unlist(y_vars), collapse = "_"),
          "_",
          format(Sys.Date(), "%Y%m%d")
        )

        tryCatch(
          {
            cli::cli_alert_info("Attempting to print plot...")
            print(p)
            cli::cli_alert_success("Plot printed successfully")

            cli::cli_alert_info("Saving plot as PNG...")
            ggplot2::ggsave(
              plot = p,
              filename = file.path(save_path, paste0(filename, ".png")),
              width = width,
              height = height,
              units = "in",
              device = "png",
              dpi = 400
            )
            cli::cli_alert_success("Plot saved as PNG successfully")

            cli::cli_alert_info("Saving plot as .qs file...")
            # from the margot package (go-bayes/margot)
            margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
          },
          error = function(e) {
            cli::cli_alert_danger("Error while saving: {conditionMessage(e)}")
          }
        )
      } else {
        cli::cli_alert_info("No save path provided. Plot not saved.")
      }

      cli::cli_alert_success("Margot plot slope created successfully \U0001F44D")

      # return the ggplot object
      return(p)
    },
    error = function(e) {
      cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
      print(e)
      return(NULL)
    },
    warning = function(w) {
      cli::cli_alert_warning("A warning occurred: {conditionMessage(w)}")
      print(w)
    }
  )
}
