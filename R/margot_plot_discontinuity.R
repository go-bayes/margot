#' Create a Discontinuity Plot for Multiple Events
#'
#' This function creates a ggplot2 visualisation to show discontinuities in data across multiple events.
#' It's particularly useful for visualising changes in trends before and after significant events.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param y_var The name of the y-axis variable in the data frame.
#' @param event_dates A vector of dates representing the events.
#' @param event_names An optional vector of names for the events. If NULL, events will be labeled "Event 1", "Event 2", etc.
#' @param start_date An optional start date for the x-axis.
#' @param end_date An optional end date for the x-axis.
#' @param title An optional title for the plot.
#' @param y_label An optional label for the y-axis.
#' @param x_label An optional label for the x-axis.
#' @param smoothing_method The method used for smoothing. Default is "gam".
#' @param gam_k The number of knots to use if smoothing_method is "gam". Default is 4.
#' @param data_fraction The fraction of data to use. Default is 1 (use all data).
#' @param seed An optional seed for reproducibility when sampling data.
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
#'
#' @return A ggplot2 object representing the discontinuity plot.
#'
#' @importFrom ggplot2 ggplot aes geom_jitter theme_classic scale_color_manual theme element_text labs geom_smooth geom_vline annotate ggsave
#' @importFrom dplyr mutate filter select n_distinct sample_frac
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom rlang sym
#' @importFrom tools toTitleCase
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(margot)
#'
#' # Assume that 'dat' is your dataset and that 'path_talk' is defined
#' muslim_discontinuity_warmth_plot <- margot_plot_discontinuity(
#'   data = dat,
#'   y_var = "warm_muslims",
#'   event_dates = c("2019-03-15", "2020-03-26"),
#'   event_names = c("Christchurch Attack", "COVID-19 Lockdown"),
#'   start_date = "2012-06-06",
#'   title = "Discontinuity at multiple events (GAM)",
#'   y_label = "Muslim Warmth",
#'   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
#'   point_alpha = 0.05,
#'   smoothing_method = "gam",
#'   gam_k = 4,
#'   data_fraction = .1,
#'   seed = 123,
#'   save_path = here::here(path_talk)
#' )
#'
#' # Display the plot
#' print(muslim_discontinuity_warmth_plot)
#' }
margot_plot_discontinuity <- function(data,
                                      y_var,
                                      event_dates,
                                      event_names = NULL,
                                      start_date = NULL,
                                      end_date = NULL,
                                      title = NULL,
                                      y_label = NULL,
                                      x_label = NULL,
                                      smoothing_method = "gam",
                                      gam_k = 4,
                                      data_fraction = 1,
                                      seed = NULL,
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
                                      remove_underscores = TRUE) {
  cli::cli_h1("Margot Plot Discontinuity")

  # initialise p as NULL
  p <- NULL

  tryCatch(
    {
      # null coalescing operator replacement
      `%||%` <- function(x, y) if (is.null(x)) y else x

      cli::cli_alert_info("Preparing data...")
      # define color palette
      modified_okabe_ito_colors <- c(
        "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
        "#D55E00", "#CC79A7", "#000000", "#999999"
      )

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

      # remove rows with na or infinite values in y_var
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(y_var)) & is.finite(!!rlang::sym(y_var)))

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

      # generate automatic title if not provided
      if (is.null(title)) {
        year_range <- range(df$year, na.rm = TRUE)
        title <- sprintf(
          "%s Discontinuity Plot\nN = %s participants, %s observations; years %.0f - %.0f",
          transform_label(y_var),
          format(n_participants, big.mark = ","),
          format(n_observations, big.mark = ","),
          year_range[1],
          year_range[2] + 1
        )
      }

      # create event condition based on multiple event dates
      event_dates <- as.Date(event_dates)
      n_discontinuities <- length(event_dates) + 1

      df <- df %>%
        dplyr::mutate(event_condition = cut(timeline,
                                            breaks = c(as.Date(-Inf), event_dates, as.Date(Inf)),
                                            labels = seq_len(n_discontinuities),
                                            include.lowest = TRUE
        ))

      # recycle colors for the number of discontinuities
      recycled_colors <- rep_len(modified_okabe_ito_colors, length.out = n_discontinuities)

      cli::cli_alert_success("Data prepared successfully")

      cli::cli_alert_info("Creating base plot...")

      # create the ggplot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = timeline, y = !!rlang::sym(y_var), color = event_condition)) +
        ggplot2::geom_jitter(alpha = point_alpha, width = jitter_width) +
        ggplot2::theme_classic() +
        ggplot2::scale_color_manual(
          values = recycled_colors,
          name = "Event Periods"
        ) +
        ggplot2::theme(
          legend.position = legend_position,
          legend.text = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 12)
        ) +
        ggplot2::labs(
          title = title,
          y = transform_label(y_label %||% y_var),
          x = transform_label(x_label %||% "Timeline")
        )

      cli::cli_alert_success("Base plot created")

      cli::cli_alert_info("Adding smoothing...")

      # add smoothing directly without wrapper
      p <- p + tryCatch(
        {
          if (smoothing_method == "gam") {
            ggplot2::geom_smooth(
              method = "gam",
              formula = y ~ s(x, k = gam_k), se = FALSE
            )
          } else {
            ggplot2::geom_smooth(
              method = smoothing_method,
              formula = y ~ x, se = FALSE
            )
          }
        },
        error = function(e) {
          cli::cli_alert_danger("Failed to add smoothing: {conditionMessage(e)}")
          cli::cli_alert_info("Plotting without smoothing...")
          ggplot2::geom_blank()
        }
      )

      cli::cli_alert_success("Smoothing added")

      cli::cli_alert_info("Adding event lines and labels...")

      # add vertical lines for event dates
      for (i in seq_along(event_dates)) {
        event_date <- event_dates[i]
        event_name <- if (!is.null(event_names) && length(event_names) >= i) event_names[i] else paste("Event", i)

        p <- p + ggplot2::geom_vline(
          xintercept = event_date,
          color = event_line_color,
          alpha = event_line_alpha,
          linetype = event_line_type,
          linewidth = event_line_width
        )

        # calculate y-position for the label to be at the top of the data points
        y_max <- max(df[[y_var]], na.rm = TRUE)
        y_min <- min(df[[y_var]], na.rm = TRUE)
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
                                   linewidth = 0.25
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
      cli::cli_alert_info("Saving plot...")

      # save plot if a save path is provided
      if (!is.null(save_path)) {
        # generate filename
        filename <- paste0(
          "discontinuity_plot_", y_var, "_",
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

      cli::cli_alert_success("Margot plot discontinuity created successfully \U0001F44D")

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
