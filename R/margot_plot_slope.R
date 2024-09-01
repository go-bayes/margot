#' Create a Slope Plot for Multiple Variables
#'
#' This function creates a ggplot2 visualisation to show trends in multiple variables over time.
#' It's possible to add vertical lines at significant events.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param y_vars A list of variable names or a single variable name to be plotted on the y-axis.
#' @param event_dates An optional vector of dates representing the events.
#' @param event_names An optional vector of names for the events. If NULL, events will be labeled "Event 1", "Event 2", etc.
#' @param start_date An optional start date for the x-axis.
#' @param end_date An optional end date for the x-axis.
#' @param title An optional title for the plot.
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
#'
#' @return A ggplot2 object representing the slope plot.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import cli
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
#'   title = "Trend in Warmth Towards Muslims",
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
#'   title = "Trends in Warmth Towards Muslims and Immigrants",
#'   y_label = "Warmth",
#'   x_label = "NZAVS Time 4 - 14 Cohort (2012-2023)",
#'   y_limits = c(1, 7)
#' )
#'
#' # Plot with points, using a subset of data
#' point_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = list("warm_asians", "warm_pacific"),
#'   plot_points = TRUE,
#'   point_alpha = 0.05,
#'   data_fraction = 0.1,
#'   seed = 123,
#'   title = "Trends in Warmth Towards Asians and Pacific Islanders",
#'   y_label = "Warmth"
#' )
#'
#' # Save the plot
#' saved_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = "political_orientation",
#'   title = "Trend in Political Orientation",
#'   save_path = here::here("outputs", "plots"),
#'   width = 10,
#'   height = 6
#' )
#'
#' # Custom styling
#' custom_plot <- margot_plot_slope(
#'   data = dat,
#'   y_vars = list("sat_government", "sat_nz_econ_conditions"),
#'   event_dates = "2017-10-26",
#'   event_names = "2017 Election",
#'   title = "Trends in Government and Business Satisfaction",
#'   y_label = "Satisfaction Level (0-10)",
#'   y_limits = c(0,10), # range of data differs from (1-7)
#'   event_line_color = "blue",
#'   event_label_color = "blue",
#'   legend_position = "top"
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
                              y_limits = NULL) {

  cli::cli_h1("Margot Plot Slope")

  # Initialize p as NULL
  p <- NULL

  tryCatch({
    # Function to transform labels
    transform_label <- function(label) {
      if (remove_underscores) {
        label <- gsub("_", " ", label)
      }
      if (use_title_case) {
        label <- tools::toTitleCase(label)
      }
      return(label)
    }

    cli::cli_alert_info("Preparing data...")
    # define color palette
    modified_okabe_ito_colors <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
                                   "#D55E00", "#CC79A7", "#000000", "#999999")

    # prepare the data
    df <- data %>%
      mutate(year = as.numeric(as.character(wave))) %>%
      mutate(timeline = base_date + tscore)

    # Sample the data if data_fraction < 1
    if (data_fraction < 1) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      df <- df %>% sample_frac(data_fraction)
    }

    # filter date range if specified
    if (!is.null(start_date)) {
      df <- df %>% filter(timeline >= as.Date(start_date))
    }
    if (!is.null(end_date)) {
      df <- df %>% filter(timeline <= as.Date(end_date))
    }

    # Ensure y_vars is a list
    if (!is.list(y_vars)) {
      y_vars <- list(y_vars)
    }

    # Check if all specified y variables exist in the data
    missing_vars <- y_vars[!y_vars %in% names(df)]
    if (length(missing_vars) > 0) {
      cli::cli_alert_warning("The following variables are not present in the data: {paste(missing_vars, collapse = ', ')}")
      y_vars <- y_vars[y_vars %in% names(df)]
      if (length(y_vars) == 0) {
        cli::cli_alert_danger("No valid y variables remain. Cannot create plot.")
        return(NULL)
      }
    }

    # Remove rows with NA or infinite values in any y_var
    for (var in y_vars) {
      df <- df %>% filter(!is.na(!!sym(var)) & is.finite(!!sym(var)))
    }

    cli::cli_alert_success("Data prepared successfully")

    # Warning for plotting points with multiple y variables
    if (plot_points && length(y_vars) > 1) {
      cli::cli_alert_warning("Plotting points with multiple y variables may result in a cluttered plot.")
    }

    cli::cli_alert_info("Creating base plot...")

    # Reshape data for plotting multiple y variables
    df_long <- df %>%
      tidyr::pivot_longer(cols = all_of(unlist(y_vars)),
                          names_to = "variable",
                          values_to = "value")

    # Determine y-axis limits
    if (is.null(y_limits)) {
      y_min <- min(df_long$value, na.rm = TRUE)
      y_max <- max(df_long$value, na.rm = TRUE)
      if (y_min >= 1 && y_max <= 7) {
        y_limits <- c(1, 7)
      } else {
        y_limits <- c(y_min, y_max)
      }
    }

    # create the ggplot
    p <- ggplot(df_long, aes(x = timeline, y = value, color = variable)) +
      geom_smooth(method = "lm", se = FALSE) +  # add linear trend lines
      theme_classic() +
      scale_color_manual(values = modified_okabe_ito_colors,
                         name = "Variables") +
      theme(
        legend.position = legend_position,
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
      ) +
      labs(
        title = title,
        y = transform_label(y_label %||% "Value"),
        x = transform_label(x_label %||% "Timeline")
      ) +
      scale_y_continuous(limits = y_limits)

    # add points if specified
    if (plot_points) {
      p <- p + geom_jitter(alpha = point_alpha, width = jitter_width)
    }

    cli::cli_alert_success("Base plot created")

    if (!is.null(event_dates)) {
      cli::cli_alert_info("Adding event lines and labels...")
      # add vertical lines for event dates
      for (i in seq_along(event_dates)) {
        event_date <- as.Date(event_dates[i])
        event_name <- if (!is.null(event_names) && length(event_names) >= i) event_names[i] else paste("Event", i)

        p <- p + geom_vline(xintercept = event_date,
                            color = event_line_color,
                            alpha = event_line_alpha,
                            linetype = event_line_type,
                            linewidth = event_line_width)

        # calculate y-position for the label to be at the top of the data points
        y_max <- max(y_limits)
        y_min <- min(y_limits)
        y_range <- y_max - y_min
        label_height <- nchar(event_name) * 0.015 * y_range  # Adjust this multiplier as needed
        y_position <- y_max

        # calculate x-position slightly to the left of the event line
        x_offset <- 5  # adjust this value to move labels further left or right
        x_position <- event_date - x_offset

        # add white rectangle with grey border behind the event label
        p <- p + annotate("rect",
                          xmin = x_position - 0.5,
                          xmax = x_position + 0.5,
                          ymin = y_position - label_height,
                          ymax = y_position,
                          fill = "white",
                          color = "grey50",
                          alpha = 0.9,
                          size = 0.25)

        # add text label for the event
        p <- p + annotate("text",
                          x = x_position,
                          y = y_position,
                          label = transform_label(event_name),
                          color = event_label_color,
                          size = event_label_size,
                          angle = 90,
                          vjust = 1,  # Align text to the top of the label box
                          hjust = 1)  # Align text to the right of the label box
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

      tryCatch({
        cli::cli_alert_info("Attempting to print plot...")
        print(p)
        cli::cli_alert_success("Plot printed successfully")

        cli::cli_alert_info("Saving plot as PNG...")
        ggsave(
          plot = p,
          filename = file.path(save_path, paste0(filename, ".png")),
          width = width,
          height = height,
          units = "in",
          device = 'png',
          dpi = 400
        )
        cli::cli_alert_success("Plot saved as PNG successfully")

        cli::cli_alert_info("Saving plot as .qs file...")
        # from the margot package (go-bayes/margot)
        margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

      }, error = function(e) {
        cli::cli_alert_danger("Error while saving: {conditionMessage(e)}")
      })
    } else {
      cli::cli_alert_info("No save path provided. Plot not saved.")
    }

    cli::cli_alert_success("Margot plot slope created successfully \U0001F44D")

    # return the ggplot object
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
