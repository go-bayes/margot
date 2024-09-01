#' Create a Boxplot with Facets using ggplot2
#'
#' This function creates a boxplot for one or more variables, faceted by a specified variable (usually time).
#' It offers various customization options for the plot appearance and layout.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param y_vars A list of variable names to be plotted on the y-axis.
#' @param x_var The name of the variable to be used for faceting (default is "wave").
#' @param title The title of the plot (optional).
#' @param y_label The label for the y-axis (optional).
#' @param x_label The label for the x-axis (optional, not typically used due to faceting).
#' @param data_fraction The fraction of data to use (default is 1, i.e., all data).
#' @param seed A seed for random sampling if data_fraction < 1 (optional).
#' @param show_points Logical, whether to show individual data points (default is FALSE).
#' @param point_alpha Alpha value for data points if shown (default is 0.3).
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
#' @examples
#' # Example 1: Basic usage with two variables
#' boxplot_basic <- margot_plot_boxplot(
#'   data = your_data,
#'   y_vars = list("variable1", "variable2"),
#'   x_var = "wave",
#'   title = "My Boxplot"
#' )
#'
#' # Example 2: Using facet wrap with a single row
#' boxplot_wrap_single_row <- margot_plot_boxplot(
#'   data = your_data,
#'   y_vars = list("warm_muslims", "warm_immigrants"),
#'   x_var = "wave",
#'   title = "Warmth Towards Muslims and Immigrants",
#'   y_label = "Warmth",
#'   facet_type = "wrap",
#'   facet_nrow = 1,
#'   width = 20,
#'   height = 6,
#'   save_path = here::here("path", "to", "save")
#' )
#'
#' # Example 3: Showing individual data points
#' boxplot_with_points <- margot_plot_boxplot(
#'   data = your_data,
#'   y_vars = list("variable1", "variable2", "variable3"),
#'   x_var = "wave",
#'   title = "Boxplot with Data Points",
#'   show_points = TRUE,
#'   point_alpha = 0.2,
#'   point_size = 0.3
#' )
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import cli
#'
#' @export
margot_plot_boxplot <- function(data,
                                y_vars,
                                x_var = "wave",
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
                                ...) {  # This allows passing additional arguments to geom_boxplot()

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

    # sample the data if data_fraction < 1
    if (data_fraction < 1) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      df <- df %>% sample_frac(data_fraction)
    }

    # ensure y_vars is a list
    if (!is.list(y_vars)) {
      y_vars <- list(y_vars)
    }

    # reshape data for plotting multiple y variables
    df_long <- df %>%
      tidyr::pivot_longer(cols = all_of(unlist(y_vars)),
                          names_to = "variable",
                          values_to = "value")

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

    cli::cli_alert_success("Data prepared successfully")

    cli::cli_alert_info("Creating plot...")

    # create the ggplot
    p <- ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot(...) +  # Pass additional arguments to geom_boxplot()
      theme_minimal() +
      scale_fill_manual(values = modified_okabe_ito_colors) +
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
        x = NULL  # Remove x-axis label as it's redundant with facets
      ) +
      scale_y_continuous(limits = y_limits)

    # Add faceting based on user choice
    if (facet_type == "grid") {
      p <- p + facet_grid(. ~ .data[[x_var]], scales = facet_scales, space = facet_space)
    } else if (facet_type == "wrap") {
      p <- p + facet_wrap(~ .data[[x_var]], scales = facet_scales, ncol = facet_ncol, nrow = facet_nrow)
    }

    # add points if requested
    if (show_points) {
      p <- p + geom_jitter(aes(color = variable), width = 0.2, alpha = point_alpha, size = point_size) +
        scale_color_manual(values = modified_okabe_ito_colors)
    }

    cli::cli_alert_success("Plot created")

    # save plot if a save path is provided
    if (!is.null(save_path)) {
      filename <- paste0(
        "boxplot_",
        paste(unlist(y_vars), collapse = "_"),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".png"
      )

      ggsave(
        plot = p,
        filename = file.path(save_path, filename),
        width = width,
        height = height,
        units = "in",
        dpi = 300
      )
      cli::cli_alert_success("Plot saved successfully")
    }

    return(p)
  }, error = function(e) {
    cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
    print(e)
    return(NULL)
  })
}
