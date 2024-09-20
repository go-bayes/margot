#' Create Boxplots with Covariates using ggplot2
#'
#' This function creates boxplots for one outcome variable across specified panel waves,
#' allowing for different groups as covariates. It combines features from
#' margot_plot_boxplot() and margot_plot_slope_covariate().
#'
#' @param data A data frame containing the variables to be plotted.
#' @param outcome The name of the outcome variable to be plotted.
#' @param covariate The name of the covariate variable for grouping.
#' @param waves A vector of wave values to include in the plot (default is NULL, which includes all waves).
#' @param id_col Name of the column containing unique identifiers (default is "id").
#' @param title The title of the plot (optional, auto-generated if NULL).
#' @param y_label The label for the y-axis (optional).
#' @param x_label The label for the x-axis (optional, defaults to "Wave").
#' @param color_label The label for the color legend (optional, defaults to the covariate name).
#' @param show_points Logical, whether to show individual data points (default is FALSE).
#' @param point_alpha Alpha value for data points if shown (default is 0.05).
#' @param point_size Size of data points if shown (default is 0.5).
#' @param include_timestamp Logical, whether to include timestamp in plot title and filename (default is FALSE).
#' @param save_path Path to save the plot (optional).
#' @param prefix Optional prefix for the saved file name (default is NULL).
#' @param width Width of the saved plot in inches (default is 12).
#' @param height Height of the saved plot in inches (default is 8).
#' @param legend_position Position of the legend (default is "right").
#' @param y_limits Y-axis limits (optional).
#' @param coord_flip Logical, whether to flip the coordinates of the plot (default is FALSE).
#' @param ... Additional arguments passed to geom_boxplot().
#'
#' @return A ggplot object representing the boxplot with covariates.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import cli
#' @import stringr
#' @import ggokabeito
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with all waves
#' p1 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "env_climate_chg_concern",
#'   covariate = "education",
#'   id_col = "id"
#' )
#'
#' # Example 2: Plotting specific waves with custom labels
#' p2 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "political_orientation",
#'   covariate = "age_group",
#'   waves = c(2021, 2022, 2023),
#'   y_label = "Political Orientation",
#'   color_label = "Age Group",
#'   id_col = "id"
#' )
#'
#' # Example 3: Showing individual points and flipping coordinates
#' p3 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "env_sat_nz_environment",
#'   covariate = "income_bracket",
#'   show_points = TRUE,
#'   coord_flip = TRUE,
#'   y_label = "Satisfaction with NZ Environment",
#'   color_label = "Income Bracket",
#'   id_col = "id"
#' )
#'
#' # Example 4: Customizing plot appearance and saving
#' p4 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "envefficacy",
#'   covariate = "gender",
#'   y_label = "Environmental Efficacy",
#'   color_label = "Gender",
#'   legend_position = "bottom",
#'   y_limits = c(1, 7),
#'   save_path = "path/to/save",
#'   prefix = "env_efficacy",
#'   width = 10,
#'   height = 6,
#'   id_col = "id"
#' )
#'
#' # Example 5: Using with categorical outcome and including timestamp
#' p5 <- margot_plot_boxplot_covariate(
#'   data = your_data,
#'   outcome = "env_climate_chg_cause",
#'   covariate = "political_party",
#'   y_label = "Perceived Cause of Climate Change",
#'   color_label = "Political Party",
#'   include_timestamp = TRUE,
#'   id_col = "id"
#' )
#' }
margot_plot_boxplot_covariate <- function(data,
                                          outcome,
                                          covariate,
                                          waves = NULL,
                                          id_col = "id",
                                          title = NULL,
                                          y_label = NULL,
                                          x_label = "Wave",
                                          color_label = NULL,
                                          show_points = FALSE,
                                          point_alpha = 0.05,
                                          point_size = 0.5,
                                          include_timestamp = FALSE,
                                          save_path = NULL,
                                          prefix = NULL,
                                          width = 16,
                                          height = 8,
                                          legend_position = "right",
                                          y_limits = NULL,
                                          coord_flip = FALSE,
                                          ...) {
  cli::cli_h1("Margot Plot Boxplot with Covariate")

  tryCatch(
    {
      cli::cli_alert_info("Preparing data...")

      # Prepare the data
      df <- data

      # Ensure wave column is present
      if (!"wave" %in% names(df)) {
        stop("The 'wave' column is missing from the data.")
      }

      # Convert wave to factor and ensure all specified waves are included
      if (!is.null(waves)) {
        df$wave <- factor(df$wave, levels = waves)
      } else {
        df$wave <- as.factor(df$wave)
      }

      # Filter waves if specified
      if (!is.null(waves)) {
        df <- df %>% dplyr::filter(wave %in% waves)
      }

      # Function to convert to title case and remove underscores
      format_label <- function(x) {
        stringr::str_to_title(gsub("_", " ", x))
      }

      # Format labels
      outcome_label <- format_label(outcome)
      covariate_label <- format_label(covariate)

      # Remove NAs and count participants who responded
      df <- df %>%
        dplyr::filter(!is.na(!!sym(outcome)) & is.finite(!!sym(outcome)) &
                        !is.na(!!sym(covariate)) & is.finite(!!sym(covariate)))

      # Count total unique participants
      total_unique <- dplyr::n_distinct(df[[id_col]])

      # Calculate total observations
      total_obs <- nrow(df)

      # Determine the title
      if (is.null(title)) {
        title <- sprintf(
          "Distribution of %s by Wave and %s\nTotal N = %d unique participants, %d observations",
          outcome_label, covariate_label, total_unique, total_obs
        )
      }

      if (include_timestamp) {
        title <- paste(title, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      }

      # Determine y-axis limits
      if (is.null(y_limits)) {
        y_min <- min(df[[outcome]], na.rm = TRUE)
        y_max <- max(df[[outcome]], na.rm = TRUE)
        if (y_min >= 1 && y_max <= 7) {
          y_limits <- c(1, 7)
        } else {
          y_limits <- c(y_min, y_max)
        }
      }

      cli::cli_alert_success("Data prepared successfully")

      cli::cli_alert_info("Creating plot...")

      # Create the ggplot
      p <- ggplot(df, aes(x = wave, y = !!sym(outcome), fill = !!sym(covariate))) +
        geom_boxplot(...) +
        theme_minimal() +
        ggokabeito::scale_fill_okabe_ito() +
        scale_x_discrete(drop = FALSE) + # Force all levels to be shown
        theme(
          legend.position = legend_position,
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          strip.text = element_text(size = 10),
          panel.spacing = unit(0.2, "lines")
        ) +
        labs(
          title = title,
          y = y_label %||% outcome_label,
          x = x_label,
          fill = color_label %||% covariate_label
        ) +
        scale_y_continuous(limits = y_limits)

      # Add points if requested
      if (show_points) {
        p <- p + geom_jitter(aes(color = !!sym(covariate)), width = 0.2, alpha = point_alpha, size = point_size) +
          ggokabeito::scale_color_okabe_ito()
      }

      # Flip coordinates if requested
      if (coord_flip) {
        p <- p + coord_flip()

        # Adjust text angle for better readability when coordinates are flipped
        p <- p + theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
          axis.text.y = element_text(angle = 0, hjust = 1, size = 8)
        )
      }

      cli::cli_alert_success("Plot created")

      # Save plot if a save path is provided
      if (!is.null(save_path)) {
        filename <- "boxplot_covariate"

        # Add the optional prefix
        if (!is.null(prefix) && nzchar(prefix)) {
          filename <- paste0(prefix, "_", filename)
        }

        filename <- paste0(filename, "_", outcome, "_by_", covariate)

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
          dpi = 300
        )

        margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

        cli::cli_alert_success("Plot saved successfully")
      } else {
        cli::cli_alert_info("No save path provided. Plot not saved.")
      }

      cli::cli_alert_success("Margot plot boxplot with covariate created successfully \U0001F44D")

      return(p)
    },
    error = function(e) {
      cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
      print(e)
      return(NULL)
    }
  )
}
