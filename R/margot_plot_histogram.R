#' Create a Histogram with Mean and Standard Deviation Highlights for Each Wave and Variable
#'
#' This function generates a histogram plot for specified variables across different waves of data.
#' It highlights the mean and standard deviation for each variable in each wave.
#'
#' @param data A data frame containing the variables for the plot.
#' @param col_names Vector of names of the columns to create histograms for.
#' @param id_col Name of the column containing unique identifiers. Default is "id".
#' @param wave_col Name of the column containing wave information. Default is "wave".
#' @param waves Vector of waves to include in the plot. If NULL, all waves are included.
#' @param binwidth Width of the bins for the histogram. Default is 0.5.
#' @param title An optional title for the plot. If NULL, an automatic title will be generated.
#' @param x_label An optional label for the x-axis. If NULL, "Value" will be used.
#' @param y_label An optional label for the y-axis. Default is "Count".
#' @param save_path An optional path to save the plot. If NULL, the plot will not be saved.
#' @param width The width of the saved plot in inches. Default is 12.
#' @param height The height of the saved plot in inches. Default is 8.
#' @param facet_scales Scales for facet. Either "fixed", "free_x", "free_y", or "free". Default is "free".
#' @param color_palette An optional custom color palette for the plot.
#' @param add_timestamp Logical. If TRUE, adds a timestamp to the saved filename. Default is FALSE.
#' @param file_prefix An optional prefix to add to the beginning of the saved filename. Default is an empty string.
#' @param mean_line_color Color of the vertical line representing the mean. Default is "black".
#' @param sd_line_color Color of the dashed lines representing the standard deviation. Default is "black".
#' @param vertical_facets Logical. If TRUE, facets are arranged vertically. If FALSE (default), facets are arranged horizontally.
#'
#' @return A ggplot2 object representing the histogram with highlights.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import cli
#' @import stringr
#'
#' @examples
#' # basic usage with default settings
#' margot_plot_histogram(
#'   data = your_data,
#'   col_names = c("variable1", "variable2"),
#'   id_col = "participant_id",
#'   wave_col = "survey_wave"
#' )
#'
#' # specify waves and custom binwidth
#' margot_plot_histogram(
#'   data = your_data,
#'   col_names = c("score1", "score2"),
#'   waves = c(2018, 2020),
#'   binwidth = 1
#' )
#'
#' # use custom labels and saving the plot with timestamp and prefix
#' margot_plot_histogram(
#'   data = your_data,
#'   col_names = c("attitude_measure"),
#'   title = "Distribution of Attitudes Over Time",
#'   x_label = "Attitude Score",
#'   save_path = "path/to/save/plot",
#'   add_timestamp = TRUE,
#'   file_prefix = "study1"
#' )
#'
#' # use a custom color palette and custom line colors
#' custom_colors <- c("#FF9999", "#66B2FF")
#' margot_plot_histogram(
#'   data = your_data,
#'   col_names = c("var1", "var2"),
#'   color_palette = custom_colors,
#'   mean_line_color = "red",
#'   sd_line_color = "blue"
#' )
#'
#' # use vertical faceting
#' margot_plot_histogram(
#'   data = your_data,
#'   col_names = c("var1", "var2"),
#'   vertical_facets = TRUE
#' )
#'
#' @export
margot_plot_histogram <- function(data,
                                  col_names,
                                  id_col = "id",
                                  wave_col = "wave",
                                  waves = NULL,
                                  binwidth = 0.5,
                                  title = NULL,
                                  x_label = NULL,
                                  y_label = "Count",
                                  save_path = NULL,
                                  width = 12,
                                  height = 8,
                                  facet_scales = "free",
                                  color_palette = NULL,
                                  add_timestamp = FALSE,
                                  file_prefix = "",
                                  mean_line_color = "black",
                                  sd_line_color = "black",
                                  vertical_facets = FALSE) {  # New parameter

  cli::cli_h1("Margot Plot Histogram")

  tryCatch(
    {
      # validate inputs
      if (!all(c(col_names, id_col, wave_col) %in% names(data))) {
        stop("One or more specified columns do not exist in the dataframe.")
      }

      # ensure wave column is a factor
      data[[wave_col]] <- as.factor(data[[wave_col]])

      # filter waves if specified
      if (!is.null(waves)) {
        data <- data[data[[wave_col]] %in% waves, ]
        if (nrow(data) == 0) stop("No data left after filtering for specified waves.")
      }

      # set default color palette if not provided
      if (is.null(color_palette)) {
        color_palette <- c(
          "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7", "#000000", "#999999"
        )
      }

      # reshape data for plotting multiple columns
      data_long <- tidyr::pivot_longer(data, cols = all_of(col_names), names_to = "variable", values_to = "value")

      # check for completely missing columns in any wave
      missing_columns <- data_long %>%
        group_by(!!sym(wave_col), variable) %>%
        summarise(all_missing = all(is.na(value)), .groups = "drop") %>%
        filter(all_missing)

      if (nrow(missing_columns) > 0) {
        cli::cli_alert_warning("The following columns are completely missing for some waves:")
        for (i in 1:nrow(missing_columns)) {
          cli::cli_alert_info("Wave {missing_columns[[wave_col]][i]}: {missing_columns$variable[i]}")
        }
      }

      # remove NAs and count participants who responded to at least one outcome of interest
      data_long <- data_long %>%
        filter(!is.na(value) & is.finite(value))

      # count total unique participants
      total_unique <- n_distinct(data_long[[id_col]])

      # calculate total observations
      total_obs <- nrow(data_long)

      # recycle colors if necessary
      n_variables <- length(unique(data_long$variable))
      recycled_colors <- rep_len(color_palette, length.out = n_variables)

      # compute statistics for each variable within each wave
      stats <- data_long %>%
        group_by(!!sym(wave_col), variable) %>%
        summarise(
          avg_val = mean(value, na.rm = TRUE),
          std_val = sd(value, na.rm = TRUE),
          n_obs = n(),
          n_unique = n_distinct(!!sym(id_col)),
          .groups = "drop"
        )

      # function to convert to title case and remove underscores
      format_label <- function(x) {
        label <- stringr::str_to_title(gsub("_", " ", x))
        # preserve "NZ" capitalisation
        label <- gsub("Nz", "NZ", label)
        return(label)
      }

      # determine the title based on the number of waves
      if (is.null(title)) {
        if (length(unique(data_long[[wave_col]])) == 1) {
          title <- sprintf(
            "Distribution of %s by Wave = %s\nTotal N = %d unique participants, %d observations",
            paste(format_label(col_names), collapse = ", "),
            unique(data_long[[wave_col]]),
            total_unique, total_obs
          )
        } else {
          title <- sprintf(
            "Distribution of %s by Wave\nTotal N = %d unique participants, %d observations",
            paste(format_label(col_names), collapse = ", "),
            total_unique, total_obs
          )
        }
      }

      # create the plot
      p <- ggplot(data_long, aes(x = value, fill = variable)) +
        geom_histogram(aes(y = after_stat(count)), binwidth = binwidth, color = "white", alpha = 0.7) +
        geom_vline(data = stats, aes(xintercept = avg_val), color = mean_line_color, linewidth = 1) +
        geom_vline(data = stats, aes(xintercept = avg_val - std_val), color = sd_line_color, linewidth = 0.5, linetype = "dashed") +
        geom_vline(data = stats, aes(xintercept = avg_val + std_val), color = sd_line_color, linewidth = 0.5, linetype = "dashed") +
        geom_text(
          data = stats,
          aes(x = avg_val, y = Inf, label = sprintf("Mean: %.2f\nSD: %.2f", avg_val, std_val)),
          vjust = 1, hjust = 0, size = 4
        ) +
        labs(
          title = title,
          x = x_label %||% "Value",
          y = y_label,
          fill = "Variable"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "bottom",
          strip.text = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(angle = 0, hjust = 1)
        ) +
        scale_fill_manual(values = recycled_colors, labels = format_label)

      # Modified faceting for different scenarios
      if (length(unique(data_long[[wave_col]])) > 1 && length(col_names) > 1) {
        if (vertical_facets) {
          p <- p + facet_grid(rows = vars(.data[[wave_col]], variable),
                              scales = facet_scales,
                              labeller = labeller(
                                .rows = as_labeller(format_label)
                              )
          )
        } else {
          p <- p + facet_grid(variable ~ .data[[wave_col]],
                              scales = facet_scales,
                              labeller = labeller(
                                .cols = as_labeller(format_label),
                                .rows = as_labeller(format_label)
                              )
          )
        }
      } else if (length(unique(data_long[[wave_col]])) > 1) {
        p <- p + facet_wrap(~ .data[[wave_col]],
                            scales = facet_scales,
                            labeller = as_labeller(format_label),
                            ncol = if (vertical_facets) 1 else NULL
        )
      } else if (length(col_names) > 1) {
        p <- p + facet_wrap(~variable,
                            scales = facet_scales,
                            labeller = as_labeller(format_label),
                            ncol = if (vertical_facets) 1 else NULL
        )
      }
      # save plot if a save path is provided
      if (!is.null(save_path)) {
        filename <- "histogram"

        # Add the optional prefix
        if (nzchar(file_prefix)) {
          filename <- paste0(file_prefix, "_", filename)
        }

        filename <- paste0(
          filename, "_",
          paste(col_names, collapse = "_"),
          "_by_", wave_col
        )

        # Add timestamp if requested
        if (add_timestamp) {
          filename <- paste0(filename, "_", format(Sys.Date(), "%Y%m%d"))
        }

        cli::cli_alert_info("Saving plot...")

        ggsave(
          plot = p,
          filename = file.path(save_path, paste0(filename, ".png")),
          width = width,
          height = height,
          units = "in",
          device = "png",
          dpi = 500
        )

        margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

        cli::cli_alert_success("Plot saved successfully as '{filename}' in '{save_path}'")
      } else {
        cli::cli_alert_info("No save path provided. Plot not saved.")
      }

      cli::cli_alert_success("Margot plot histogram created successfully \U0001F44D")

      return(p)
    },
    error = function(e) {
      cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
      print(e)
      return(NULL)
    }
  )
}
