#' Visualise Shifts in Data Distributions with Highlighted Ranges
#'
#' This function creates a histogram that highlights a specified range of values to visualise shifts in data distributions.
#' The highlighted range can indicate areas of interest, such as shifts up or down in the distribution.
#' This visualisation is useful for understanding the implications of causal contrasts, such as modified treatment policies.
#' The fill colour of the histogram is dynamically adjusted based on the specified direction of the shift.
#'
#' @param df A dataframe containing the variable of interest.
#' @param col_name The name of the column in \code{df} to be visualised in the histogram. This should be a numeric variable.
#' @param binwidth The width of the bins for the histogram. Default is 1. Adjust this based on the distribution and scale of your data.
#' @param range_highlight A numeric vector of length 2 specifying the start and end of the range to highlight. If \code{NULL}, no range is highlighted.
#' @param shift A character string indicating the direction of the shift, either "up" or "down". Default is "up".
#' @param show_avg_line A logical value indicating whether to display a vertical line representing the average value. Default is \code{TRUE}.
#' @param print_avg_value A logical value indicating whether to print the numerical value of the average on the plot.
#'                        When \code{TRUE}, the value is printed horizontally to the left of the mean line at the bottom of the graph.
#'                        Default is \code{TRUE}.
#' @param show_sd_line A logical value indicating whether to display vertical dashed lines (in grey) representing one standard deviation on either side of the average. Default is \code{TRUE}.
#' @param title An optional custom title for the plot. If \code{NULL}, a default title will be generated.
#' @param subtitle An optional custom subtitle for the plot. If \code{NULL}, a default subtitle will be generated.
#' @param x_lab An optional label for the x-axis. If \code{NULL}, the formatted column name is used.
#' @param y_lab The label for the y-axis. Default is "Count".
#' @param save_path An optional path to save the plot. If \code{NULL}, the plot will not be saved.
#' @param width The width of the saved plot in inches. Default is 12.
#' @param height The height of the saved plot in inches. Default is 8.
#' @param include_timestamp A logical value indicating whether to include a timestamp in the saved filename. Default is \code{FALSE}.
#'
#' @return A ggplot object representing the histogram with specified highlights.
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_h1
#' @importFrom stringr str_to_title
#'
#' @export
margot_plot_shift <- function(df, col_name, binwidth = 1, range_highlight = NULL, shift = "up",
                              show_avg_line = TRUE, print_avg_value = TRUE, show_sd_line = TRUE,
                              title = NULL, subtitle = NULL, x_lab = NULL, y_lab = "Count",
                              save_path = NULL, width = 12, height = 8, include_timestamp = FALSE) {
  cli::cli_h1("Margot Plot Shift")

  tryCatch(
    {
      # Input validation
      if (!col_name %in% names(df)) {
        cli::cli_alert_danger("Column '{col_name}' not found in the dataframe.")
        return(NULL)
      }
      if (all(is.na(df[[col_name]]))) {
        cli::cli_alert_danger("The specified column contains only NA values.")
        return(NULL)
      }
      if (!shift %in% c("up", "down")) {
        cli::cli_alert_danger("'shift' must be either 'up' or 'down'.")
        return(NULL)
      }

      # Function to convert to title case and remove underscores
      format_label <- function(x) {
        label <- stringr::str_to_title(gsub("_", " ", x))
        label <- gsub("Nz", "NZ", label)
        return(label)
      }

      formatted_col_name <- format_label(col_name)

      # Calculate average and standard deviation
      avg_val <- mean(df[[col_name]], na.rm = TRUE)
      sd_val <- sd(df[[col_name]], na.rm = TRUE)
      cli::cli_alert_info("Average value of {formatted_col_name}: {round(avg_val, 2)}")

      # Determine the fill colour based on shift direction
      highlight_color <- if (shift == "up") "gold2" else "dodgerblue"

      # Create fill colour column based on range_highlight
      if (!is.null(range_highlight) && length(range_highlight) == 2) {
        df$fill_color <- ifelse(df[[col_name]] >= range_highlight[1] & df[[col_name]] <= range_highlight[2],
          highlight_color, "lightgray"
        )
        cli::cli_alert_info("Highlighting range: [{range_highlight[1]}, {range_highlight[2]}]")
      } else {
        df$fill_color <- "lightgray"
        cli::cli_alert_info("No range highlighted")
      }

      # Define subtitle based on shift direction
      if (is.null(subtitle)) {
        subtitle <- if (shift == "up") {
          "Highlights region shifted up to boundary with grey"
        } else {
          "Highlights region shifted down to boundary with grey"
        }
        if (show_avg_line) {
          subtitle <- paste(subtitle, "\nRed dashed line shows the average value")
        }
        if (show_sd_line) {
          subtitle <- paste(subtitle, "\nGrey dashed lines show ± one standard deviation")
        }
      }

      # Create the plot
      p <- ggplot(df, aes(x = !!rlang::sym(col_name), fill = fill_color)) +
        geom_histogram(binwidth = binwidth, alpha = 0.7) +
        scale_fill_identity() +
        labs(
          title = ifelse(is.null(title),
            paste("Distribution of", formatted_col_name, "with Shift Intervention"),
            title
          ),
          subtitle = subtitle,
          x = ifelse(is.null(x_lab), formatted_col_name, x_lab),
          y = y_lab,
          caption = sprintf("N = %d observations", nrow(df))
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      # Add average line and annotation
      if (show_avg_line) {
        p <- p + geom_vline(xintercept = avg_val, color = "darkred", linetype = "dashed", linewidth = 0.75)
        if (print_avg_value) {
          p <- p + geom_text(
            x = avg_val, y = 0, label = round(avg_val, 2),
            color = "darkred", hjust = 1.1, vjust = -0.5, angle = 0
          )
        }
      }

      # Add standard deviation lines if requested
      if (show_sd_line) {
        p <- p +
          geom_vline(xintercept = avg_val - sd_val, color = "grey", linetype = "dashed", linewidth = 0.75) +
          geom_vline(xintercept = avg_val + sd_val, color = "grey", linetype = "dashed", linewidth = 0.75)
      }

      # Save plot if a save path is provided
      if (!is.null(save_path)) {
        filename <- paste0("shift_", col_name, "_", shift)
        if (include_timestamp) {
          filename <- paste0(filename, "_", format(Sys.Date(), "%Y%m%d"))
        }
        cli::cli_alert_info("Saving plot...")
        ggsave(
          plot = p,
          filename = file.path(save_path, paste0(filename, ".png")),
          width = width, height = height, units = "in",
          device = "png", dpi = 300
        )
        margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
        cli::cli_alert_success("Plot saved successfully as '{filename}' in '{save_path}'")
      } else {
        cli::cli_alert_info("No save path provided. Plot not saved.")
      }

      cli::cli_alert_success("Margot plot shift created successfully")
      return(p)
    },
    error = function(e) {
      cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
      print(e)
      return(NULL)
    }
  )
}
# margot_plot_shift <- function(df, col_name, binwidth = 1, range_highlight = NULL, shift = "up", show_avg_line = TRUE,
#                               title = NULL, subtitle = NULL, x_lab = NULL, y_lab = "Count",
#                               save_path = NULL, width = 12, height = 8, include_timestamp = FALSE) {
#
#   cli::cli_h1("Margot Plot Shift")
#
#   tryCatch({
#     # Input validation
#     if(!col_name %in% names(df)) {
#       cli::cli_alert_danger("Column '{col_name}' not found in the dataframe.")
#       return(NULL)
#     }
#     if(all(is.na(df[[col_name]]))) {
#       cli::cli_alert_danger("The specified column contains only NA values.")
#       return(NULL)
#     }
#     if(!shift %in% c("up", "down")) {
#       cli::cli_alert_danger("'shift' must be either 'up' or 'down'.")
#       return(NULL)
#     }
#
#     # Function to convert to title case and remove underscores
#     format_label <- function(x) {
#       label <- stringr::str_to_title(gsub("_", " ", x))
#       # preserve "NZ" capitalisation
#       label <- gsub("Nz", "NZ", label)
#       return(label)
#     }
#
#     # Format column name for labels
#     formatted_col_name <- format_label(col_name)
#
#     # Calculate average value for the vertical line
#     avg_val <- mean(df[[col_name]], na.rm = TRUE)
#     cli::cli_alert_info("Average value of {formatted_col_name}: {round(avg_val, 2)}")
#
#     # Determine the fill colour based on the shift direction
#     highlight_color <- if(shift == "up") "gold2" else "dodgerblue"
#
#     # Create a new column for fill colour based on range_highlight
#     if (!is.null(range_highlight) && length(range_highlight) == 2) {
#       df$fill_color <- ifelse(df[[col_name]] >= range_highlight[1] & df[[col_name]] <= range_highlight[2], highlight_color, "lightgray")
#       cli::cli_alert_info("Highlighting range: [{range_highlight[1]}, {range_highlight[2]}]")
#     } else {
#       df$fill_color <- "lightgray" # Default colour if no range_highlight is provided
#       cli::cli_alert_info("No range highlighted")
#     }
#
#     # Define subtitle based on the shift direction
#     if (is.null(subtitle)) {
#       subtitle <- if(shift == "up") {
#         "Highlights region shifted up to boundary with grey"
#       } else {
#         "Highlights region shifted down to boundary with grey"
#       }
#       if(show_avg_line) {
#         subtitle <- paste(subtitle, "\nRed dashed line shows the average value")
#       }
#     }
#
#     # Create the plot
#     p <- ggplot(df, aes(x = !!rlang::sym(col_name), fill = fill_color)) +
#       geom_histogram(binwidth = binwidth, alpha = 0.7) +
#       scale_fill_identity() +
#       labs(title = ifelse(is.null(title), paste("Distribution of", formatted_col_name, "with Shift Intervention"), title),
#            subtitle = subtitle,
#            x = ifelse(is.null(x_lab), formatted_col_name, x_lab),
#            y = y_lab,
#            caption = sprintf("N = %d observations", nrow(df))) +
#       theme_minimal() +
#       theme(text = element_text(size = 12),
#             axis.text.x = element_text(angle = 45, hjust = 1))
#
#     # Conditionally add the average value line
#     if(show_avg_line) {
#       p <- p + geom_vline(xintercept = avg_val, color = "darkred", linetype = "dashed", linewidth = .75)
#     }
#
#     # Save plot if a save path is provided
#     if (!is.null(save_path)) {
#       filename <- paste0(
#         "shift_", col_name, "_", shift
#       )
#
#       if (include_timestamp) {
#         filename <- paste0(filename, "_", format(Sys.Date(), "%Y%m%d"))
#       }
#
#       cli::cli_alert_info("Saving plot...")
#
#       ggsave(
#         plot = p,
#         filename = file.path(save_path, paste0(filename, ".png")),
#         width = width,
#         height = height,
#         units = "in",
#         device = 'png',
#         dpi = 300
#       )
#
#       margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)
#
#       cli::cli_alert_success("Plot saved successfully as '{filename}' in '{save_path}'")
#     } else {
#       cli::cli_alert_info("No save path provided. Plot not saved.")
#     }
#
#     cli::cli_alert_success("Margot plot shift created successfully \U0001F44D")
#
#     return(p)
#   }, error = function(e) {
#     cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
#     print(e)
#     return(NULL)
#   })
# }
