#' Create a Coloured Histogram with Quantile or Custom Breaks
#'
#' This function creates a histogram with coloured regions based on quantile breaks or custom breaks.
#' It uses the `create_ordered_variable` function to categorise the data and then plots the histogram
#' with different colours for each category.
#'
#' @param df A data frame containing the variable to be plotted.
#' @param col_name The name of the column in the data frame to be plotted.
#' @param n_divisions The number of divisions for quantile breaks. Ignored if custom_breaks is provided.
#' @param custom_breaks A numeric vector of custom break points.
#' @param cutpoint_inclusive Character. Either "lower" or "upper", specifying whether the cutpoint should be included in the lower or upper interval.
#' @param ties.method A character string specifying how ties should be handled. See ?quantile for details.
#' @param colour_palette A vector of colors to use for the intervals. If NULL, uses the Okabe-Ito palette.
#' @param hist_colour The color of the histogram borders.
#' @param line_type The type of line to use for the histogram borders.
#' @param line_width The width of the lines for the histogram borders.
#' @param title The title of the plot. If NULL, a default title is used.
#' @param subtitle The subtitle of the plot. If NULL, a default subtitle is used.
#' @param x_lab The label for the x-axis. If NULL, the formatted column name is used.
#' @param y_lab The label for the y-axis. Default is "Count".
#' @param theme_choice The ggplot2 theme to use. Default is theme_classic().
#' @param text_size The base text size for the plot.
#' @param axis_text_angle The angle of the x-axis text.
#' @param x_scale_transform Optional. A transformation for the x-axis (e.g., "log10").
#' @param y_scale_transform Optional. A transformation for the y-axis (e.g., "log10").
#' @param additional_layers A list of additional ggplot2 layers to add to the plot.
#' @param binwidth The width of the bins for the histogram. If NULL, calculated automatically.
#' @param save_path An optional path to save the plot. If NULL, the plot will not be saved.
#' @param width The width of the saved plot in inches. Default is 16.
#' @param height The height of the saved plot in inches. Default is 10.
#' @param legend_position The position of the legend. Can be "right", "left", "bottom", "top", or a two-element numeric vector.
#' @param include_timestamp A logical value indicating whether to include a timestamp in the saved filename. Default is FALSE.
#' @param file_prefix An optional prefix to add to the beginning of the saved filename.
#'
#' @return A ggplot2 object representing the colored histogram.
#'
#' @import ggplot2
#' @importFrom ggokabeito palette_okabe_ito
#' @importFrom rlang sym
#' @importFrom stringr str_to_title
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger
#'
#' @export
margot_plot_categorical <- function(df, col_name, n_divisions = NULL, custom_breaks = NULL,
                                    cutpoint_inclusive = "upper",
                                    ties.method = NULL,
                                    colour_palette = NULL,
                                    hist_colour = "black",
                                    line_type = "solid", line_width = 0.75,
                                    title = NULL, subtitle = NULL,
                                    x_lab = NULL, y_lab = "Count",
                                    theme_choice = theme_classic(),
                                    text_size = 12, axis_text_angle = 45,
                                    x_scale_transform = NULL, y_scale_transform = NULL,
                                    additional_layers = NULL,
                                    binwidth = NULL,
                                    save_path = NULL,
                                    width = 16, height = 10,
                                    legend_position = "right",
                                    include_timestamp = FALSE,
                                    file_prefix = "") {

  cli::cli_h1("Margot Plot Categorical")

  tryCatch({
    # Input validation
    if (!col_name %in% names(df)) {
      cli::cli_alert_danger("Column '{col_name}' not found in the dataframe.")
      return(NULL)
    }

    # function to convert to title case and remove underscores
    format_label <- function(x) {
      label <- stringr::str_to_title(gsub("_", " ", x))
      # preserve "NZ" capitalisation
      label <- gsub("Nz", "NZ", label)
      return(label)
    }

    # Remove NAs and warn the user
    original_rows <- nrow(df)
    df <- df[!is.na(df[[col_name]]), ]
    removed_rows <- original_rows - nrow(df)
    if (removed_rows > 0) {
      cli::cli_alert_warning("{removed_rows} rows with NA values in {col_name} were removed.")
    }

    # Use create_ordered_variable to get the breaks and labels
    result_df <- create_ordered_variable(df, col_name, n_divisions = n_divisions,
                                         custom_breaks = custom_breaks,
                                         cutpoint_inclusive = cutpoint_inclusive,
                                         ties.method = ties.method)

    # Check for the new column name in result_df
    # First, try to determine based on custom_breaks or n_divisions
    local_n_divisions <- if (!is.null(custom_breaks)) {
      length(custom_breaks) - 1
    } else {
      n_divisions
    }

    # Now determine suffix based on divisions
    suffix <- if (!is.null(local_n_divisions) && local_n_divisions == 2) "_binary" else "_cat"
    expected_col_name <- paste0(col_name, suffix)

    # If the expected column isn't found, try the alternative suffix
    if (!expected_col_name %in% names(result_df)) {
      alternative_suffix <- if (suffix == "_binary") "_cat" else "_binary"
      alternative_col_name <- paste0(col_name, alternative_suffix)

      if (alternative_col_name %in% names(result_df)) {
        # Use the alternative column if found
        new_col_name <- alternative_col_name
      } else {
        # Search for any column that matches the pattern col_name + suffix
        possible_cols <- grep(paste0("^", col_name, "_(binary|cat)$"), names(result_df), value = TRUE)

        if (length(possible_cols) > 0) {
          new_col_name <- possible_cols[1]
        } else {
          cli::cli_alert_danger("No categorical column found for '{col_name}' in the result dataframe.")
          return(NULL)
        }
      }
    } else {
      # Expected column exists
      new_col_name <- expected_col_name
    }

    # Instead of cli::cli_alert_info
    message(paste0("\nUsing categorical column: ", new_col_name, "\n"))

    # Get the levels of the new categorical variable
    cat_levels <- levels(result_df[[new_col_name]])

    # Set up color palette
    if (is.null(colour_palette)) {
      okabe_ito_palette <- ggokabeito::palette_okabe_ito()
      colour_palette <- okabe_ito_palette[1:length(cat_levels)]
    } else if (length(colour_palette) < length(cat_levels)) {
      cli::cli_alert_danger("The provided colour palette doesn't have enough colors for all intervals.")
      return(NULL)
    }

    # Determine binwidth if not provided
    if (is.null(binwidth)) {
      binwidth <- diff(range(df[[col_name]], na.rm = TRUE)) / 30
    }

    # Format column name for labels
    formatted_col_name <- format_label(col_name)

    # Create the plot
    p <- ggplot(result_df, aes(x = !!rlang::sym(col_name), fill = !!rlang::sym(new_col_name))) +
      geom_histogram(aes(y = after_stat(count)),
                     binwidth = binwidth,
                     colour = hist_colour) +
      scale_fill_manual(values = colour_palette, name = "Intervals") +
      labs(title = ifelse(is.null(title),
                          paste(formatted_col_name, "Distribution"),
                          title),
           subtitle = ifelse(is.null(subtitle),
                             paste("Colored regions indicate intervals:", paste(cat_levels, collapse = ", ")),
                             subtitle),
           x = ifelse(is.null(x_lab), formatted_col_name, x_lab),
           y = y_lab,
           caption = sprintf("N = %d observations", nrow(result_df))) +
      theme_choice +
      theme(text = element_text(size = text_size),
            axis.text.x = element_text(angle = axis_text_angle, hjust = 1),
            legend.position = legend_position)

    # Axis transformations if requested
    if (!is.null(x_scale_transform)) {
      p <- p + scale_x_continuous(trans = x_scale_transform)
    }
    if (!is.null(y_scale_transform)) {
      p <- p + scale_y_continuous(trans = y_scale_transform)
    }

    # Add any additional layers
    if (!is.null(additional_layers)) {
      for (layer in additional_layers) {
        p <- p + layer
      }
    }

    # Save plot if a save path is provided
    if (!is.null(save_path)) {
      filename <- "categorical"

      # Add the optional prefix
      if (nzchar(file_prefix)) {
        filename <- paste0(file_prefix, "_", filename)
      }

      filename <- paste0(filename, "_", col_name)

      if (include_timestamp) {
        filename <- paste0(filename, "_", format(Sys.Date(), "%Y%m%d"))
      }

      cli::cli_alert_info("Saving plot...")

      ggsave(
        plot = p,
        filename = file.path(save_path, paste0(filename, ".png")),
        width = width,
        height = height,
        units = "in",
        device = 'png',
        dpi = 300
      )

      margot::here_save_qs(p, filename, save_path, preset = "high", nthreads = 1)

      cli::cli_alert_success("Plot saved successfully as '{filename}' in '{save_path}'")
    } else {
      cli::cli_alert_info("No save path provided. Plot not saved.")
    }

    cli::cli_alert_success("Margot plot categorical created successfully \U0001F44D")

    return(p)
  }, error = function(e) {
    cli::cli_alert_danger("An error occurred: {conditionMessage(e)}")
    print(e)
    return(NULL)
  })
}

