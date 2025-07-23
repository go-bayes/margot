#' Create a Coloured Histogram with Quantile or Custom Breaks (DEPRECATED)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `margot_plot_histogram()` instead.
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
#' @param x_lab The label for the x-axis. If NULL, the column name is used.
#' @param y_lab The label for the y-axis. Default is "Count".
#' @param theme_choice The ggplot2 theme to use. Default is theme_classic().
#' @param text_size The base text size for the plot.
#' @param axis_text_angle The angle of the x-axis text.
#' @param add_density Logical. If TRUE, adds a density curve to the plot.
#' @param add_rug Logical. If TRUE, adds a rug plot to the x-axis.
#' @param facet_var Optional. The name of a variable to use for faceting.
#' @param x_scale_transform Optional. A transformation for the x-axis (e.g., "log10").
#' @param y_scale_transform Optional. A transformation for the y-axis (e.g., "log10").
#' @param additional_layers A list of additional ggplot2 layers to add to the plot.
#' @param binwidth The width of the bins for the histogram. If NULL, calculated automatically.
#'
#' @return A ggplot2 object representing the colored histogram.
#'
#' @examples
#' df <- data.frame(value = rnorm(1000))
#' coloured_histogram_quantiles(df, "value", n_divisions = 4)
#'
#' # With custom breaks
#' coloured_histogram_quantiles(df, "value", custom_breaks = c(-2, -1, 0, 1, 2))
#'
#' @import ggplot2
#' @importFrom ggokabeito palette_okabe_ito
#' @importFrom rlang sym
#' @importFrom tools toTitleCase
#' @importFrom lifecycle deprecate_warn
#'
#' @keywords internal
margot_plot_hist <- function(df, col_name, n_divisions = NULL, custom_breaks = NULL,
                             cutpoint_inclusive = "upper",
                             ties.method = NULL,
                             colour_palette = NULL,
                             hist_colour = "black",
                             line_type = "solid", line_width = 0.75,
                             title = NULL, subtitle = NULL,
                             x_lab = NULL, y_lab = "Count",
                             theme_choice = theme_classic(),
                             text_size = 12, axis_text_angle = 45,
                             add_density = FALSE, add_rug = FALSE,
                             facet_var = NULL,
                             x_scale_transform = NULL, y_scale_transform = NULL,
                             additional_layers = NULL,
                             binwidth = NULL) {

  # Deprecation warning
  lifecycle::deprecate_warn(
    when = "0.2.1.39",
    what = "margot_plot_hist()",
    with = "margot_plot_histogram()",
    details = "Please use `margot_plot_categorical()` for future development."
  )

  # validate cutpoint_inclusive
  if (!cutpoint_inclusive %in% c("lower", "upper")) {
    stop("cutpoint_inclusive must be either 'lower' or 'upper'")
  }

  # remove NAs and warn the user
  original_rows <- nrow(df)
  df <- df[!is.na(df[[col_name]]), ]
  removed_rows <- original_rows - nrow(df)
  if (removed_rows > 0) {
    warning(paste(removed_rows, "rows with NA values in", col_name, "were removed."))
  }

  # use create_ordered_variable to get the breaks and labels
  result_df <- create_ordered_variable(df, col_name, n_divisions = n_divisions,
                                       custom_breaks = custom_breaks,
                                       cutpoint_inclusive = cutpoint_inclusive,
                                       ties.method = ties.method)

  # Extract the new column name (it's the original name with "_cat" appended)
  new_col_name <- paste0(col_name, "_cat")

  # Get the levels of the new categorical variable
  cat_levels <- levels(result_df[[new_col_name]])

  # Set up color palette
  if (is.null(colour_palette)) {
    okabe_ito_palette <- ggokabeito::palette_okabe_ito()
    colour_palette <- okabe_ito_palette[1:length(cat_levels)]
  } else if (length(colour_palette) < length(cat_levels)) {
    stop("The provided colour palette doesn't have enough colors for all intervals.")
  }

  # determine binwidth if not provided
  if (is.null(binwidth)) {
    binwidth <- diff(range(df[[col_name]], na.rm = TRUE)) / 30
  }

  # Create the plot
  p <- ggplot(result_df, aes(x = !!rlang::sym(col_name), fill = !!rlang::sym(new_col_name))) +
    geom_histogram(aes(y = after_stat(count)),
                   binwidth = binwidth,
                   colour = hist_colour) +
    scale_fill_manual(values = colour_palette, name = "Intervals") +
    labs(title = ifelse(is.null(title),
                        paste(tools::toTitleCase(col_name), "Histogram with Interval Highlights"),
                        title),
         subtitle = ifelse(is.null(subtitle),
                           paste("Colored regions indicate intervals."),
                           subtitle),
         x = ifelse(is.null(x_lab), tools::toTitleCase(col_name), x_lab),
         y = y_lab) +
    theme_choice +
    theme(text = element_text(size = text_size),
          axis.text.x = element_text(angle = axis_text_angle, hjust = 1))

  #  density curve if requested
  if (add_density) {
    p <- p + geom_density(alpha = 0.2, color = "black")
  }

  #  rug plot if requested
  if (add_rug) {
    p <- p + geom_rug()
  }

  #  faceting if requested
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!sym(facet_var)))
  }

  # axis transformations if requested
  if (!is.null(x_scale_transform)) {
    p <- p + scale_x_continuous(trans = x_scale_transform)
  }
  if (!is.null(y_scale_transform)) {
    p <- p + scale_y_continuous(trans = y_scale_transform)
  }

  # add any additional layers
  if (!is.null(additional_layers)) {
    for (layer in additional_layers) {
      p <- p + layer
    }
  }

  return(p)
}
