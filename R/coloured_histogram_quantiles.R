#' visualise distribution with automatically calculated quantile highlights
#'
#' @param df dataframe containing the data to be visualised
#' @param col_name name of the column to create a histogram for
#' @param n_divisions the number of divisions to create. if null, user must provide breaks
#' @param breaks optional. a numeric vector specifying custom breakpoints
#' @param binwidth width of the bins for the histogram
#' @param n_bins optional. number of bins for the histogram. overrides binwidth if specified
#' @param cutpoint_inclusive a character string specifying whether cutpoints should be included
#'   in the lower or upper category. must be either "lower" or "upper". default is "upper"
#' @param ties.method a character string specifying how ties should be handled
#' @param colours optional. a vector of colours for the quantile lines
#' @param hist_fill colour for histogram fill. default is "lightgray"
#' @param hist_colour colour for histogram outline. default is "black"
#' @param line_type line type for quantile lines. default is "solid"
#' @param line_width line width for quantile lines. default is 0.75
#' @param title custom title for the plot. if null, a default title is used
#' @param subtitle custom subtitle for the plot. if null, a default subtitle is used
#' @param x_lab custom x-axis label. if null, the column name is used
#' @param y_lab custom y-axis label. default is "count"
#' @param theme_choice ggplot2 theme to use. default is theme_classic()
#' @param text_size base text size for the plot. default is 12
#' @param axis_text_angle angle for x-axis text. default is 45
#' @param add_density logical. if true, adds a density curve to the plot
#' @param add_rug logical. if true, adds a rug plot to the x-axis
#' @param facet_var optional. name of variable to use for faceting
#' @param x_scale_transform optional. transformation for x-axis (e.g., "log10")
#' @param y_scale_transform optional. transformation for y-axis (e.g., "log10")
#' @param additional_layers optional list of additional ggplot2 layers to add to the plot
#'
#' @return a ggplot object representing the histogram with highlighted quantiles
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom tools toTitleCase
#' @importFrom stats quantile
#' @export
coloured_histogram_quantiles <- function(df, col_name, n_divisions = NULL, breaks = NULL,
                                         binwidth = NULL, n_bins = NULL,
                                         cutpoint_inclusive = "upper",
                                         ties.method = NULL, colours = NULL,
                                         hist_fill = "lightgray", hist_colour = "black",
                                         line_type = "solid", line_width = 0.75,
                                         title = NULL, subtitle = NULL,
                                         x_lab = NULL, y_lab = "Count",
                                         theme_choice = theme_classic(),
                                         text_size = 12, axis_text_angle = 45,
                                         add_density = FALSE, add_rug = FALSE,
                                         facet_var = NULL,
                                         x_scale_transform = NULL, y_scale_transform = NULL,
                                         additional_layers = NULL) {
  # input validation
  if (!col_name %in% names(df)) {
    stop("col_name does not exist in the dataframe.")
  }
  if (all(is.na(df[[col_name]]))) {
    stop("the specified column contains only NA values.")
  }
  if (is.null(n_divisions) && is.null(breaks)) {
    stop("either n_divisions or breaks must be specified.")
  }
  if (!cutpoint_inclusive %in% c("lower", "upper")) {
    stop("invalid cutpoint_inclusive. must be either 'lower' or 'upper'.")
  }

  # set default ties.method based on cutpoint_inclusive if not specified
  if (is.null(ties.method)) {
    ties.method <- if(cutpoint_inclusive == "lower") "first" else "last"
  }
  if (!ties.method %in% c("first", "last", "random", "ordered")) {
    stop("invalid ties.method. must be one of 'first', 'last', 'random', or 'ordered'.")
  }

  # calculate breaks if not provided
  if (is.null(breaks)) {
    probs <- seq(0, 1, length.out = n_divisions + 1)
    breaks <- unique(quantile(df[[col_name]], probs = probs, na.rm = TRUE, type = 1, ties.method = ties.method))
  } else {
    n_divisions <- length(breaks) - 1
  }

  # create labels for the breaks
  break_labels <- vapply(seq_len(n_divisions), function(i) {
    left <- if (cutpoint_inclusive == "lower" || i > 1) "[" else "("
    right <- if (cutpoint_inclusive == "upper" || i < n_divisions) "]" else ")"
    sprintf("%s%.1f,%.1f%s", left, breaks[i], breaks[i+1], right)
  }, character(1))

  # create data frame for v-lines
  line_data <- data.frame(
    value = breaks[-c(1, length(breaks))],
    label = break_labels[-length(break_labels)],
    colour = if (is.null(colours)) rainbow(n_divisions - 1) else colours
  )

  # create the plot
  p <- ggplot(df, aes(x = !!rlang::sym(col_name))) +
    geom_histogram(aes(y = after_stat(count)),
                   binwidth = binwidth,
                   bins = n_bins,
                   fill = hist_fill,
                   colour = hist_colour) +
    geom_vline(data = line_data,
               aes(xintercept = value),
               colour = line_data$colour,
               linewidth = line_width,
               linetype = line_type) +
    labs(title = ifelse(is.null(title),
                        paste(tools::toTitleCase(col_name), "Histogram with Quantile Highlights"),
                        title),
         subtitle = ifelse(is.null(subtitle),
                           "Vertical lines indicate the quantile divisions.",
                           subtitle),
         x = ifelse(is.null(x_lab), tools::toTitleCase(col_name), x_lab),
         y = y_lab) +
    theme_choice +
    theme(text = element_text(size = text_size),
          axis.text.x = element_text(angle = axis_text_angle, hjust = 1))

  # add cutpoint labels to x-axis
  p <- p + scale_x_continuous(
    breaks = breaks,
    labels = function(x) sprintf("%.2f", x),
    sec.axis = dup_axis(name = NULL, labels = NULL)
  )

  # add density curve if requested
  if (add_density) {
    p <- p + geom_density(alpha = 0.2)
  }

  # add rug plot if requested
  if (add_rug) {
    p <- p + geom_rug()
  }

  # add faceting if requested
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!sym(facet_var)))
  }

  # apply axis transformations if requested
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
