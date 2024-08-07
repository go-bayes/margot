#' Visualise Causal Effect Estimates with Enhanced Flexibility
#'
#' @description This function renders a graphical representation of causal effect estimates, organised by effect size on either the risk difference (RD) or risk ratio (RR) scale. It categorises estimates into "positive", "negative", or "zero_crossing" based on their confidence intervals. The function features advanced customisability for graphical parameters, including error bar width and the application of custom ggplot2 themes. Notably, it avoids direct manipulation of the input data and implements an advanced labelling strategy for the x-axis to prevent incoherent negative values for RR.
#'
#' @param .data A data frame of causal effect estimates, ideally output by a `group_tab` function or similar. The data should include confidence intervals and effect estimate values.
#' @param type Character string indicating the scale of effect estimates: "RD" for risk difference or "RR" for risk ratio, with "RD" as the default.
#' @param order Character string indicating the ordering of the output: "default" for default descending, "alphabetical" for alphabetical, and "custom" for custom order provided in .data.
#' @param custom_order Optional vector specifying custom ordering if 'order' is set to 'custom'.
#' @param title Main title for the plot.
#' @param subtitle Subtitle for the plot.
#' @param estimate_scale Numeric multiplier to adjust the horizontal offset of estimate labels, aiding in plot clarity. Default is 1.
#' @param base_size Base font size for the plot, applied globally unless overridden. Default is 11.
#' @param text_size Font size for the estimate labels. Default is 2.75.
#' @param point_size Size of points representing the estimates. Default is 0.5.
#' @param title_size Font size for the plot title. Default is 10.
#' @param subtitle_size Font size for the plot subtitle. Default is 9.
#' @param legend_text_size Font size for legend text. Default is 6.
#' @param legend_title_size Font size for legend titles. Default is 6.
#' @param x_offset Horizontal adjustment for estimate labels, varied based on `type`. Default adjustments are 0 for "RR" and -1.75 for "RD".
#' @param x_lim_lo Lower limit of the x-axis, automatically adjusted based on `type`.
#' @param x_lim_hi Upper limit of the x-axis, automatically adjusted based on `type`.
#' @param linewidth Width of the error bars in the plot. Default is 0.5.
#' @param plot_theme ggplot2 theme object for customising plot appearance. Inherits `base_size` from `base_size` parameter to maintain consistency. Uses `theme_classic()` as default but allows for customisation.
#'
#' @return A ggplot object displaying the causal effect estimates with categorisation and error bars. This plot is tailored for further modifications or direct usage.
#' @export
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline scale_color_manual labs geom_text coord_cartesian theme element_text margin
#' @importFrom rlang .data
#' @import dplyr
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        order = c("default", "alphabetical"),
                        title = NULL,
                        subtitle = NULL,
                        estimate_scale = 1,
                        base_size = 11,
                        text_size = 2.75,
                        point_size = .5,
                        title_size = 10,
                        subtitle_size = 9,
                        legend_text_size = 6,
                        legend_title_size = 6,
                        x_offset = ifelse(type == "RR", 0, -1.75),
                        x_lim_lo = ifelse(type == "RR", .1, -1.75),
                        x_lim_hi = ifelse(type == "RR", 2.5, 1),
                        linewidth = .5,
                        plot_theme = NULL) {
  require("ggplot2")
  require("dplyr")

  type <- match.arg(type)
  order <- match.arg(order)

  # Check if the data needs processing by group_tab
  if (!"Estimate" %in% names(.data) || !"outcome" %in% names(.data)) {
    .data <- group_tab(.data, type = type, order = order)
  }

  # Dynamic theme adjustment
  if (is.null(plot_theme)) {
    plot_theme <- theme_classic(base_size = base_size)
  } else {
    plot_theme <- plot_theme + theme(text = element_text(size = base_size))
  }

  # Prepare the data for plotting, including ordering
  effect_size_col <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"
  .data <- .data %>%
    mutate(outcome = factor(outcome, levels = if (order == "alphabetical") sort(unique(outcome)) else unique(outcome))) %>%
    arrange(if (order == "alphabetical") outcome else desc(!!sym(effect_size_col)))

  # Start building the plot
  out <- ggplot(
    data = .data,
    aes(
      y = outcome,
      x = !!sym(effect_size_col),
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      color = Estimate
    )
  ) + geom_errorbarh(aes(color = Estimate), height = .3,
                     linewidth = linewidth, position = position_dodge(width = .3)) +
    geom_point(size = point_size, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
    scale_color_manual(values = c("positive" = "dodgerblue", "not reliable" = "black", "negative" = "orange")) +
    labs(
      x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"),
      y = NULL,
      title = ifelse(is.null(title), "", title),
      subtitle = ifelse(is.null(subtitle), "", subtitle)
    ) +
    geom_text(aes(x = x_offset * estimate_scale, label = estimate_lab), size = text_size, hjust = 0, fontface = "bold") +
    coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
    plot_theme +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = title_size, hjust = 0),
      plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )

  # Conditionally add x-axis scale modifications for RR
  if (type == "RR") {
    custom_x_labels <- function(x) {
      ifelse(x < 0, "", as.character(x))
    }
    out <- out + scale_x_continuous(labels = custom_x_labels)
  }

  return(out)
}
# margot_plot <- function(.data,
#                         type = c("RD", "RR"),
#                         order = c("default", "alphabetical"),
#                         title,
#                         subtitle,
#                         estimate_scale = 1,
#                         base_size = 11,
#                         text_size = 2.75,
#                         point_size = .5,
#                         title_size = 10,
#                         subtitle_size = 9,
#                         legend_text_size = 6,
#                         legend_title_size = 6,
#                         x_offset = ifelse(type == "RR", 0, -1.75),
#                         x_lim_lo = ifelse(type == "RR", .1, -1.75),
#                         x_lim_hi = ifelse(type == "RR", 2.5, 1),
#                         linewidth = .5,
#                         plot_theme = NULL) {
#   require("ggplot2")
#   require("dplyr")
#
#   type <- match.arg(type)
#   order <- match.arg(order)
#
#   # Check if the data needs processing by group_tab
#   if (!"Estimate" %in% names(.data) || !"outcome" %in% names(.data)) {
#     .data <- group_tab(.data, type = type, order = order)
#   }
#
#
#   # Dynamic theme adjustment
#   if (is.null(plot_theme)) {
#     plot_theme <- theme_classic(base_size = base_size)
#   } else {
#     plot_theme <- plot_theme + theme(text = element_text(size = base_size))
#   }
#
#   # Prepare the data for plotting, including ordering
#   effect_size_col <- if (type == "RR") "E[Y(1)]/E[Y(0)]" else "E[Y(1)]-E[Y(0)]"
#   .data <- .data %>%
#     mutate(outcome = factor(outcome, levels = if (order == "alphabetical") sort(unique(outcome)) else unique(outcome))) %>%
#     arrange(if (order == "alphabetical") outcome else desc(!!sym(effect_size_col)))
#
#   # Start building the plot
#   out <- ggplot(
#     data = .data,
#     aes(
#       y = outcome,
#       x = !!sym(effect_size_col),
#       xmin = `2.5 %`,
#       xmax = `97.5 %`,
#       color = Estimate
#     )
#   ) + geom_errorbarh(aes(color = Estimate), height = .3,
#                      linewidth = linewidth, position = position_dodge(width = .3)) +
#     geom_point(size = point_size, position = position_dodge(width = 0.3)) +
#     geom_vline(xintercept = if(type == "RR") 1 else 0, linetype = "solid") +
#     scale_color_manual(values = c("positive" = "dodgerblue", "not reliable" = "black", "negative" = "orange")) +
#     labs(x = paste0("Causal ", ifelse(type == "RR", "risk ratio", "difference"), " scale"), y = NULL, title = title, subtitle = subtitle) +
#     geom_text(aes(x = x_offset * estimate_scale, label = estimate_lab), size = text_size, hjust = 0, fontface = "bold") +
#     coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
#     plot_theme +
#     theme(
#       legend.position = "top",
#       legend.direction = "horizontal",
#       axis.ticks.x = element_blank(),
#       axis.ticks.y = element_blank(),
#       plot.title = element_text(face = "bold", size = title_size, hjust = 0),
#       plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
#       legend.text = element_text(size = legend_text_size),
#       legend.title = element_text(size = legend_title_size),
#       plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
#     )
#
#   # Conditionally add x-axis scale modifications for RR
#   if (type == "RR") {
#     custom_x_labels <- function(x) {
#       ifelse(x < 0, "", as.character(x))
#     }
#     out <- out + scale_x_continuous(labels = custom_x_labels)
#   }
#
#   return(out)
# }
