#' Visualise Causal Effect Estimates with E-Values
#'
#' @description Creates a graph to visualise multiple causal effect estimates, organised by effect size,
#' on either risk difference (RD) or risk ratio (RR) scales. It categorises estimates as "positive",
#' "negative", or "zero_crossing" based on their confidence intervals. This function is flexible,
#' allowing customisation of graphical parameters and axis simplification.
#'
#' @param .data Data frame containing causal effect estimates outputtd by the `group_tab` function.
#' @param type Character string indicating the scale of effect estimates: "RD" for risk difference
#'   or "RR" for risk ratio. Defaults to "RD".
#' @param title Main title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param ylab Y-axis label, typically left blank as outcomes serve as labels.
#' @param estimate_scale Scale factor for estimate label offsets. Default is 1.
#' @param base_size Base font size in the plot. Default is 11.
#' @param text_size Font size for estimate labels. Default is 2.75.
#' @param point_size Size of the points representing estimates. Default is 0.5.
#' @param title_size Font size for the plot title. Default is 10.
#' @param subtitle_size Font size for the subtitle. Default is 9.
#' @param legend_text_size Font size for legend text. Default is 6.
#' @param legend_title_size Font size for legend titles. Default is 6.
#' @param x_offset Horizontal adjustment of estimate labels, varies by `type`.
#' @param x_lim_lo Lower limit of the x-axis, adjusted based on `type`.
#' @param x_lim_hi Upper limit of the x-axis, adjusted based on `type`.
#' @param linewidth Width of the error bars. Default is 0.5.
#' @param simplify_axis Boolean to simplify axis for a cleaner look. Default is FALSE.
#'
#' @return A `ggplot` object visualising causal effect estimates with error bars and categorisation labels.
#'
#' @examples
#' \dontrun{
#' title <- "Religious Service At Least Once Per Week vs None"
#'
#' # Example using Risk Difference (RD) scale
#' plot_example_rd <- margot_plot(
#'   data = your_data_frame, # replace your_data_frame output of group_tab() function
#'   type = "RD",
#'   title = title,
#'   subtitle = "subtitle here",
#'   estimate_scale = 1,
#'   linewidth = 0.5,
#'   simplify_axis = TRUE,
#'   base_size = 11,
#'   text_size = 2.75,
#'   point_size = 0.5,
#'   title_size = 10,
#'   subtitle_size = 9,
#'   legend_text_size = 6,
#'   legend_title_size = 6,
#'   x_offset = -1.75,
#'   x_lim_lo = -1.75,
#'   x_lim_hi = 1
#' )
#' print(plot_example_rd)
#' }
#' @export
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline theme_classic scale_color_manual labs geom_text coord_cartesian theme element_text margin
#' @importFrom tibble rownames_to_column
#' @import tibble dplyr
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        title,
                        subtitle,
                        ylab = "",
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
                        simplify_axis = FALSE) {
  type <- match.arg(type)
  xintercept <- if (type == "RR") 1 else 0
  x_axis_label <- if (type == "RR") "Causal risk ratio scale (vertical line marks threshold: risk ratio = 1)" else "Causal difference scale (vertical line marks threshold: difference = 0)"

  # define reliability based on type
  if (type == "RR") {
    .data$Reliability <-
      ifelse(
        .data$`2.5 %` > 1 & .data$`97.5 %` > 1,
        "positive",
        ifelse(
          .data$`2.5 %` < 1 &
            .data$`97.5 %` < 1,
          "negative",
          "zero_crossing"
        )
      )
  } else {
    .data$Reliability <-
      ifelse(
        .data$`2.5 %` > 0 & .data$`97.5 %` > 0,
        "positive",
        ifelse(
          .data$`2.5 %` < 0 &
            .data$`97.5 %` < 0,
          "negative",
          "zero_crossing"
        )
      )
  }
  # add a condition to apply axis simplifications
  if (simplify_axis) {
    simplified_theme <- theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = title_size, hjust = 0),
      plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
  } else {
    simplified_theme <- theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold", size = title_size, hjust = 0),
      plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
  }

  out <- ggplot(
    data = .data,
    aes(
      y = reorder(outcome, .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]]),
      x = .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]],
      xmin = .data$`2.5 %`,
      xmax = .data$`97.5 %`,
      group = Estimate,
      color = Reliability
    )
  ) +
    geom_errorbarh(aes(color = Reliability), height = .3, linewidth = linewidth,
                   position = position_dodge(width = .3)) +
    geom_point(size = point_size, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = xintercept, linetype = "solid") +
    theme_classic(base_size = base_size) +
    scale_color_manual(values = c(
      "positive" = "dodgerblue",
      "zero_crossing" = "black",
      "negative" = "orange"
    )) +
    labs(
      x = x_axis_label,
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    geom_text(
      aes(x = x_offset * estimate_scale, label = estimate_lab),
      size = text_size,
      hjust = 0,
      fontface = ifelse(.data$Estimate == "unreliable", "plain", "bold")
    ) +
    coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
    # selected theme option
    simplified_theme



  return(out)
}
