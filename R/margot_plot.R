#' Visualise Causal Effect Estimates with E-Values
#'
#' @description Creates a graph of multiple causal effect estimates, organised by effect size. It is designed to
#'   work with the output of the `group_tab` function, allowing for visual comparison across estimates
#'   categorized as "positive", "negative", or "zero_crossing" based on their reliability and effect size.
#'   Supports both risk difference (RD) and risk ratio (RR) scales.
#'
#' @param .data Data frame output from `group_tab` function containing categorized causal effect estimates.
#' @param type Character string indicating the scale of effect estimates to be visualised: "RD" for risk difference
#'   or "RR" for risk ratio. Default is "RD".
#' @param title The main title for the plot.
#' @param subtitle The subtitle for the plot.
#' @param xlab Label for the x-axis, typically denoting the scale or measurement of effect estimates.
#' @param ylab Label for the y-axis, generally left blank as outcomes are used as labels.
#' @param estimate_scale Numeric value for adjusting the text offset of estimate labels. Default is 1.
#' @param base_size Numeric value for the base font size in the plot. Default is 11.
#' @param text_size Numeric value for the size of estimate labels. Default is 2.75.
#' @param point_size Numeric value for the size of points representing the estimates. Default is 0.5.
#' @param title_size Numeric value for the font size of the plot title. Default is 10.
#' @param subtitle_size Numeric value for the font size of the plot subtitle. Default is 9.
#' @param legend_text_size Numeric value for the font size of the legend text. Default is 6.
#' @param legend_title_size Numeric value for the font size of the legend title. Default is 6.
#' @param x_offset Numeric value for horizontal adjustment of estimate labels. Adjusts based on `type`.
#' @param x_lim_lo Numeric value for the lower limit of the x-axis. Adjusts based on `type`.
#' @param x_lim_hi Numeric value for the upper limit of the x-axis. Adjusts based on `type`.
#'
#' @return A ggplot object visualising the causal effect estimates with error bars and categorization labels,
#'   ready for further modification or saving.
#'
#' @examples
#'  \dontrun{
#' title <- "Religious Service At Least Once Per Week vs None"
#'
#' # Plotting perceived social support with Risk Difference (RD) scale
#' plot_group_tab_all_perceived_support <- margot_plot(
#'   group_tab_all_perceived_support,
#'   type = "RD",
#'   title = title,
#'   subtitle = "Perceived Social Support",
#'   xlab = "",
#'   ylab = "",
#'   estimate_scale = 1,
#'   base_size = 18,
#'   text_size = 4.5,
#'   point_size = 3.5,
#'   title_size = 20,
#'   subtitle_size = 16,
#'   legend_text_size = 10,
#'   legend_title_size = 10,
#'   x_offset = -.5,
#'   x_lim_lo = -.5,
#'   x_lim_hi =  .5
#' )
#'
#' # Plot for support received from others with Risk Ratio (RR) scale
#' plot_group_tab_all_received_time <- margot_plot(
#'   group_tab_contrast_received_time,
#'   type = "RR",
#'   title = title,
#'   subtitle = "Support Received From Others: Time",
#'   xlab = "",
#'   ylab = "",
#'   estimate_scale = 1,
#'   base_size = 18,
#'   text_size = 4.5,
#'   point_size = 3.5,
#'   title_size = 20,
#'   subtitle_size = 16,
#'   legend_text_size = 10,
#'   legend_title_size = 10,
#'   x_offset = 0,
#'   x_lim_lo = 0,
#'   x_lim_hi =  4
#' )
#'
#' print(plot_group_tab_all_perceived_support)
#' print(plot_group_tab_all_received_time)
#' }
#' @export
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline theme_classic scale_color_manual labs geom_text coord_cartesian theme element_text margin
#' @importFrom tibble rownames_to_column
#' @import tibble dplyr
margot_plot <- function(.data,
                        type = c("RD", "RR"),
                        title,
                        subtitle,
                        xlab,
                        ylab,
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
                        x_lim_hi = ifelse(type == "RR", 2.5, 1)) {
  type <- match.arg(type)
  xintercept <- if (type == "RR")
    1
  else
    0
  x_axis_label <-
    if (type == "RR")
      "Causal Risk Ratio Scale"
  else
    "Causal Difference Scale"

  # Define Reliability based on type
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

  out <- ggplot(
    data = .data,
    aes(
      y = reorder(outcome, .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]]),
      x = .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]],
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      group = Estimate,
      color = Reliability
    )
  ) +
    geom_errorbarh(aes(color = Reliability),
                   height = .3,
                   position = position_dodge(width = 0.3)) +
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
      y = " ",
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
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      plot.title = element_text(
        face = "bold",
        size = title_size,
        hjust = 0
      ),
      plot.subtitle = element_text(
        face = "bold",
        size = subtitle_size,
        hjust = 0
      ),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      plot.margin = margin(
        t = 10,
        r = 10,
        b = 10,
        l = 10,
        unit = "pt"
      )
    )


  return(out)
}
