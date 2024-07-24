#' Plot Rank Average Treatment Effect
#'
#' This function creates a ggplot visualisation of the Rank Average Treatment Effect.
#' It displays the estimate with a confidence interval, using a simple black line
#' and light gray shading by default.
#'
#' @param x An object of class "rank_average_treatment_effect", typically the output
#'   of the rank_average_treatment_effect() function.
#' @param title Character string for the plot title. Default is "Targeting Operator Characteristic".
#' @param subtitle Character string for the plot subtitle. Default explains the confidence interval.
#' @param x_lab Character string for the x-axis label. Default is "Treated fraction (q)".
#' @param y_lab Character string for the y-axis label. Default is "Estimate".
#' @param ... Additional arguments passed to ggplot.
#'
#' @return A ggplot object that can be further customised or printed.
#'
#' @examples
#' \dontrun{
#' # Assuming rate_eval is your rank_average_treatment_effect object
#' p <- plot_rank_average_treatment_effect(rate_eval)
#' print(p)
#'
#' # Customise colors using ggokabeito
#' p_colored <- p +
#'   ggokabeito::scale_fill_okabe_ito() +
#'   ggokabeito::scale_color_okabe_ito()
#' print(p_colored)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_hline labs theme_minimal theme
#' @importFrom dplyr mutate
#'
#' @export
margot_plot_grf_rate <- function(x,
                                 title = "Targeting Operator Characteristic",
                                 subtitle = "(95% confidence interval shown as shaded area)",
                                 x_lab = "Treated fraction (q)",
                                 y_lab = "Estimate",
                                 ...) {
  TOC <- x$TOC

  # Prepare the data
  plot_data <- TOC %>%
    mutate(
      lower = estimate - 1.96 * std.err,
      upper = estimate + 1.96 * std.err
    )

  # Create the plot
  p <- ggplot(plot_data, aes(x = q, y = estimate, color = priority, fill = priority)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = y_lab
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "top"  # Place legend at the top
    ) +
    scale_color_manual(values = "black") +
    scale_fill_manual(values = "lightgray")

  #  additional customisation options
  dots <- list(...)
  if (length(dots) > 0) {
    p <- p + dots
  }

  return(p)
}
