#' Plot Qini Curves from margot_multi_arm_causal_forest Results
#'
#' This function creates a ggplot object displaying Qini curves based on the
#' results of a margot_multi_arm_causal_forest() model.
#'
#' @param mc_result A list containing the results from margot_multi_arm_causal_forest().
#' @param outcome_var A character string specifying the name of the outcome variable
#'   to plot. This should match one of the model names in mc_result$results.
#'
#' @return A ggplot object representing the Qini curves for the specified outcome variable.
#'
#' @import ggplot2
#' @import ggokabeito
#'
#' @examples
#' \dontrun{
#' # Assuming mc.test is the result of margot_multi_arm_causal_forest()
#' plot_qini_curves(mc.test, "model_t2_belong_z")
#' }
#'
#' @export
margot_plot_qini <- function(mc_result, outcome_var) {
  # Extract the qini data for the specified outcome variable
  qini_data <- mc_result$results[[outcome_var]]$qini_data

  # Create the plot
  p <- ggplot(qini_data, aes(x = index, y = gain, colour = arm, linetype = arm)) +
    geom_line(linewidth = 0.5) +
    labs(
      x = "Spend",
      y = "Gain",
      title = paste("Qini Curves for", outcome_var)
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    ) +
    ggokabeito::scale_color_okabe_ito() +
    guides(colour = guide_legend(title = "Treatment Arm"),
           linetype = guide_legend(title = "Treatment Arm"))

  return(p)
}
