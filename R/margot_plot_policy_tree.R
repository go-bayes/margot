#' Plot Policy Tree Results
#'
#' This function creates a visualization of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates two plots showing the relationships between the top three split variables,
#' with points colored by the predicted optimal treatment.
#'
#' @param mc_test A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to plot.
#' @param color_scale A ggplot2 color scale. Default is ggokabeito::scale_colour_okabe_ito().
#' @param point_alpha Numeric value between 0 and 1 for point transparency. Default is 0.6.
#' @param theme_function A ggplot2 theme function. Default is theme_classic.
#' @param title_size Numeric value for the size of the plot title. Default is 14.
#' @param subtitle_size Numeric value for the size of the plot subtitle. Default is 12.
#' @param axis_title_size Numeric value for the size of axis titles. Default is 10.
#' @param legend_title_size Numeric value for the size of the legend title. Default is 10.
#' @param jitter_width Numeric value for the amount of horizontal jitter. Default is 0.3.
#' @param jitter_height Numeric value for the amount of vertical jitter. Default is 0.3.
#'
#' @return A ggplot object containing two side-by-side plots of the policy tree results.
#'
#' @examples
#' plot <- plot_policy_tree_results(mc_test, "model_t2_belong_z")
#' print(plot)
#'
#' @import ggplot2
#' @import patchwork
#' @import ggokabeito
#'
#' @export
margot_plot_policy_tree <- function(mc_test, model_name,
                                     color_scale = ggokabeito::scale_colour_okabe_ito(),
                                     point_alpha = 0.25,
                                     theme_function = theme_classic,
                                     title_size = 14,
                                     subtitle_size = 12,
                                     axis_title_size = 10,
                                     legend_title_size = 10,
                                     jitter_width = 0.3,
                                     jitter_height = 0.3) {
  # Extract the plot data for the specified model
  plot_data <- mc_test$results[[model_name]]$plot_data

  # Extract X_test, predictions, and split_variables
  X_test <- plot_data$X_test
  predictions <- plot_data$predictions
  split_variables <- plot_data$split_variables

  # Use the actual column names from X_test
  actual_columns <- names(X_test)

  # Create a data frame for plotting
  plot_df <- data.frame(
    x1 = X_test[[actual_columns[1]]],
    x2 = X_test[[actual_columns[2]]],
    x3 = X_test[[actual_columns[3]]],
    prediction = as.factor(predictions - 1)  # Subtract 1 to match the legend in your example
  )

  # Create the base plot
  base_plot <- function(x, y, x_lab, y_lab, subtitle) {
    ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
      geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
      color_scale +
      labs(
        x = x_lab,
        y = y_lab,
        subtitle = subtitle
      ) +
      theme_function() +
      theme(
        plot.title = element_text(size = title_size),
        plot.subtitle = element_text(size = subtitle_size),
        axis.title = element_text(size = axis_title_size),
        legend.title = element_text(size = legend_title_size)
      )
  }

  # Create the first plot
  p1 <- base_plot("x1", "x2", actual_columns[1], actual_columns[2],
                  paste(actual_columns[1], "vs", actual_columns[2])) +
    labs(title = paste("Policy Tree Results for", model_name)) +
    theme(legend.position = "top")

  # Create the second plot
  p2 <- base_plot("x2", "x3", actual_columns[2], actual_columns[3],
                  paste(actual_columns[2], "vs", actual_columns[3])) +
    theme(legend.position = "none")

  # Combine the plots using patchwork
  combined_plot <- p1 + p2 +
    plot_layout(ncol = 2, widths = c(1.2, 1))

  return(combined_plot)
}