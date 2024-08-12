#' Plot Policy Tree Results
#'
#' This function creates a visualization of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates two plots showing the relationships between the top three split variables,
#' with points colored by the predicted optimal treatment.
#'
#' @param mc_test A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to plot.
#' @param color_scale An optional ggplot2 color scale. If NULL (default), a color scale based on the Okabe-Ito palette is created,
#'        with blue for the first level, orange for the second, and subsequent colors for additional levels.
#' @param point_alpha Numeric value between 0 and 1 for point transparency. Default is 0.5.
#' @param theme_function A ggplot2 theme function. Default is theme_classic.
#' @param title_size Numeric value for the size of the plot title. Default is 16.
#' @param subtitle_size Numeric value for the size of the plot subtitle. Default is 16.
#' @param axis_title_size Numeric value for the size of axis titles. Default is 16.
#' @param legend_title_size Numeric value for the size of the legend title. Default is 10.
#' @param jitter_width Numeric value for the amount of horizontal jitter. Default is 0.3.
#' @param jitter_height Numeric value for the amount of vertical jitter. Default is 0.3.
#' @param split_line_color Color of the split lines. Default is "darkgray".
#' @param split_line_alpha Numeric value between 0 and 1 for split line transparency. Default is 0.7.
#' @param split_line_type Line type for split lines. Default is "dashed".
#' @param split_line_linewidth Numeric value for the thickness of split lines. Default is 0.5.
#' @param split_label_size Numeric value for the size of split value labels. Default is 10.
#' @param split_label_color Color of split value labels. Default is "darkgray".
#' @param custom_action_names Optional vector of custom names for the actions. Must match the number of actions in the policy tree.
#' @param legend_position String specifying the position of the legend. Can be "top", "bottom", "left", or "right". Default is "bottom".
#' @param plot_selection String specifying which plots to display: "both", "p1", or "p2". Default is "both".
#'
#' @return A ggplot object containing the specified plot(s) of the policy tree results.
#'
#' @import ggplot2
#' @import patchwork
#'
#' @examples
#' \dontrun{
#' # Default (both plots, legend at bottom)
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z")
#'
#' # Only the first plot (p1)
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z", plot_selection = "p1")
#'
#' # Both plots with legend on the right
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z", legend_position = "right")
#'
#' # Custom color scale
#' custom_scale <- scale_colour_manual(values = c("red", "green", "blue"))
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z", color_scale = custom_scale)
#' }
#'
#' @export
margot_plot_policy_tree <- function(mc_test, model_name,
                                    color_scale = NULL,  # We'll set this inside the function
                                    point_alpha = 0.5,
                                    theme_function = theme_classic,
                                    title_size = 16,
                                    subtitle_size = 16,
                                    axis_title_size = 16,
                                    legend_title_size = 12,
                                    jitter_width = 0.3,
                                    jitter_height = 0.3,
                                    split_line_color = "darkgray",
                                    split_line_alpha = 0.7,
                                    split_line_type = "dashed",
                                    split_line_linewidth = 0.5,
                                    split_label_size = 10,
                                    split_label_color = "darkgray",
                                    custom_action_names = NULL,
                                    legend_position = "bottom",
                                    plot_selection = "both") {

  # Extract policy tree object
  policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2

  if (is.null(policy_tree_obj)) {
    stop("Policy tree object not found for the specified model name.")
  }

  # Extract action names from the policy tree object
  action_names <- policy_tree_obj$action.names

  # If custom action names are provided, use them instead
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != length(action_names)) {
      stop("The number of custom action names must match the number of actions in the policy tree.")
    }
    action_names <- custom_action_names
  }

  # Extract policy tree object
  policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2

  if (is.null(policy_tree_obj)) {
    stop("Policy tree object not found for the specified model name.")
  }

  # Extract action names and number of actions from the policy tree object
  action_names <- policy_tree_obj$action.names
  n_actions <- policy_tree_obj$n.actions

  # If custom action names are provided, use them instead
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != n_actions) {
      stop("The number of custom action names must match the number of actions in the policy tree.")
    }
    action_names <- custom_action_names
  }

  # Define the Okabe-Ito palette, starting with blue and orange
  okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

  # If color_scale is not provided, create it based on the number of actions
  if (is.null(color_scale)) {
    # Create a named vector of colors
    color_vector <- setNames(okabe_ito_palette[1:n_actions], action_names)

    color_scale <- scale_colour_manual(values = color_vector)
  }


  # Extract the plot data for the specified model
  plot_data <- mc_test$results[[model_name]]$plot_data

  # Extract X_test, predictions, and split_variables
  X_test <- plot_data$X_test
  predictions <- plot_data$predictions

  # Extract split variables and their values from the policy tree
  policy_tree <- policy_tree_obj$nodes
  x1_col <- policy_tree[[1]]$split_variable
  x2_col <- policy_tree[[2]]$split_variable
  x3_col <- policy_tree[[3]]$split_variable

  # Extract split values
  x1_split <- policy_tree[[1]]$split_value
  x2_split <- policy_tree[[2]]$split_value
  x3_split <- policy_tree[[3]]$split_value

  # Use the actual column names from X_test
  actual_columns <- names(X_test)

  # Create a data frame for plotting
  plot_df <- data.frame(
    x1 = X_test[[actual_columns[x1_col]]],
    x2 = X_test[[actual_columns[x2_col]]],
    x3 = X_test[[actual_columns[x3_col]]],
    prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
  )

  # Function to create a secondary axis with split value
  create_secondary_axis <- function(primary_range, split_value) {
    secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
    secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
    sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
  }

  # Create the base plot
  base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split) {
    p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
      geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
      geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
                 linetype = split_line_type, linewidth = split_line_linewidth) +
      geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
                 linetype = split_line_type, linewidth = split_line_linewidth) +
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
        legend.title = element_text(size = legend_title_size),
        axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
        axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
      )

    # Add secondary axes with split values
    p <- p +
      scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split)) +
      scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split))

    return(p)
  }

  # Create the individual plots based on plot_selection
  p1 <- if (plot_selection %in% c("both", "p1")) {
    base_plot("x1", "x2", actual_columns[x1_col], actual_columns[x2_col],
              paste(actual_columns[x1_col], "vs", actual_columns[x2_col]),
              x1_split, x2_split)
  } else {
    NULL
  }

  p2 <- if (plot_selection %in% c("both", "p2")) {
    base_plot("x1", "x3", actual_columns[x1_col], actual_columns[x3_col],
              paste(actual_columns[x1_col], "vs", actual_columns[x3_col]),
              x1_split, x3_split)
  } else {
    NULL
  }

  # Combine the plots based on plot_selection
  if (plot_selection == "both") {
    combined_plot <- p1 + p2 +
      patchwork::plot_layout(guides = "collect") &
      patchwork::plot_annotation(
        title = paste("Policy Tree Results for", model_name),
        tag_levels = 'A'
      ) &
      theme(
        plot.tag = element_text(size = 12, face = "bold"),
        legend.position = legend_position
      )
  } else if (plot_selection == "p1") {
    combined_plot <- p1 +
      patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 1")) &
      theme(legend.position = legend_position)
  } else if (plot_selection == "p2") {
    combined_plot <- p2 +
      patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 2")) &
      theme(legend.position = legend_position)
  } else {
    stop("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
  }

  # Return the combined plot
  return(combined_plot)
}
# margot_plot_policy_tree <- function(mc_test, model_name,
#                                     color_scale = NULL,  # We'll set this inside the function
#                                     point_alpha = 0.5,
#                                     theme_function = theme_classic,
#                                     title_size = 14,
#                                     subtitle_size = 12,
#                                     axis_title_size = 10,
#                                     legend_title_size = 10,
#                                     jitter_width = 0.3,
#                                     jitter_height = 0.3,
#                                     split_line_color = "darkgray",
#                                     split_line_alpha = 0.7,
#                                     split_line_type = "dashed",
#                                     split_line_linewidth = 0.5,
#                                     split_label_size = 10,
#                                     split_label_color = "darkgray",
#                                     custom_action_names = NULL,
#                                     legend_position = "bottom",
#                                     plot_selection = "both") {
#
#   # Extract policy tree object
#   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#
#   if (is.null(policy_tree_obj)) {
#     stop("Policy tree object not found for the specified model name.")
#   }
#
#   # Extract action names from the policy tree object
#   action_names <- policy_tree_obj$action.names
#
#   # If custom action names are provided, use them instead
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != length(action_names)) {
#       stop("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#   }
#
#   # Define the Okabe-Ito palette, starting with blue and orange
#   okabe_ito_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#
#
#   # If color_scale is not provided, create it based on the number of action_names
#   if (is.null(color_scale)) {
#     num_colors_needed <- length(action_names)
#     colors_to_use <- okabe_ito_palette[1:min(num_colors_needed, length(okabe_ito_palette))]
#
#     # If we need more colors than in the palette, recycle them
#     if (num_colors_needed > length(okabe_ito_palette)) {
#       colors_to_use <- rep_len(colors_to_use, num_colors_needed)
#     }
#
#     color_scale <- scale_colour_manual(values = colors_to_use)
#   }
#
#
#   # Extract the plot data for the specified model
#   plot_data <- mc_test$results[[model_name]]$plot_data
#
#   # Extract X_test, predictions, and split_variables
#   X_test <- plot_data$X_test
#   predictions <- plot_data$predictions
#
#   # Extract split variables and their values from the policy tree
#   policy_tree <- policy_tree_obj$nodes
#   x1_col <- policy_tree[[1]]$split_variable
#   x2_col <- policy_tree[[2]]$split_variable
#   x3_col <- policy_tree[[3]]$split_variable
#
#   # Extract split values
#   x1_split <- policy_tree[[1]]$split_value
#   x2_split <- policy_tree[[2]]$split_value
#   x3_split <- policy_tree[[3]]$split_value
#
#   # Use the actual column names from X_test
#   actual_columns <- names(X_test)
#
#   # Create a data frame for plotting
#   plot_df <- data.frame(
#     x1 = X_test[[actual_columns[x1_col]]],
#     x2 = X_test[[actual_columns[x2_col]]],
#     x3 = X_test[[actual_columns[x3_col]]],
#     prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
#   )
#
#   # Function to create a secondary axis with split value
#   create_secondary_axis <- function(primary_range, split_value) {
#     secondary_breaks <- c(min(primary_range), split_value, max(primary_range))
#     secondary_labels <- c("", sprintf("Split: %.2f", split_value), "")
#     sec_axis(~ ., breaks = secondary_breaks, labels = secondary_labels)
#   }
#
#   # Create the base plot
#   base_plot <- function(x, y, x_lab, y_lab, subtitle, x_split, y_split) {
#     p <- ggplot(plot_df, aes(x = .data[[x]], y = .data[[y]], color = prediction)) +
#       geom_jitter(alpha = point_alpha, width = jitter_width, height = jitter_height) +
#       geom_vline(xintercept = x_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, linewidth = split_line_linewidth) +
#       color_scale +
#       labs(
#         x = x_lab,
#         y = y_lab,
#         subtitle = subtitle
#       ) +
#       theme_function() +
#       theme(
#         plot.title = element_text(size = title_size),
#         plot.subtitle = element_text(size = subtitle_size),
#         axis.title = element_text(size = axis_title_size),
#         legend.title = element_text(size = legend_title_size),
#         axis.text.x.top = element_text(size = split_label_size, color = split_label_color),
#         axis.text.y.right = element_text(size = split_label_size, color = split_label_color)
#       )
#
#     # Add secondary axes with split values
#     p <- p +
#       scale_x_continuous(sec.axis = create_secondary_axis(range(plot_df[[x]]), x_split)) +
#       scale_y_continuous(sec.axis = create_secondary_axis(range(plot_df[[y]]), y_split))
#
#     return(p)
#   }
#
#   # Create the individual plots based on plot_selection
#   p1 <- if (plot_selection %in% c("both", "p1")) {
#     base_plot("x1", "x2", actual_columns[x1_col], actual_columns[x2_col],
#               paste(actual_columns[x1_col], "vs", actual_columns[x2_col]),
#               x1_split, x2_split)
#   } else {
#     NULL
#   }
#
#   p2 <- if (plot_selection %in% c("both", "p2")) {
#     base_plot("x1", "x3", actual_columns[x1_col], actual_columns[x3_col],
#               paste(actual_columns[x1_col], "vs", actual_columns[x3_col]),
#               x1_split, x3_split)
#   } else {
#     NULL
#   }
#
#   # Combine the plots based on plot_selection
#   if (plot_selection == "both") {
#     combined_plot <- p1 + p2 +
#       patchwork::plot_layout(guides = "collect") &
#       patchwork::plot_annotation(
#         title = paste("Policy Tree Results for", model_name),
#         tag_levels = 'A'
#       ) &
#       theme(
#         plot.tag = element_text(size = 12, face = "bold"),
#         legend.position = legend_position
#       )
#   } else if (plot_selection == "p1") {
#     combined_plot <- p1 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 1")) &
#       theme(legend.position = legend_position)
#   } else if (plot_selection == "p2") {
#     combined_plot <- p2 +
#       patchwork::plot_annotation(title = paste("Policy Tree Results for", model_name, "- Plot 2")) &
#       theme(legend.position = legend_position)
#   } else {
#     stop("Invalid plot_selection. Choose 'both', 'p1', or 'p2'.")
#   }
#
#   # Return the combined plot
#   return(combined_plot)
# }
