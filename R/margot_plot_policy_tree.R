#' Plot Policy Tree Results
#'
#' This function creates a visualization of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates two plots showing the relationships between the top three split variables,
#' with points colored by the predicted optimal treatment.
#'
#' @param mc_test A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to plot.
#' @param color_scale A ggplot2 color scale. Default is ggokabeito::scale_colour_okabe_ito().
#' @param point_alpha Numeric value between 0 and 1 for point transparency. Default is 0.25.
#' @param theme_function A ggplot2 theme function. Default is theme_classic.
#' @param title_size Numeric value for the size of the plot title. Default is 14.
#' @param subtitle_size Numeric value for the size of the plot subtitle. Default is 12.
#' @param axis_title_size Numeric value for the size of axis titles. Default is 10.
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
#'
#' @return A ggplot object containing two side-by-side plots of the policy tree results.
#'
#' @import ggplot2
#' @import patchwork
#' @import ggokabeito
#'
#' @examples
#' \dontrun{
#' plot <- margot_plot_policy_tree(mc_test, "model_t2_belong_z")
#' print(plot)
#' }
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
                                    jitter_height = 0.3,
                                    split_line_color = "darkgray",
                                    split_line_alpha = 0.7,
                                    split_line_type = "dashed",
                                    split_line_linewidth = 0.5,  # Changed from split_line_size
                                    split_label_size = 10,
                                    split_label_color = "darkgray",
                                    custom_action_names = NULL) {

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
                 linetype = split_line_type, linewidth = split_line_linewidth) +  # Updated to split_line_linewidth
      geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
                 linetype = split_line_type, linewidth = split_line_linewidth) +  # Updated to split_line_linewidth
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

  # Create the first plot
  p1 <- base_plot("x1", "x2", actual_columns[x1_col], actual_columns[x2_col],
                  paste(actual_columns[x1_col], "vs", actual_columns[x2_col]),
                  x1_split, x2_split)

  # Create the second plot
  p2 <- base_plot("x2", "x3", actual_columns[x2_col], actual_columns[x3_col],
                  paste(actual_columns[x2_col], "vs", actual_columns[x3_col]),
                  x2_split, x3_split) +
    theme(legend.position = "none")

  # Combine the plots using `patchwork` with labels
  combined_plot <- (p1 + p2) +
    plot_layout(ncol = 2, widths = c(1.2, 1)) +
    plot_annotation(
      title = paste("Policy Tree Results for", model_name),
      tag_levels = 'A'
    ) &
    theme(plot.tag = element_text(size = 12, face = "bold"))

  # Return the combined plot
  return(combined_plot)
}


# margot_plot_policy_tree <- function(mc_test, model_name,
#                                     color_scale = ggokabeito::scale_colour_okabe_ito(),
#                                     point_alpha = 0.25,
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
#                                     split_line_size = 0.5,
#                                     split_label_size = 10,
#                                     split_label_color = "darkgray",
#                                     train_proportion = 0.8,
#                                     custom_action_names = NULL) {
#   # extract policy tree object
#   policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2
#
#   # extract action names from the policy tree object
#   action_names <- policy_tree_obj$action.names
#
#   # if custom action names are provided, use them instead
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != length(action_names)) {
#       stop("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#   }
#
#   # create the markdown interpretation
#   create_markdown_interpretation <- function(policy_tree, actual_columns, action_names) {
#     interpretation <- paste0(
#       "### Policy Tree Interpretation\n\n",
#       "A policy tree obtains simple rule-based policies, where the rule takes the form of a shallow decision tree. ",
#       "The policytree algorithm uses doubly robust reward estimates from grf to find a shallow, but globally optimal decision tree. ",
#       sprintf("We train the model on %.0f%% of the data and then evaluate the model on the remainder of the data. ", train_proportion * 100),
#       "The graph helps to clarify whether the leaf node in the test set samples are predicted to have mean outcomes in line with the prescribed policy.\n\n",
#       "## Specific findings for this policy tree:\n\n",
#       "1. The first split is based on the variable '", actual_columns[policy_tree[[1]]$split_variable], "' at a value of ", round(policy_tree[[1]]$split_value, 4), ".\n\n",
#       "   - If this value is less than or equal to the split value:\n",
#       "     2. The second split is based on '", actual_columns[policy_tree[[2]]$split_variable], "' at ", round(policy_tree[[2]]$split_value, 4), ".\n",
#       "        - If this is less than or equal to the split value, the recommended action is: **", action_names[policy_tree[[4]]$action], "**\n",
#       "        - Otherwise, the recommended action is: **", action_names[policy_tree[[5]]$action], "**\n\n",
#       "   - If the first split value is greater than the split value:\n",
#       "     3. The second split is based on '", actual_columns[policy_tree[[3]]$split_variable], "' at ", round(policy_tree[[3]]$split_value, 4), ".\n",
#       "        - If this is less than or equal to the split value, the recommended action is: **", action_names[policy_tree[[6]]$action], "**\n",
#       "        - Otherwise, the recommended action is: **", action_names[policy_tree[[7]]$action], "**\n\n",
#       "This policy tree suggests that an optimal treatment strategy is related to these inflection points in the variables the policy tree identifies, ",
#       "with recommended actions based on subgroups defined by these split points."
#     )
#     return(interpretation)
#   }
#
#   # extract the plot data for the specified model
#   plot_data <- mc_test$results[[model_name]]$plot_data
#
#   # extract X_test, predictions, and split_variables
#   X_test <- plot_data$X_test
#   predictions <- plot_data$predictions
#
#   # extract split variables and their values from the policy tree
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
#   # use the actual column names from X_test
#   actual_columns <- names(X_test)
#
#   # a data frame for plotting
#   plot_df <- data.frame(
#     x1 = X_test[[actual_columns[x1_col]]],
#     x2 = X_test[[actual_columns[x2_col]]],
#     x3 = X_test[[actual_columns[x3_col]]],
#     prediction = factor(predictions, levels = seq_along(action_names), labels = action_names)
#   )
#
#   # a data frame for plotting
#   plot_df <- data.frame(
#     x1 = X_test[[actual_columns[x1_col]]],
#     x2 = X_test[[actual_columns[x2_col]]],
#     x3 = X_test[[actual_columns[x3_col]]],
#     prediction = as.factor(predictions - 1)  # subtract 1 to match the legend in your example
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
#                  linetype = split_line_type, size = split_line_size) +
#       geom_hline(yintercept = y_split, color = split_line_color, alpha = split_line_alpha,
#                  linetype = split_line_type, size = split_line_size) +
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
#   # create the first plot
#   p1 <- base_plot("x1", "x2", actual_columns[x1_col], actual_columns[x2_col],
#                   paste(actual_columns[x1_col], "vs", actual_columns[x2_col]),
#                   x1_split, x2_split)
#
#   # create the second plot
#   p2 <- base_plot("x2", "x3", actual_columns[x2_col], actual_columns[x3_col],
#                   paste(actual_columns[x2_col], "vs", actual_columns[x3_col]),
#                   x2_split, x3_split) +
#     theme(legend.position = "none")
#
#   # combine the plots using `patchwork` with labels
#   combined_plot <- (p1 + p2) +
#     plot_layout(ncol = 2, widths = c(1.2, 1)) +
#     plot_annotation(
#       title = paste("Policy Tree Results for", model_name),
#       tag_levels = 'A'
#     ) &
#     theme(plot.tag = element_text(size = 12, face = "bold"))
#
#
#   # Create the markdown interpretation
#   markdown_interpretation <- create_markdown_interpretation(policy_tree, actual_columns, action_names)
#
#   # Return both the plot and the markdown interpretation
#   return(list(plot = combined_plot, interpretation = markdown_interpretation))
# }
