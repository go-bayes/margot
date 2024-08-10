#' Plot Decision Tree from Policy Tree Results
#'
#' This function creates a ggplot object displaying a decision tree based on the
#' results of a policy tree model.
#'
#' @param batch_model_output A list containing the results from a policy tree model.
#' @param model_name A character string specifying the name of the model within
#'   the batch_model_output to plot. This should match one of the model names in
#'   batch_model_output$results.
#' @param x_padding The amount of padding to add to the x-axis. Default is 0.25.
#' @param y_padding The amount of padding to add to the y-axis. Default is 0.05.
#' @param border_size The size of the border around node labels. Default is 1.
#' @param edge_label_hjust_true Horizontal justification for "True" edge labels.
#'   Default is 1.5.
#' @param edge_label_hjust_false Horizontal justification for "False" edge labels.
#'   Default is -0.5.
#' @param span_ratio The aspect ratio of the plot. Default is 2.
#' @param text_size The size of the text in the node labels. Default is 4.
#' @param node_distance_adjustment The adjustment for spacing between nodes 5 and 6.
#'   Default is 0.3.
#' @param leaf_fill The fill color for leaf nodes. Default is "lightgreen".
#' @param non_leaf_fill The fill color for non-leaf nodes. Default is "lightyellow".
#' @param title The title for the plot. Set to "none" for no title. Default is NULL.
#'
#' @return A ggplot object representing the decision tree for the specified model.
#'
#' @import ggplot2
#' @import janitor
#'
#' @examples
#' \dontrun{
#' # Assuming result_psychological_well_being is the batch model output
#' margot_plot_decision_tree(result_psychological_well_being, "model_t2_hlth_fatigue_z")
#' }
#'
#' @export
margot_plot_decision_tree <- function(batch_model_output, model_name, x_padding = 0.25, y_padding = 0.05,
                                      border_size = 1, edge_label_hjust_true = 1.5,
                                      edge_label_hjust_false = -0.5, span_ratio = 2,
                                      text_size = 4, node_distance_adjustment = 0.3,
                                      leaf_fill = "lightgreen", non_leaf_fill = "lightyellow",
                                      title = NULL) {
  # Extract the policy tree using the batch model output and model name
  policy_tree <- batch_model_output$results[[model_name]]$policy_tree_depth_2

  # Debug node data to get positions
  node_data <- debug_node_data_with_positions(policy_tree)

  # Manually adjust positions for nodes 5 and 6 with user-defined spacing
  if (5 %in% node_data$id && 6 %in% node_data$id) {
    node_data$x[node_data$id == 5] <- node_data$x[node_data$id == 5] - node_distance_adjustment  # Move node 5 west
    node_data$x[node_data$id == 6] <- node_data$x[node_data$id == 6] + node_distance_adjustment  # Move node 6 east
  }

  # create edge data
  edge_data <- data.frame(
    x = numeric(),
    y = numeric(),
    xend = numeric(),
    yend = numeric(),
    edge_label = character(),
    hjust = numeric()  # New column for horizontal justification
  )

  for (i in 1:nrow(node_data)) {
    if (!is.na(node_data$left_child[i])) {
      edge_data <- rbind(edge_data, data.frame(
        x = node_data$x[i],
        y = node_data$y[i],
        xend = node_data$x[node_data$left_child[i]],
        yend = node_data$y[node_data$left_child[i]],
        edge_label = "True",
        hjust = edge_label_hjust_true  # Set hjust for "True"
      ))
    }
    if (!is.na(node_data$right_child[i])) {
      edge_data <- rbind(edge_data, data.frame(
        x = node_data$x[i],
        y = node_data$y[i],
        xend = node_data$x[node_data$right_child[i]],
        yend = node_data$y[node_data$right_child[i]],
        edge_label = "False",
        hjust = edge_label_hjust_false  # Set hjust for "False"
      ))
    }
  }

  # create labels for nodes
  node_data$label <- ifelse(node_data$is_leaf,
                            paste("Action:", node_data$action),
                            paste(node_data$split_variable, "\n<=", round(node_data$split_value, 2)))

  # Format model name for the title
  formatted_model_name <- janitor::make_clean_names(model_name)
  formatted_model_name <- stringr::str_replace_all(formatted_model_name, "_", " ")
  formatted_model_name <- tools::toTitleCase(formatted_model_name)

  # set title if not provided
  if (is.null(title)) {
    title <- formatted_model_name
  } else if (title == "none") {
    title <- NULL
  }

  # create the plot
  p <- ggplot() +
    geom_segment(data = edge_data, aes(x = x, y = y, xend = xend, yend = yend)) +  # No arrowheads
    geom_label(data = node_data, aes(x = x, y = y, label = label),
               size = text_size,
               fill = ifelse(node_data$is_leaf, leaf_fill, non_leaf_fill),
               label.padding = unit(border_size, "lines")) +  # Adjust border size
    geom_text(data = edge_data, aes(x = (x + xend) / 2, y = (y + yend) / 2,
                                    label = edge_label, hjust = hjust), size = text_size) +  # Adjust hjust
    theme_void() +
    coord_fixed(ratio = span_ratio) +  # Adjust span ratio
    scale_y_reverse() +
    expand_limits(x = c(min(node_data$x) - x_padding, max(node_data$x) + x_padding),
                  y = c(min(node_data$y) - y_padding, max(node_data$y) + y_padding)) +
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Adjust margins
      plot.title = element_text(hjust = 0.5, face = "bold")  # Center and bold title
    ) +
    labs(title = title)

  return(p)
}

# #eExample usage
# final_plot <- margot_plot_decision_tree(
#   result_psychological_well_being,
#   "model_t2_hlth_fatigue_z",
#   # title = "none"  # specify "none" for no title
# )
# final_plot
