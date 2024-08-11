#' Plot Decision Tree from Policy Tree Results
#'
#' This function creates a ggplot object displaying a decision tree based on the
#' results of a policy tree model.
#'
#' @param result_object A list containing the results from a policy tree model,
#'   or a policy tree object directly.
#' @param model_name A character string specifying the name of the model within
#'   the result_object to plot. This should match one of the model names in
#'   result_object$results. If NULL and a policy tree object is provided directly,
#'   the function will use that object.
#' @param x_padding The amount of padding to add to the x-axis. Default is 0.25.
#' @param y_padding The amount of padding to add to the y-axis. Default is 0.25.
#' @param border_size The size of the border around node labels. Default is 1.
#' @param edge_label_offset The offset for edge labels from the branches. Default is 0.05.
#' @param span_ratio The aspect ratio of the plot. Default is 0.5.
#' @param text_size The size of the text in the node labels. Default is 5.
#' @param non_leaf_fill The fill color for non-leaf nodes. Default is "lightyellow".
#' @param title The title for the plot. Default is NULL.
#' @param leaf_palette A vector of colors to be used for leaf nodes. If NULL (default),
#'   the function will use the Okabe-Ito color palette.
#'
#' @return A ggplot object representing the decision tree for the specified model.
#'
#' @import ggplot2
#' @import janitor
#' @import ggokabeito
#'
#' @examples
#' \dontrun{
#' # Assuming result_object is the policy tree model output
#' margot_plot_decision_tree(result_object, "model_name")
#'
#' # Or if you have a policy tree object directly
#' margot_plot_decision_tree(policy_tree_obj)
#'
#' # Using a custom color palette for leaf nodes
#' custom_palette <- c("#FF0000", "#00FF00", "#0000FF")
#' margot_plot_decision_tree(result_object, "model_name", leaf_palette = custom_palette)
#' }
#'
#' @export
margot_plot_decision_tree <- function(result_object, model_name = NULL,
                                      x_padding = 0.25,
                                      y_padding = 0.25,
                                      border_size = 1,
                                      edge_label_offset = 0.02,
                                      span_ratio = 0.25,
                                      text_size = 4,
                                      non_leaf_fill = "lightyellow",
                                      title = NULL,
                                      leaf_palette = NULL) {

  # Determine the correct policy tree object
  if (!is.null(model_name) && "results" %in% names(result_object)) {
    policy_tree_obj <- result_object$results[[model_name]]$policy_tree_depth_2
  } else if ("nodes" %in% names(result_object)) {
    policy_tree_obj <- result_object
  } else {
    stop("Invalid input. Please provide either a results object with a model name, or a policy tree object.")
  }

  calculate_node_positions <- function(policy_tree) {
    nodes <- policy_tree$nodes
    columns <- policy_tree$columns
    action_names <- policy_tree$action.names

    node_data <- data.frame(
      id = integer(length(nodes)),
      is_leaf = logical(length(nodes)),
      split_variable = character(length(nodes)),
      split_value = numeric(length(nodes)),
      action = character(length(nodes)),
      left_child = integer(length(nodes)),
      right_child = integer(length(nodes)),
      x = numeric(length(nodes)),
      y = numeric(length(nodes)),
      label = character(length(nodes)),
      stringsAsFactors = FALSE
    )

    for (i in seq_along(nodes)) {
      node_data$id[i] <- i
      node_data$is_leaf[i] <- nodes[[i]]$is_leaf
      node_data$split_variable[i] <- if (!is.null(nodes[[i]]$split_variable)) as.character(columns[nodes[[i]]$split_variable]) else NA_character_
      node_data$split_value[i] <- if (!is.null(nodes[[i]]$split_value)) as.numeric(nodes[[i]]$split_value) else NA_real_
      node_data$action[i] <- if (!is.null(nodes[[i]]$action)) as.character(action_names[nodes[[i]]$action]) else NA_character_
      node_data$left_child[i] <- if (!is.null(nodes[[i]]$left_child)) as.integer(nodes[[i]]$left_child) else NA_integer_
      node_data$right_child[i] <- if (!is.null(nodes[[i]]$right_child)) as.integer(nodes[[i]]$right_child) else NA_integer_

      # Create label
      if (node_data$is_leaf[i]) {
        node_data$label[i] <- paste("Action:", node_data$action[i])
      } else {
        node_data$label[i] <- paste(node_data$split_variable[i], "\n<=", round(node_data$split_value[i], 2))
      }
    }

    max_depth <- policy_tree$depth
    for (i in 1:nrow(node_data)) {
      if (i == 1) {
        node_data$x[i] <- 0.5
        node_data$y[i] <- 1
      } else {
        parent <- which(node_data$left_child == i | node_data$right_child == i)
        node_data$y[i] <- node_data$y[parent] - 1/(max_depth)
        x_offset <- 0.5 / (2^(max_depth - node_data$y[i]))
        node_data$x[i] <- if (node_data$left_child[parent] == i) {
          node_data$x[parent] - x_offset
        } else {
          node_data$x[parent] + x_offset
        }
      }
    }
    return(node_data)
  }

  # Print policy tree structure
  print_policy_tree <- function(policy_tree) {
    print_node <- function(node, depth = 0) {
      indent <- paste(rep("  ", depth), collapse = "")
      if (node$is_leaf) {
        cat(paste0(indent, "* Action: ", policy_tree$action.names[node$action], "\n"))
      } else {
        cat(paste0(indent, "Split on: ", policy_tree$columns[node$split_variable],
                   " <= ", node$split_value, "\n"))
        print_node(policy_tree$nodes[[node$left_child]], depth + 1)
        print_node(policy_tree$nodes[[node$right_child]], depth + 1)
      }
    }
    cat("Policy Tree Structure:\n")
    print_node(policy_tree$nodes[[1]])
  }

  print_policy_tree(policy_tree_obj)

  # Calculate node positions
  node_data <- calculate_node_positions(policy_tree_obj)

  # Create edge data
  edge_data <- data.frame(
    x = numeric(),
    y = numeric(),
    xend = numeric(),
    yend = numeric(),
    edge_label = character(),
    hjust = numeric(),
    vjust = numeric(),
    angle = numeric()
  )

  for (i in 1:nrow(node_data)) {
    if (!is.na(node_data$left_child[i])) {
      left_child <- node_data$left_child[i]
      edge_data <- rbind(edge_data, data.frame(
        x = node_data$x[i],
        y = node_data$y[i],
        xend = node_data$x[left_child],
        yend = node_data$y[left_child],
        edge_label = "True",
        hjust = 1,
        vjust = 0.5,
        angle = 0
      ))
    }
    if (!is.na(node_data$right_child[i])) {
      right_child <- node_data$right_child[i]
      edge_data <- rbind(edge_data, data.frame(
        x = node_data$x[i],
        y = node_data$y[i],
        xend = node_data$x[right_child],
        yend = node_data$y[right_child],
        edge_label = "False",
        hjust = 0,
        vjust = 0.5,
        angle = 0
      ))
    }
  }

  # Adjust label positions
  edge_data$label_x <- edge_data$x + (edge_data$xend - edge_data$x) / 2
  edge_data$label_y <- edge_data$y + (edge_data$yend - edge_data$y) / 2

  # Adjust label positions to be outside the branches
  edge_data$label_x <- edge_data$label_x +
    edge_label_offset * sign(edge_data$xend - edge_data$x)

  # Use provided palette or default to Okabe-Ito
  if (is.null(leaf_palette)) {
    leaf_palette <- ggokabeito::palette_okabe_ito()
  }

  # Create a mapping of actions to colors
  unique_actions <- unique(node_data$action[!is.na(node_data$action)])
  action_colors <- setNames(
    rep_len(leaf_palette, length(unique_actions)),
    unique_actions
  )

  # Assign colors to leaf nodes based on their action
  node_data$fill_color <- ifelse(
    node_data$is_leaf,
    action_colors[node_data$action],
    non_leaf_fill
  )

  # Calculate aspect ratio based on the span_ratio
  y_range <- max(node_data$y) - min(node_data$y)
  x_range <- max(node_data$x) - min(node_data$x)
  aspect_ratio <- (x_range / y_range) * span_ratio

  if (is.null(title) && !is.null(model_name)) {
    formatted_model_name <- janitor::make_clean_names(model_name)
    formatted_model_name <- stringr::str_replace_all(formatted_model_name, "_", " ")
    formatted_model_name <- tools::toTitleCase(formatted_model_name)
    title <- formatted_model_name
  } else if (identical(title, "none")) {
    title <- NULL
  }

  p <- ggplot() +
    geom_segment(data = edge_data, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_label(data = node_data, aes(x = x, y = y, label = label),
               size = text_size,
               fill = node_data$fill_color,
               label.padding = unit(border_size, "lines")) +
    geom_text(data = edge_data,
              aes(x = label_x, y = label_y, label = edge_label,
                  hjust = hjust, vjust = vjust),
              size = text_size) +
    theme_void() +
    coord_fixed(ratio = aspect_ratio) +
    scale_x_continuous(expand = expansion(mult = c(x_padding, x_padding))) +
    scale_y_continuous(expand = expansion(mult = c(y_padding, y_padding))) +
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    labs(title = title)

  # Run basic unit test ### NEED TO ALLOW AUTOMATIC UPDATING
  # test_result <- tryCatch({
  #   testthat::test_that("Tree structure and plot alignment test", {
  #     # Test that root node exists
  #     expect_true("label" %in% names(node_data))
  #     expect_equal(nrow(node_data[node_data$y == max(node_data$y),]), 1)
  #
  #     # Test that leaf nodes exist
  #     expect_true(any(grepl("Action:", node_data$label)))
  #
  #     # Test that edge labels are correct
  #     expect_true(all(edge_data$edge_label %in% c("True", "False")))
  #
  #     # Test the tree structure
  #     root_node <- node_data[node_data$y == max(node_data$y),]
  #     left_child <- node_data[node_data$id == root_node$left_child,]
  #     right_child <- node_data[node_data$id == root_node$right_child,]
  #
  #     expect_true(grepl("t0_log_hours_housework_z", root_node$label))
  #     expect_true(grepl("t0_hlth_bmi_z", left_child$label))
  #     expect_true(grepl("t0_log_hours_exercise_z", right_child$label))
  #   })
  #   "All tests passed."
  # }, error = function(e) {
  #   paste("Test failed:", e$message)
  # })
  #
  # cat("\nTest Result: ", test_result, "\n")

  return(p)
}
# old
# margot_plot_decision_tree <- function(batch_model_output, model_name,
#                                       x_padding = 0.25,
#                                       y_padding = 0.25,
#                                       border_size = 1,
#                                       edge_label_hjust_true = 1.5,
#                                       edge_label_hjust_false = -0.5,
#                                       span_ratio = 0.5,
#                                       text_size = 5,
#                                       node_distance_adjustment = -1.5,
#                                       leaf_fill = "lightgreen",
#                                       non_leaf_fill = "lightyellow",
#                                       title = NULL) {
#   # Extract the policy tree using the batch model output and model name
#   policy_tree <- batch_model_output$results[[model_name]]$policy_tree_depth_2
#
#   # Debug node data to get positions
#   node_data <- debug_node_data_with_positions(policy_tree, node_distance_adjustment)
#
#   # Create edge data
#   edge_data <- data.frame(
#     x = numeric(),
#     y = numeric(),
#     xend = numeric(),
#     yend = numeric(),
#     edge_label = character(),
#     hjust = numeric()
#   )
#
#   for (i in 1:nrow(node_data)) {
#     if (!is.na(node_data$left_child[i])) {
#       edge_data <- rbind(edge_data, data.frame(
#         x = node_data$x[i],
#         y = node_data$y[i],
#         xend = node_data$x[node_data$left_child[i]],
#         yend = node_data$y[node_data$left_child[i]],
#         edge_label = "True",
#         hjust = edge_label_hjust_true
#       ))
#     }
#     if (!is.na(node_data$right_child[i])) {
#       edge_data <- rbind(edge_data, data.frame(
#         x = node_data$x[i],
#         y = node_data$y[i],
#         xend = node_data$x[node_data$right_child[i]],
#         yend = node_data$y[node_data$right_child[i]],
#         edge_label = "False",
#         hjust = edge_label_hjust_false
#       ))
#     }
#   }
#
#   # Create labels for nodes
#   node_data$label <- ifelse(node_data$is_leaf,
#                             paste("Action:", node_data$action),
#                             paste(node_data$split_variable, "\n<=", round(node_data$split_value, 2)))
#
#   # Format model name for the title
#   formatted_model_name <- janitor::make_clean_names(model_name)
#   formatted_model_name <- stringr::str_replace_all(formatted_model_name, "_", " ")
#   formatted_model_name <- tools::toTitleCase(formatted_model_name)
#
#   # Set title if not provided
#   if (is.null(title)) {
#     title <- formatted_model_name
#   } else if (title == "none") {
#     title <- NULL
#   }
#
#   # Calculate aspect ratio based on the span_ratio
#   y_range <- max(node_data$y) - min(node_data$y)
#   x_range <- max(node_data$x) - min(node_data$x)
#   aspect_ratio <- (x_range / y_range) * span_ratio
#
#   # Create the plot
#   p <- ggplot() +
#     geom_segment(data = edge_data, aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_label(data = node_data, aes(x = x, y = y, label = label),
#                size = text_size,
#                fill = ifelse(node_data$is_leaf, leaf_fill, non_leaf_fill),
#                label.padding = unit(border_size, "lines")) +
#     geom_text(data = edge_data, aes(x = (x + xend) / 2, y = (y + yend) / 2,
#                                     label = edge_label, hjust = hjust), size = text_size) +
#     theme_void() +
#     coord_fixed(ratio = aspect_ratio) +
#     scale_x_continuous(expand = expansion(mult = c(x_padding, x_padding))) +
#     scale_y_continuous(expand = expansion(mult = c(y_padding, y_padding))) +
#     theme(
#       plot.margin = unit(c(1, 1, 1, 1), "cm"),
#       plot.title = element_text(hjust = 0.5, face = "bold")
#     ) +
#     labs(title = title)
#
#   return(p)
# }
