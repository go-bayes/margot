#' Calculate Node Data and Positions for Policy Tree Visualisation
#'
#' This internal function processes a policy tree object and calculates the positions
#' of nodes for visualisation purposes. It extracts relevant information from the
#' policy tree and computes x and y coordinates for each node.
#'
#' @param policy_tree A policy tree object, typically obtained from a causal forest model.
#' @param node_distance_adjustment The adjustment for spacing between nodes. Default is -1.5.
#'
#' @return A data frame containing information about each node in the policy tree,
#'   including its ID, whether it's a leaf node, split variable, split value, action,
#'   left and right child node IDs, and x and y coordinates for visualization.
#'
#' @keywords internal
debug_node_data_with_positions <- function(policy_tree, node_distance_adjustment = -1.5) {
  nodes <- policy_tree$nodes
  columns <- policy_tree$columns
  action_names <- policy_tree$action.names

  # Create a data frame for the nodes
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
    stringsAsFactors = FALSE
  )

  # Populate the node data
  for (i in seq_along(nodes)) {
    node_data$id[i] <- i
    node_data$is_leaf[i] <- nodes[[i]]$is_leaf
    node_data$split_variable[i] <- if (!is.null(nodes[[i]]$split_variable)) as.character(columns[nodes[[i]]$split_variable]) else NA_character_
    node_data$split_value[i] <- if (!is.null(nodes[[i]]$split_value)) as.numeric(nodes[[i]]$split_value) else NA_real_
    node_data$action[i] <- if (!is.null(nodes[[i]]$action)) as.character(action_names[nodes[[i]]$action]) else NA_character_
    node_data$left_child[i] <- if (!is.null(nodes[[i]]$left_child)) as.integer(nodes[[i]]$left_child) else NA_integer_
    node_data$right_child[i] <- if (!is.null(nodes[[i]]$right_child)) as.integer(nodes[[i]]$right_child) else NA_integer_
  }

  # Calculate node positions
  max_depth <- policy_tree$depth
  for (i in 1:nrow(node_data)) {
    if (i == 1) {
      node_data$x[i] <- 0.5
      node_data$y[i] <- 1
    } else {
      parent <- which(node_data$left_child == i | node_data$right_child == i)
      node_data$y[i] <- node_data$y[parent] - 1/(max_depth)

      # Keep outer edges at the same angle
      if (node_data$y[i] == min(node_data$y)) {
        x_offset <- 0.5 / (2^(max_depth - 1))
      } else {
        # Adjust inner nodes
        x_offset <- 0.5 / (2^(max_depth - 1)) * node_distance_adjustment
      }

      if (node_data$left_child[parent] == i) {
        node_data$x[i] <- node_data$x[parent] - x_offset
      } else {
        node_data$x[i] <- node_data$x[parent] + x_offset
      }
    }
  }

  return(node_data)
}
