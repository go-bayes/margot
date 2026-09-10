# validate stored tree links before recursive plotting; never change the fitted rule.
.margot_policy_plot_validate_tree <- function(tree) {
  nodes <- tree$nodes
  if (!is.list(nodes) || !length(nodes) || !length(tree$columns) || !length(tree$action.names)) {
    stop("A policy tree requires nodes, columns and action names.", call. = FALSE)
  }
  seen <- integer()
  visit <- function(id, level) {
    if (length(id) != 1L || !is.numeric(id) || !is.finite(id) || id != as.integer(id) || id < 1 || id > length(nodes)) stop("Invalid policy-tree child index.", call. = FALSE)
    if (id %in% seen) stop("Policy-tree links contain a cycle or repeated child.", call. = FALSE)
    seen <<- c(seen, id)
    node <- nodes[[id]]
    if (!is.logical(node$is_leaf) || length(node$is_leaf) != 1 || is.na(node$is_leaf)) stop("Every node requires a logical is_leaf flag.", call. = FALSE)
    if (isTRUE(node$is_leaf)) {
      links <- list(node$left_child, node$right_child)
      if (any(!vapply(links, function(link) is.null(link) || (length(link) == 1L && is.na(link)), logical(1)))) stop("Terminal nodes cannot contain child links.", call. = FALSE)
      action <- node$action
      if (length(action) != 1 || !is.numeric(action) || !is.finite(action) || action != as.integer(action) || action < 1 || action > length(tree$action.names)) stop("Invalid terminal action.", call. = FALSE)
      return(level)
    }
    variable <- node$split_variable
    if (length(variable) != 1 || !is.numeric(variable) || !is.finite(variable) || variable != as.integer(variable) || variable < 1 || variable > length(tree$columns)) stop("Invalid split variable index.", call. = FALSE)
    if (length(node$split_value) != 1 || !is.numeric(node$split_value) || !is.finite(node$split_value)) stop("Invalid split threshold.", call. = FALSE)
    max(visit(node$left_child, level + 1L), visit(node$right_child, level + 1L))
  }
  depth <- visit(1L, 0L)
  if (length(seen) != length(nodes)) stop("Policy tree contains unreachable nodes.", call. = FALSE)
  if (depth > 2L) stop("Policy-tree plots support depth zero, one or two.", call. = FALSE)
  if (length(tree$depth) != 1 || !is.numeric(tree$depth) || !is.finite(tree$depth) || tree$depth < depth) stop("Stored depth is incompatible with policy-tree links.", call. = FALSE)
  invisible(depth)
}

# assign ordered leaves equal spacing and centre each parent over its children.
.margot_policy_compact_positions <- function(nodes) {
  x <- y <- rep(NA_real_, length(nodes))
  next_leaf <- 0L
  visit <- function(id, level) {
    y[[id]] <<- -level
    if (isTRUE(nodes[[id]]$is_leaf)) {
      next_leaf <<- next_leaf + 1L
      x[[id]] <<- next_leaf
    } else {
      children <- c(nodes[[id]]$left_child, nodes[[id]]$right_child)
      for (child in children) visit(child, level + 1L)
      x[[id]] <<- mean(x[children])
    }
  }
  visit(1L, 0L)
  list(x = x, y = y)
}

# resolve edge text from explicit labels or the same threshold text used by nodes.
.margot_policy_branch_labels <- function(edges, labels) {
  if (is.function(labels)) {
    result <- labels(edges)
  } else if (is.data.frame(labels)) {
    if (!all(c("parent_id", "side", "label") %in% names(labels)) || nrow(labels) != nrow(edges)) stop("branch_labels must identify every edge with parent_id, side and label.", call. = FALSE)
    key <- paste(labels$parent_id, labels$side)
    expected <- paste(edges$parent_id, edges$side)
    if (anyDuplicated(key) || !setequal(key, expected)) stop("branch_labels must identify every edge exactly once.", call. = FALSE)
    result <- labels$label[match(expected, key)]
  } else if (identical(labels, "condition")) {
    result <- paste(ifelse(edges$side == "left", "<=", ">"), edges$threshold_label)
  } else {
    if (!is.character(labels) || length(labels) != 2L || !setequal(names(labels), c("left", "right")) || anyDuplicated(names(labels))) stop("branch_labels must be 'condition', a named left/right pair, an edge table or a function.", call. = FALSE)
    result <- unname(labels[edges$side])
  }
  if (!is.character(result) || length(result) != nrow(edges) || anyNA(result)) stop("branch_labels must return one character label per edge.", call. = FALSE)
  result
}

# wrap complete node labels at explicit character widths while preserving line breaks.
.margot_policy_wrap_nodes <- function(labels, width) {
  if (is.null(width)) return(labels)
  if (!is.numeric(width) || length(width) != 1 || !is.finite(width) || width < 1 || width != as.integer(width)) stop("node_label_width must be a positive integer.", call. = FALSE)
  vapply(labels, .margot_policy_wrap, character(1), width = width, USE.NAMES = FALSE)
}

# validate panel names and literal ggplot labels before assembling nested plots.
.margot_policy_validate_panel_labels <- function(labels) {
  if (!is.list(labels) || (length(labels) && (is.null(names(labels)) || anyDuplicated(names(labels)) || any(!names(labels) %in% c("A", "B", "C", "D"))))) stop("panel_labels must be a named list with keys A, B, C or D.", call. = FALSE)
  for (key in names(labels)) {
    values <- labels[[key]]
    if (!is.list(values) || is.null(names(values)) || anyDuplicated(names(values)) || any(!names(values) %in% c("title", "subtitle", "x", "y", "caption"))) stop("Each panel label override must name title, subtitle, x, y or caption.", call. = FALSE)
    if (any(!vapply(values, function(value) is.null(value) || (is.character(value) && length(value) == 1L && !is.na(value)), logical(1)))) stop("Panel label overrides must be character scalars or NULL.", call. = FALSE)
  }
  invisible(NULL)
}

# apply validated presentation labels to the named report panels.
.margot_policy_panel_labels <- function(panels, labels) {
  .margot_policy_validate_panel_labels(labels)
  for (key in names(labels)) panels[[key]] <- panels[[key]] + do.call(ggplot2::labs, labels[[key]])
  panels
}
