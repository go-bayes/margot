#' Internal wrapper for policy tree computation
#'
#' @description
#' Internal function that wraps either policytree::policy_tree() or
#' fastpolicytree::fastpolicytree() depending on user preference and
#' package availability.
#'
#' @param X Covariate matrix
#' @param Gamma Matrix of doubly robust scores
#' @param depth Integer depth of tree (1 or 2)
#' @param tree_method Character string: "policytree" or "fastpolicytree"
#'
#' @return Policy tree object
#' @keywords internal
#' @importFrom cli cli_alert_warning
.compute_policy_tree <- function(
    X,
    Gamma,
    depth,
    tree_method = "policytree",
    min_node_size = getOption("margot.policy_tree.min_node_size", 1L)
) {
  # validate tree_method
  tree_method <- match.arg(tree_method, c("policytree", "fastpolicytree"))

  if (!is.numeric(min_node_size) || length(min_node_size) != 1L || is.na(min_node_size)) {
    stop("min_node_size must be a single numeric value")
  }
  min_node_size <- as.integer(min_node_size)
  if (min_node_size < 1L) {
    stop("min_node_size must be >= 1")
  }

  # check if fastpolicytree is requested and available
  if (tree_method == "fastpolicytree") {
    if (!requireNamespace("fastpolicytree", quietly = TRUE)) {
      cli::cli_alert_warning(
        "fastpolicytree package not installed, falling back to policytree. ",
        "Install with: install.packages('fastpolicytree')"
      )
      tree_method <- "policytree"
    }
  }

  # compute tree using selected method
  if (tree_method == "fastpolicytree") {
    fastpolicytree::fastpolicytree(
      X,
      Gamma,
      depth = depth,
      min.node.size = min_node_size
    )
  } else {
    policytree::policy_tree(
      X,
      Gamma,
      depth = depth,
      min.node.size = min_node_size
    )
  }
}

#' Normalise policy tree actions to 1-based indexing
#' @keywords internal
.normalize_policy_actions <- function(actions) {
  if (is.null(actions)) return(actions)
  actions <- as.integer(actions)
  if (!length(actions)) return(actions)
  if (all(is.na(actions))) return(actions)
  if (min(actions, na.rm = TRUE) == 0L) actions <- actions + 1L
  actions
}

#' Check if fastpolicytree is available
#'
#' @return Logical indicating if fastpolicytree package is installed
#' @keywords internal
.has_fastpolicytree <- function() {
  requireNamespace("fastpolicytree", quietly = TRUE)
}

#' Get policy tree method with fallback
#'
#' @param requested_method The requested tree method
#' @param verbose Whether to print messages about fallback
#' @return The method to use (may differ from requested if package unavailable)
#' @keywords internal
.get_tree_method <- function(requested_method = "policytree", verbose = TRUE) {
  if (requested_method == "fastpolicytree" && !.has_fastpolicytree()) {
    if (verbose) {
      cli::cli_alert_info(
        "Note: fastpolicytree not available, using policytree. ",
        "For ~10x faster computation, install fastpolicytree."
      )
    }
    return("policytree")
  }

  return(requested_method)
}
