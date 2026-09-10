#' Create a Combined Decision Tree and Policy Relationship Graph
#'
#' This function generates a combined plot consisting of a decision tree and a graph
#' showing relationships between variables in the recommended policy.
#'
#' @param result_object An object containing the results from a multi-arm causal forest model.
#' @param model_name A character string specifying the name of the model.
#' @param max_depth Integer, 1 or 2; which decision tree depth to plot. Default: 2.
#' @param label_mapping Optional named list for custom label mappings.
#' @param original_df Optional dataframe with untransformed variables.
#' @param layout A list specifying the layout of the combined plot at either depth. Default is
#'   `list(heights = c(1, 2))`, which sets the relative heights of the two plots.
#' @param annotation A list specifying the annotation for the combined plot at either depth. Default is
#'   `list(tag_levels = "A")`, which adds alphabetic tags to the subplots.
#' @param generate_policy_tree Logical, whether to generate the policy tree plot. Default is TRUE.
#' @param generate_decision_tree Logical, whether to generate the decision tree plot. Default is TRUE.
#' @param policy_tree_args A list of arguments to pass to `margot_plot_policy_tree`. Default is list().
#' @param decision_tree_args A list of arguments to pass to `margot_plot_decision_tree`. Default is list().
#'
#' @return A list containing:
#'   \item{policy_tree}{A ggplot object representing the policy tree (if generated)}
#'   \item{decision_tree}{A ggplot object representing the decision tree (if generated)}
#'   \item{combined_plot}{A ggplot object representing the combined plot (if both plots are generated)}
#'
#' @import ggplot2
#' @import patchwork
#' @import cli
#' @export
margot_plot_policy_combo <- function(result_object,
                                     model_name,
                                     max_depth = 2L,
                                     label_mapping = NULL,
                                     original_df = NULL,
                                     layout = list(heights = c(1, 2)),
                                     annotation = list(tag_levels = "A"),
                                     generate_policy_tree = TRUE,
                                     generate_decision_tree = TRUE,
                                     policy_tree_args = list(),
                                     decision_tree_args = list()) {
  cli::cli_h1("Margot Plot Policy Combo")

  # weighted display labels use every projection row and the same supplied weights.
  if (!is.null(policy_tree_args$display_weights) &&
      isTRUE(decision_tree_args$show_leaf_metrics) && is.null(decision_tree_args$leaf_metrics)) {
    tree <- result_object$results[[model_name]][[paste0("policy_tree_depth_", max_depth)]]
    reference <- .policy_tree_build_predict_df(result_object$results[[model_name]]$plot_data, tree$columns)
    w <- policy_tree_args$display_weights
    .margot_policy_display_weights(w, nrow(reference))
    ids <- .margot_policy_tree_leaf_ids(tree, reference)
    if (anyNA(ids)) stop("Every reference row must have a finite leaf assignment.", call. = FALSE)
    terminal <- which(vapply(tree$nodes, function(node) isTRUE(node$is_leaf), logical(1)))
    labels <- vapply(terminal, function(id) {
      action <- .margot_policy_reporting_action_label(tree$action.names[tree$nodes[[id]]$action], label_mapping)
      paste0(action, "\n", formatC(100 * sum(w[ids == id]) / sum(w), format = "f", digits = 1),
        "% weighted\nn = ", sum(ids == id))
    }, character(1))
    decision_tree_args$leaf_metrics <- data.frame(node_id = terminal, label = labels)
  }

  policy_tree_plot <- NULL
  decision_tree_plot <- NULL
  combined_plot <- NULL

  # 1) generate the decision tree at the requested depth
  if (generate_decision_tree) {
    cli::cli_alert_info("Generating decision tree (depth {max_depth})...")
    decision_tree_plot <- do.call(
      margot_plot_decision_tree,
      c(
        list(
          result_object = result_object,
          model_name    = model_name,
          max_depth     = max_depth,
          original_df   = original_df,
          label_mapping = label_mapping
        ),
        decision_tree_args
      )
    )
    cli::cli_alert_success("Decision tree plot generated.")
  }

  # 2) generate the policy tree at the same depth
  if (generate_policy_tree) {
    cli::cli_alert_info("Generating policy tree (depth {max_depth})...")
    policy_tree_plot <- do.call(
      margot_plot_policy_tree,
      c(
        list(
          result_object = result_object,
          model_name = model_name,
          max_depth = max_depth,
          original_df = original_df,
          label_mapping = label_mapping
        ),
        policy_tree_args
      )
    )
    cli::cli_alert_success("Policy tree plot generated.")
  }

  # 3) combine
  if (generate_decision_tree && generate_policy_tree) {
    cli::cli_alert_info("Combining plots...")
    # treat a nested depth-two projection as one labelled panel.
    projection_panel <- if (inherits(policy_tree_plot, "patchwork")) {
      patchwork::wrap_elements(panel = policy_tree_plot)
    } else policy_tree_plot
    combined_plot <- (decision_tree_plot / projection_panel) +
      patchwork::plot_layout(heights = layout$heights) +
      patchwork::plot_annotation(tag_levels = annotation$tag_levels) &
      ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", hjust = 0))
    cli::cli_alert_success("Plots combined successfully.")
  } else if (generate_decision_tree) {
    combined_plot <- decision_tree_plot
  } else if (generate_policy_tree) {
    combined_plot <- policy_tree_plot
  }

  return(list(
    policy_tree   = policy_tree_plot,
    decision_tree = decision_tree_plot,
    combined_plot = combined_plot
  ))
}
