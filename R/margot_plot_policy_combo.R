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
#' @param layout A list specifying the layout of the combined plot when max_depth==2. Default is
#'   `list(heights = c(1, 2))`, which sets the relative heights of the two plots.
#' @param annotation A list specifying the annotation for the combined plot when max_depth==2. Default is
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
                                     max_depth             = 2L,
                                     label_mapping         = NULL,
                                     original_df           = NULL,
                                     layout                = list(heights = c(1, 2)),
                                     annotation            = list(tag_levels = "A"),
                                     generate_policy_tree  = TRUE,
                                     generate_decision_tree= TRUE,
                                     policy_tree_args      = list(),
                                     decision_tree_args    = list()) {
  cli::cli_h1("Margot Plot Policy Combo")

  # validate inputs (omitted here for brevity)...

  policy_tree_plot  <- NULL
  decision_tree_plot<- NULL
  combined_plot     <- NULL

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
          result_object       = result_object,
          model_name    = model_name,
          max_depth     = max_depth,
          original_df   = original_df,
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
    if (max_depth == 1L) {
      # simple stack, equal heights, no tags
      combined_plot <- decision_tree_plot / policy_tree_plot
    } else {
      # previous two‐part layout with annotation
      combined_plot <- (decision_tree_plot / policy_tree_plot) +
        patchwork::plot_layout(heights = layout$heights) +
        patchwork::plot_annotation(tag_levels = annotation$tag_levels)
    }
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
