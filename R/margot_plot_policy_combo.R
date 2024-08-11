#' Create a Combined Decision Tree and Policy Relationship Graph
#'
#' This function generates a combined plot consisting of a decision tree and a graph
#' showing relationships between variables in the recommended policy.
#'
#' @param mc_test An object containing Monte Carlo test results (exact type/class to be specified)
#' @param model_name A character string specifying the name of the model
#' @param layout A list specifying the layout of the combined plot. Default is
#'   `list(heights = c(1, 2))`, which sets the relative heights of the two plots.
#' @param annotation A list specifying the annotation for the combined plot. Default is
#'   `list(tag_levels = "A")`, which adds alphabetic tags to the subplots.
#'
#' @return A list containing:
#'   \item{plot}{A ggplot2 object representing the combined decision tree and policy relationship graph}
#'
#' @details
#' This function performs the following steps:
#' 1. Generates policy results using `margot_plot_policy_tree`
#' 2. Creates a decision tree plot using `margot_plot_decision_tree`
#' 3. Combines the plots using the patchwork package
#' 4. Applies the specified layout and annotation to the combined plot
#'
#' @import ggplot2
#' @import patchwork
#'
#' @seealso
#' \code{\link{margot_plot_policy_tree}}, \code{\link{margot_plot_decision_tree}}
#'
#' @examples
#' # Assuming mc_test and model_name are already defined:
#' result <- margot_plot_policy_combo(mc_test, "MyModel")
#' print(result$plot)
#'
#' @export
margot_plot_policy_combo <- function(mc_test, model_name,
                                     layout = list(heights = c(1, 2)),
                                     annotation = list(tag_levels = "A")) {

  # Get results from margot_policy_tree
  policy_results <- margot_plot_policy_tree(mc_test, model_name)

  # Get decision tree plot
  decision_tree <- margot_plot_decision_tree(mc_test, model_name)

  # Combine all plots
  combined_plot <- decision_tree/ policy_results

  # Apply layout and annotation
  final_plot <- combined_plot +
    do.call(patchwork::plot_layout, layout) +
    do.call(patchwork::plot_annotation, annotation)

  # Return both the combined plot and the interpretation
  return(list(
    plot = final_plot
  ))
}
