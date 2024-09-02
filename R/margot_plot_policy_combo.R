#' Create a Combined Decision Tree and Policy Relationship Graph
#'
#' This function generates a combined plot consisting of a decision tree and a graph
#' showing relationships between variables in the recommended policy.
#'
#' @param result_object An object containing the results from a multi-arm causal forest model.
#' @param model_name A character string specifying the name of the model.
#' @param layout A list specifying the layout of the combined plot. Default is
#'   `list(heights = c(1, 2))`, which sets the relative heights of the two plots.
#' @param annotation A list specifying the annotation for the combined plot. Default is
#'   `list(tag_levels = "A")`, which adds alphabetic tags to the subplots.
#' @param generate_policy_tree Logical, whether to generate the policy tree plot. Default is TRUE.
#' @param generate_decision_tree Logical, whether to generate the decision tree plot. Default is TRUE.
#' @param policy_tree_args A list of arguments to pass to margot_plot_policy_tree. Default is list().
#' @param decision_tree_args A list of arguments to pass to margot_plot_decision_tree. Default is list().
#'
#' @return A list containing:
#'   \item{policy_tree}{A ggplot object representing the policy tree (if generated)}
#'   \item{decision_tree}{A ggplot object representing the decision tree (if generated)}
#'   \item{combined_plot}{A ggplot object representing the combined plot (if both plots are generated)}
#'
#' @details
#' This function performs the following steps:
#' 1. Generates policy results using `margot_plot_policy_tree` (if requested)
#' 2. Creates a decision tree plot using `margot_plot_decision_tree` (if requested)
#' 3. Combines the plots using the patchwork package (if both plots are generated)
#' 4. Applies the specified layout and annotation to the combined plot
#'
#' Default arguments for margot_plot_policy_tree:
#' - color_scale = NULL
#' - point_alpha = 0.5
#' - theme_function = theme_classic
#' - title_size = 14
#' - subtitle_size = 12
#' - axis_title_size = 10
#' - legend_title_size = 10
#' - jitter_width = 0.3
#' - jitter_height = 0.3
#' - split_line_color = "darkgray"
#' - split_line_alpha = 0.7
#' - split_line_type = "dashed"
#' - split_line_linewidth = 0.5
#' - split_label_size = 10
#' - split_label_color = "darkgray"
#' - custom_action_names = NULL
#' - legend_position = "bottom"
#' - plot_selection = "both"
#'
#' Default arguments for margot_plot_decision_tree:
#' - x_padding = 0.25
#' - y_padding = 0.25
#' - border_size = 0.35
#' - edge_label_offset = 0.02
#' - span_ratio = 0.25
#' - text_size = 3.5
#' - non_leaf_fill = "lightyellow"
#' - title = NULL
#' - leaf_palette = NULL
#'
#' @import ggplot2
#' @import patchwork
#'
#' @examples
#' \dontrun{
#' # Assuming result_object is already defined:
#' combined_plot <- margot_plot_policy_combo(
#'   result_object = result_object,
#'   model_name = "model_t2_hlth_sleep_hours_z",
#'   policy_tree_args = list(point_alpha = 0.75),
#'   decision_tree_args = list(text_size = 4.5, edge_label_offset = 0.01)
#' )
#'
#' # Print the combined plot
#' print(combined_plot)
#' }
#' @export
margot_plot_policy_combo <- function(result_object, model_name,
                                     layout = list(heights = c(1, 2)),
                                     annotation = list(tag_levels = "A"),
                                     generate_policy_tree = TRUE,
                                     generate_decision_tree = TRUE,
                                     policy_tree_args = list(),
                                     decision_tree_args = list()) {
  # Input validation for result_object
  if (!is.list(result_object) || !("results" %in% names(result_object))) {
    stop("result_object must be a list containing a 'results' element.")
  }

  # Input validation for model_name
  if (!is.character(model_name) || length(model_name) != 1) {
    stop("model_name must be a single character string.")
  }

  # Check if the specified model exists in the result_object
  if (!(model_name %in% names(result_object$results))) {
    stop(paste("The specified model_name '", model_name, "' does not exist in the result_object."))
  }

  # Check if at least one plot type is selected for generation
  if (!generate_policy_tree && !generate_decision_tree) {
    stop("At least one of generate_policy_tree or generate_decision_tree must be TRUE.")
  }

  # Check if required functions exist
  required_functions <- c("margot_plot_policy_tree", "margot_plot_decision_tree")
  missing_functions <- required_functions[!sapply(required_functions, exists, mode = "function")]

  if (length(missing_functions) > 0) {
    stop("The following required functions are not found: ",
         paste(missing_functions, collapse = ", "),
         ". Please make sure these functions are loaded.")
  }

  combined_plot <- NULL

  if (generate_policy_tree && generate_decision_tree) {
    cat("Generating policy tree...\n")
    policy_results <- do.call(margot_plot_policy_tree,
                              c(list(mc_test = result_object, model_name = model_name),
                                policy_tree_args))

    cat("Generating decision tree...\n")
    decision_tree <- do.call(margot_plot_decision_tree,
                             c(list(result_object = result_object, model_name = model_name),
                               decision_tree_args))

    cat("Combining plots...\n")
    combined_plot <- decision_tree / policy_results

    cat("Applying layout and annotation...\n")
    combined_plot <- combined_plot +
      do.call(patchwork::plot_layout, layout) +
      do.call(patchwork::plot_annotation, annotation)
  } else if (generate_policy_tree) {
    cat("Generating policy tree...\n")
    combined_plot <- do.call(margot_plot_policy_tree,
                             c(list(mc_test = result_object, model_name = model_name),
                               policy_tree_args))
  } else if (generate_decision_tree) {
    cat("Generating decision tree...\n")
    combined_plot <- do.call(margot_plot_decision_tree,
                             c(list(result_object = result_object, model_name = model_name),
                               decision_tree_args))
  }

  return(combined_plot)
}
