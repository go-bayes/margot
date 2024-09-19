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
#' @param label_mapping Optional named list for custom label mappings. Keys should be original variable names
#'        (with or without "model_" prefix), and values should be the desired display labels. Default is NULL.
#'
#' @return A list containing:
#'   \item{policy_tree}{A ggplot object representing the policy tree (if generated)}
#'   \item{decision_tree}{A ggplot object representing the decision tree (if generated)}
#'   \item{combined_plot}{A ggplot object representing the combined plot (if both plots are generated)}
#'
#' @import ggplot2
#' @import patchwork
#'
#' @examples
#' \dontrun{
#' # Assuming 'results' is your Margot results object and 'model_name' is the name of your model
#' label_mapping <- list(
#'   "t2_env_not_env_efficacy_z" = "Deny Personal Environmental Efficacy",
#'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real"
#' )
#' combined_plot <- margot_plot_policy_combo(
#'   result_object = results,
#'   model_name = "model_t2_env_not_env_efficacy_z",
#'   policy_tree_args = list(point_alpha = 0.75),
#'   decision_tree_args = list(text_size = 4.5, edge_label_offset = 0.01),
#'   label_mapping = label_mapping
#' )
#'
#' # Print the combined plot
#' print(combined_plot$combined_plot)
#' }
#' @export
margot_plot_policy_combo <- function(result_object, model_name,
                                     layout = list(heights = c(1, 2)),
                                     annotation = list(tag_levels = "A"),
                                     generate_policy_tree = TRUE,
                                     generate_decision_tree = TRUE,
                                     policy_tree_args = list(),
                                     decision_tree_args = list(),
                                     label_mapping = NULL) {
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
  policy_results <- NULL
  decision_tree <- NULL

  if (generate_policy_tree) {
    cat("Generating policy tree...\n")
    policy_results <- do.call(margot_plot_policy_tree,
                              c(list(mc_test = result_object,
                                     model_name = model_name,
                                     label_mapping = label_mapping),
                                policy_tree_args))
  }

  if (generate_decision_tree) {
    cat("Generating decision tree...\n")
    decision_tree <- do.call(margot_plot_decision_tree,
                             c(list(result_object = result_object,
                                    model_name = model_name,
                                    label_mapping = label_mapping),
                               decision_tree_args))
  }

  if (generate_policy_tree && generate_decision_tree) {
    cat("Combining plots...\n")
    combined_plot <- decision_tree / policy_results

    cat("Applying layout and annotation...\n")
    combined_plot <- combined_plot +
      do.call(patchwork::plot_layout, layout) +
      do.call(patchwork::plot_annotation, annotation)
  } else if (generate_policy_tree) {
    combined_plot <- policy_results
  } else if (generate_decision_tree) {
    combined_plot <- decision_tree
  }

  return(list(
    policy_tree = policy_results,
    decision_tree = decision_tree,
    combined_plot = combined_plot
  ))
}
# margot_plot_policy_combo <- function(result_object, model_name,
#                                      layout = list(heights = c(1, 2)),
#                                      annotation = list(tag_levels = "A"),
#                                      generate_policy_tree = TRUE,
#                                      generate_decision_tree = TRUE,
#                                      policy_tree_args = list(),
#                                      decision_tree_args = list()) {
#   # Input validation for result_object
#   if (!is.list(result_object) || !("results" %in% names(result_object))) {
#     stop("result_object must be a list containing a 'results' element.")
#   }
#
#   # Input validation for model_name
#   if (!is.character(model_name) || length(model_name) != 1) {
#     stop("model_name must be a single character string.")
#   }
#
#   # Check if the specified model exists in the result_object
#   if (!(model_name %in% names(result_object$results))) {
#     stop(paste("The specified model_name '", model_name, "' does not exist in the result_object."))
#   }
#
#   # Check if at least one plot type is selected for generation
#   if (!generate_policy_tree && !generate_decision_tree) {
#     stop("At least one of generate_policy_tree or generate_decision_tree must be TRUE.")
#   }
#
#   # Check if required functions exist
#   required_functions <- c("margot_plot_policy_tree", "margot_plot_decision_tree")
#   missing_functions <- required_functions[!sapply(required_functions, exists, mode = "function")]
#
#   if (length(missing_functions) > 0) {
#     stop("The following required functions are not found: ",
#          paste(missing_functions, collapse = ", "),
#          ". Please make sure these functions are loaded.")
#   }
#
#   combined_plot <- NULL
#
#   if (generate_policy_tree && generate_decision_tree) {
#     cat("Generating policy tree...\n")
#     policy_results <- do.call(margot_plot_policy_tree,
#                               c(list(mc_test = result_object, model_name = model_name),
#                                 policy_tree_args))
#
#     cat("Generating decision tree...\n")
#     decision_tree <- do.call(margot_plot_decision_tree,
#                              c(list(result_object = result_object, model_name = model_name),
#                                decision_tree_args))
#
#     cat("Combining plots...\n")
#     combined_plot <- decision_tree / policy_results
#
#     cat("Applying layout and annotation...\n")
#     combined_plot <- combined_plot +
#       do.call(patchwork::plot_layout, layout) +
#       do.call(patchwork::plot_annotation, annotation)
#   } else if (generate_policy_tree) {
#     cat("Generating policy tree...\n")
#     combined_plot <- do.call(margot_plot_policy_tree,
#                              c(list(mc_test = result_object, model_name = model_name),
#                                policy_tree_args))
#   } else if (generate_decision_tree) {
#     cat("Generating decision tree...\n")
#     combined_plot <- do.call(margot_plot_decision_tree,
#                              c(list(result_object = result_object, model_name = model_name),
#                                decision_tree_args))
#   }
#
#   return(combined_plot)
# }
