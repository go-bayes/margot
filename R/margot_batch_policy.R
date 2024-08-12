#' Apply Policy Tree Analysis to Multiple Models
#'
#' This function applies the `margot_policy_tree()` function to multiple models
#' contained within a single results object. It generates policy tree plots,
#' interpretations, Qini plots, decision tree visualizations, and policy combo
#' plots for each model.
#'
#' @param result_outcomes A list containing the results of multiple models.
#'   This object should have a structure similar to the output of a multi-arm
#'   causal forest model, with a `results` element containing named model results.
#' @param policy_tree_args A list of arguments to be passed to `margot_plot_policy_tree()`.
#' @param decision_tree_args A list of arguments to be passed to `margot_plot_decision_tree()`.
#' @param ... Additional arguments to be passed to `margot_policy_tree()`.
#'
#' @return A list where each element corresponds to a model in the input
#'   `result_outcomes`. Each element is itself a list containing:
#'   \item{policy_tree_plot}{A ggplot object of the policy tree plot}
#'   \item{policy_tree_interpretation}{A character string interpreting the policy tree}
#'   \item{qini_plot}{A ggplot object of the Qini plot}
#'   \item{decision_tree_visualisation}{A ggplot object visualizing the decision tree}
#'   \item{policy_combo_plot}{A ggplot object of the policy combo plot}
#'
#' @examples
#' \dontrun{
#' # Assuming result_outcomes_health contains multiple model results
#' batch_results <- margot_batch_policy(
#'   result_outcomes_health,
#'   policy_tree_args = list(point_alpha = 0.7),
#'   decision_tree_args = list(text_size = 4)
#' )
#'
#' # To access results for a specific model:
#' smoker_results <- batch_results$model_t2_smoker_binary
#'
#' # To view the policy tree plot for this model:
#' print(smoker_results$policy_tree_plot)
#' }
#'
#' @export
margot_batch_policy <- function(result_outcomes,
                                policy_tree_args = list(),
                                decision_tree_args = list(),
                                ...) {
  # Ensure the margot package is loaded
  if (!requireNamespace("margot", quietly = TRUE)) {
    stop("Package 'margot' is required but not installed. Please install it first.")
  }

  # Extract the names of the models from the results list
  model_names <- names(result_outcomes$results)

  # Initialize an empty list to store the results
  output_list <- list()

  # Loop through each model
  for (model_name in model_names) {
    # Apply margot_policy_tree() to each model
    output_list[[model_name]] <- margot_policy_tree(
      mc_test = result_outcomes,
      model_name = model_name,
      policy_tree_args = policy_tree_args,
      decision_tree_args = decision_tree_args,
      ...
    )
  }

  return(output_list)
}
# old
# margot_batch_policy <- function(result_outcomes, ...) {
#   # Ensure the margot package is loaded
#   if (!requireNamespace("margot", quietly = TRUE)) {
#     stop("Package 'margot' is required but not installed. Please install it first.")
#   }
#
#   # Extract the names of the models from the results list
#   model_names <- names(result_outcomes$results)
#
#   # Initialize an empty list to store the results
#   output_list <- list()
#
#   # Loop through each model
#   for (model_name in model_names) {
#     # Apply margot_policy_tree() to each model
#     output_list[[model_name]] <- margot_policy_tree(
#       result_outcomes,
#       model_name,
#       ...
#     )
#   }
#
#   return(output_list)
# }
