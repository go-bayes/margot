#' Generate Comprehensive Policy Tree Analysis
#'
#' This function provides a comprehensive analysis of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates a policy tree plot, an interpretation of the policy tree, a Qini curve plot, a decision tree visualization, and a policy combo plot.
#'
#' @param mc_test A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to analyze.
#' @param policy_tree_args A list of arguments specifically for margot_plot_policy_tree.
#' @param decision_tree_args A list of arguments specifically for margot_plot_decision_tree.
#'
#' @return A list containing five elements:
#'   \item{policy_tree_plot}{A ggplot object representing the policy tree visualization.}
#'   \item{policy_tree_interpretation}{A string containing the interpretation of the policy tree.}
#'   \item{qini_plot}{A ggplot object representing the Qini curve.}
#'   \item{decision_tree_visualisation}{A ggplot object representing the decision tree structure.}
#'   \item{policy_combo_plot}{A ggplot object representing the policy combo visualisation.}
#'
#' @examples
#' \dontrun{
#' results <- margot_policy_tree(mc_test, "model_t2_log_hours_exercise_z",
#'                               policy_tree_args = list(point_alpha = 0.7),
#'                               decision_tree_args = list(text_size = 4))
#' print(results$policy_tree_plot)
#' cat(results$policy_tree_interpretation)
#' print(results$qini_plot)
#' print(results$decision_tree_visualisation)
#' print(results$policy_combo_plot)
#' }
#'
#' @import ggplot2
#' @export
margot_policy_tree <- function(mc_test, model_name,
                               policy_tree_args = list(),
                               decision_tree_args = list()) {
  # Validation of inputs
  if (!is.list(mc_test) || is.null(mc_test$results)) {
    stop("mc_test must be a list containing a 'results' element.")
  }
  if (!is.character(model_name) || length(model_name) != 1) {
    stop("model_name must be a single character string.")
  }
  if (is.null(mc_test$results[[model_name]])) {
    stop(paste("Model", model_name, "not found in mc_test results."))
  }

  # Helper function to create error plot
  create_error_plot <- function(error_message) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = error_message) +
      theme_void()
  }

  # Call margot_plot_policy_tree()
  policy_tree_plot <- tryCatch({
    do.call(margot_plot_policy_tree,
            c(list(mc_test = mc_test, model_name = model_name),
              policy_tree_args))
  }, error = function(e) {
    warning(paste("Error in generating policy tree plot:", e$message))
    create_error_plot("Error in generating policy tree plot")
  })

  # Call margot_interpret_policy_tree()
  policy_tree_interpretation <- tryCatch({
    margot_interpret_policy_tree(mc_test, model_name)
  }, error = function(e) {
    warning(paste("Error in generating policy tree interpretation:", e$message))
    "Error in generating policy tree interpretation"
  })

  # Call margot_plot_qini()
  qini_plot <- tryCatch({
    margot_plot_qini(mc_test, model_name)
  }, error = function(e) {
    warning(paste("Error in generating Qini plot:", e$message))
    create_error_plot("Error in generating Qini plot")
  })

  # Generate decision tree visualization
  decision_tree_visualisation <- tryCatch({
    do.call(margot_plot_decision_tree,
            c(list(result_object = mc_test, model_name = model_name),
              decision_tree_args))
  }, error = function(e) {
    warning(paste("Error in generating decision tree visualization:", e$message))
    create_error_plot("Error in generating decision tree visualization")
  })

  # Generate policy combo plot
  policy_combo_plot <- tryCatch({
    combo_result <- margot_plot_policy_combo(
      result_object = mc_test,
      model_name = model_name,
      policy_tree_args = policy_tree_args,
      decision_tree_args = decision_tree_args
    )
    if (is.list(combo_result) && "combined_plot" %in% names(combo_result)) {
      combo_result$combined_plot
    } else {
      combo_result
    }
  }, error = function(e) {
    warning(paste("Error in generating policy combo plot:", e$message))
    create_error_plot("Error in generating policy combo plot")
  })

  # Return a list containing all five outputs
  results <- list(
    policy_tree_plot = policy_tree_plot,
    policy_tree_interpretation = policy_tree_interpretation,
    qini_plot = qini_plot,
    decision_tree_visualisation = decision_tree_visualisation,
    policy_combo_plot = policy_combo_plot
  )

  return(results)
}
