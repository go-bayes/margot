#' Generate Comprehensive Policy Tree Analysis
#'
#' This function provides a comprehensive analysis of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates a policy tree plot, an interpretation of the policy tree, a Qini curve plot, and a decision tree visualization.
#'
#' @param mc_test A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to analyze.
#' @param ... Additional arguments passed to margot_plot_policy_tree and margot_interpret_policy_tree functions.
#'
#' @return A list containing four elements:
#'   \item{policy_tree_plot}{A ggplot object representing the policy tree visualization.}
#'   \item{policy_tree_interpretation}{A string containing the interpretation of the policy tree.}
#'   \item{qini_plot}{A ggplot object representing the Qini curve.}
#'   \item{decision_tree_visualisation}{A grViz object (DiagrammeR) representing the decision tree structure.}
#'
#' @examples
#' \dontrun{
#' results <- margot_policy_tree(mc_test, "model_t2_log_hours_exercise_z")
#' print(results$policy_tree_plot)
#' cat(results$policy_tree_interpretation)
#' print(results$qini_plot)
#' print(results$decision_tree_visualisation)
#' }
#'
#' @import DiagrammeR
#' @import policytree
#' @importFrom ggplot2 ggplot
#' @export
margot_policy_tree <- function(mc_test, model_name, ...) {
  # Input validation
  if (!is.list(mc_test) || is.null(mc_test$results)) {
    stop("mc_test must be a list containing a 'results' element.")
  }

  if (!is.character(model_name) || length(model_name) != 1) {
    stop("model_name must be a single character string.")
  }

  if (is.null(mc_test$results[[model_name]])) {
    stop(paste("Model", model_name, "not found in mc_test results."))
  }

  # Call margot_plot_policy_tree()
  tryCatch({
    policy_tree_plot <- margot_plot_policy_tree(mc_test, model_name, ...)
  }, error = function(e) {
    warning(paste("Error in generating policy tree plot:", e$message))
    policy_tree_plot <- NULL
  })

  # Call margot_interpret_policy_tree()
  tryCatch({
    policy_tree_interpretation <- margot_interpret_policy_tree(mc_test, model_name, ...)
  }, error = function(e) {
    warning(paste("Error in generating policy tree interpretation:", e$message))
    policy_tree_interpretation <- NULL
  })

  # Call margot_plot_qini()
  tryCatch({
    qini_plot <- margot_plot_qini(mc_test, model_name)
  }, error = function(e) {
    warning(paste("Error in generating Qini plot:", e$message))
    qini_plot <- NULL
  })

  # Generate decision tree visualization
  tryCatch({
    decision_tree_visualisation <- plot(mc_test$results[[model_name]]$policy_tree_depth_2)
  }, error = function(e) {
    warning(paste("Error in generating decision tree visualization:", e$message))
    decision_tree_visualisation <- NULL
  })

  # Return a list containing all four outputs
  results <- list(
    policy_tree_plot = policy_tree_plot,
    policy_tree_interpretation = policy_tree_interpretation,
    qini_plot = qini_plot,
    decision_tree_visualisation = decision_tree_visualisation
  )

  # Check if any of the results are NULL and warn the user
  null_results <- names(results)[sapply(results, is.null)]
  if (length(null_results) > 0) {
    warning(paste("The following outputs could not be generated:", paste(null_results, collapse = ", ")))
  }

  return(results)
}
