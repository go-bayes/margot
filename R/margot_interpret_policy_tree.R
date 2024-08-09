#' Interpret Policy Tree Results
#'
#' This function creates an interpretation of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates a formatted description of the policy tree, including the main splits and recommended actions.
#'
#' @param mc_test A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to interpret.
#' @param train_proportion Numeric value between 0 and 1 for the proportion of data used for training. Default is 0.8.
#' @param custom_action_names Optional vector of custom names for the actions. Must match the number of actions in the policy tree.
#' @param format A string specifying the output format. Either "markdown" (default) or "latex".
#'
#' @return Invisibly returns a string containing the interpretation in the specified format.
#'   The function also prints the interpretation to the console.
#'
#' @examples
#' \dontrun{
#' # For markdown output (default):
#' margot_interpret_policy_tree(result_outcomes_health, "model_t2_log_hours_exercise_z")
#'
#' # For LaTeX output:
#' margot_interpret_policy_tree(result_outcomes_health, "model_t2_log_hours_exercise_z", format = "latex")
#'
#' # To store the interpretation in a variable:
#' interpretation <- margot_interpret_policy_tree(result_outcomes_health, "model_t2_log_hours_exercise_z")
#' }
#'
#' @export
margot_interpret_policy_tree <- function(mc_test, model_name, train_proportion = 0.8,
                                         custom_action_names = NULL, format = "markdown") {
  # Extract policy tree object
  policy_tree_obj <- mc_test$results[[model_name]]$policy_tree_depth_2

  # Extract action names from the policy tree object
  action_names <- policy_tree_obj$action.names

  # If custom action names are provided, use them instead
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != length(action_names)) {
      stop("The number of custom action names must match the number of actions in the policy tree.")
    }
    action_names <- custom_action_names
  }

  # Extract the plot data for the specified model
  plot_data <- mc_test$results[[model_name]]$plot_data

  # Extract X_test and split_variables
  X_test <- plot_data$X_test

  # Extract split variables and their values from the policy tree
  policy_tree <- policy_tree_obj$nodes
  actual_columns <- names(X_test)

  if (format == "markdown"){
    general_interpretation <- paste0(
      "**Policy Tree Interpretation:** ",
      "A policy tree obtains simple rule-based policies, where the rule takes the form of a shallow decision tree. ",
      "The policytree algorithm uses doubly robust reward estimates from grf to find a shallow, but globally optimal decision tree. ",
      sprintf("We train the model on %.0f%% of the data and then evaluate the model on the remainder of the data. ", train_proportion * 100),
      "The graph helps to clarify whether the leaf node in the test set samples are predicted to have mean outcomes in line with the prescribed policy.\n\n"
    )

    specific_interpretation <- paste0(
      "**Findings for ", model_name, ":** \n\n",
      "1. The first split is based on the variable '", actual_columns[policy_tree[[1]]$split_variable], "' at a value of ", round(policy_tree[[1]]$split_value, 4), ".\n\n",
      "   - If this value is less than or equal to the split value:\n",
      "     2. The second split is based on '", actual_columns[policy_tree[[2]]$split_variable], "' at ", round(policy_tree[[2]]$split_value, 4), ".\n",
      "        - If this is less than or equal to the split value, the recommended action is: **", action_names[policy_tree[[4]]$action], "**\n",
      "        - Otherwise, the recommended action is: **", action_names[policy_tree[[5]]$action], "**\n\n",
      "   - If the first split value is greater than the split value:\n",
      "     3. The second split is based on '", actual_columns[policy_tree[[3]]$split_variable], "' at ", round(policy_tree[[3]]$split_value, 4), ".\n",
      "        - If this is less than or equal to the split value, the recommended action is: **", action_names[policy_tree[[6]]$action], "**\n",
      "        - Otherwise, the recommended action is: **", action_names[policy_tree[[7]]$action], "**\n\n",
      "This policy tree suggests that an optimal treatment strategy is related to these inflection points in the variables the policy tree identifies, ",
      "with recommended actions based on subgroups defined by these split points."
    )
  } else if (format == "latex") {
    general_interpretation <- paste0(
      "\\textbf{Policy Tree Interpretation:} ",
      "A policy tree obtains simple rule-based policies, where the rule takes the form of a shallow decision tree. ",
      "The policytree algorithm uses doubly robust reward estimates from grf to find a shallow, but globally optimal decision tree. ",
      sprintf("We train the model on %.0f\\%% of the data and then evaluate the model on the remainder of the data. ", train_proportion * 100),
      "The graph helps to clarify whether the leaf node in the test set samples are predicted to have mean outcomes in line with the prescribed policy.\n\n"
    )

    specific_interpretation <- paste0(
      "\\textbf{Findings for ", model_name, ":}\n\n",
      "\\begin{enumerate}\n",
      "\\item The first split is based on the variable '", actual_columns[policy_tree[[1]]$split_variable], "' at a value of ", round(policy_tree[[1]]$split_value, 4), ".\n\n",
      "  \\begin{itemize}\n",
      "  \\item If this value is less than or equal to the split value:\n",
      "    \\begin{enumerate}[resume]\n",
      "    \\item The second split is based on '", actual_columns[policy_tree[[2]]$split_variable], "' at ", round(policy_tree[[2]]$split_value, 4), ".\n",
      "      \\begin{itemize}\n",
      "      \\item If this is less than or equal to the split value, the recommended action is: \\textbf{", action_names[policy_tree[[4]]$action], "}\n",
      "      \\item Otherwise, the recommended action is: \\textbf{", action_names[policy_tree[[5]]$action], "}\n",
      "      \\end{itemize}\n",
      "    \\end{enumerate}\n",
      "  \\item If the first split value is greater than the split value:\n",
      "    \\begin{enumerate}[resume]\n",
      "    \\item The second split is based on '", actual_columns[policy_tree[[3]]$split_variable], "' at ", round(policy_tree[[3]]$split_value, 4), ".\n",
      "      \\begin{itemize}\n",
      "      \\item If this is less than or equal to the split value, the recommended action is: \\textbf{", action_names[policy_tree[[6]]$action], "}\n",
      "      \\item Otherwise, the recommended action is: \\textbf{", action_names[policy_tree[[7]]$action], "}\n",
      "      \\end{itemize}\n",
      "    \\end{enumerate}\n",
      "  \\end{itemize}\n",
      "\\end{enumerate}\n\n",
      "This policy tree suggests that an optimal treatment strategy is related to these inflection points in the variables the policy tree identifies, ",
      "with recommended actions based on subgroups defined by these split points."
    )
  } else {
    stop("Invalid format. Please choose 'markdown' or 'latex'.")
  }

  interpretation <- list(
    general_interpretation = general_interpretation,
    specific_interpretation = specific_interpretation
  )

  # Combine general and specific interpretations
  full_interpretation <- paste0(interpretation$general_interpretation, "\n\n",
                                interpretation$specific_interpretation)

  # Print the full interpretation to the console
  cat(full_interpretation)

  # Return the interpretation invisibly
  invisible(interpretation)
}
