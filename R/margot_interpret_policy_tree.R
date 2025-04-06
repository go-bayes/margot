#' Interpret Policy Tree Results
#'
#' This function creates an interpretation of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates a formatted description of the policy tree, including the main splits and recommended actions.
#'
#' @param model A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to interpret.
#' @param train_proportion numeric value between 0 and 1 for the proportion of data used for training. default is 0.5.
#' @param custom_action_names optional vector of custom names for the actions. must match the number of actions in the policy tree.
#' @param label_mapping optional list that maps variable names to custom labels. used for descriptive labels in the interpretation.
#' @param original_df optional dataframe with untransformed variables, used to display split values on the data scale.
#' @param remove_tx_prefix logical indicating whether to remove prefixes like t0_ from variable names. default is true.
#' @param remove_z_suffix logical indicating whether to remove the _z suffix from variable names. default is true.
#' @param use_title_case logical indicating whether to convert variable names to title case. default is true.
#'
#' @return invisibly returns a string containing the interpretation.
#' the function also prints the interpretation to the console.
#'
#' @examples
#' \dontrun{
#' # create a label mapping
#' label_mapping <- list(
#'   "t2_env_not_climate_chg_concern_z" = "Deny Climate Change Concern",
#'   "t2_env_not_climate_chg_cause_z" = "Deny Humans Cause Climate Change"
#' )
#'
#' # interpret policy tree results with label mapping
#' interpretation <- margot_interpret_policy_tree(
#'   model = models_multi,
#'   model_name = "model_t2_env_not_climate_chg_concern_z",
#'   label_mapping = label_mapping,
#'   original_df = original_df
#' )
#'
#' # print the interpretation
#' cat(interpretation)
#' }
#'
#' @export
margot_interpret_policy_tree <- function(model, model_name, train_proportion = 0.5,
                                         custom_action_names = NULL, label_mapping = NULL,
                                         original_df = NULL, remove_tx_prefix = TRUE,
                                         remove_z_suffix = TRUE, use_title_case = TRUE) {
  cli::cli_alert_info("Starting policy tree interpretation for {model_name}")

  # use the global transform_var_name function
  transform_var <- function(var_name) {
    transform_var_name(var_name, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case)
  }

  # extract policy tree object
  policy_tree_obj <- model$results[[model_name]]$policy_tree_depth_2

  # extract action names and apply custom names if provided
  action_names <- policy_tree_obj$action.names
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != length(action_names)) {
      cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
    }
    action_names <- custom_action_names
    cli::cli_alert_success("Applied custom action names")
  }

  # extract split variables (numeric indices)
  split_variable_indices <- sapply(policy_tree_obj$nodes, function(node) node$split_variable)
  split_variable_indices <- unlist(split_variable_indices)
  split_variable_indices <- split_variable_indices[!is.na(split_variable_indices)]

  # map numeric indices to actual variable names
  if (!is.vector(policy_tree_obj$columns) && !is.character(policy_tree_obj$columns)) {
    cli::cli_abort("policy_tree_obj$columns must be a character vector.")
  }
  split_var_names <- policy_tree_obj$columns[split_variable_indices]

  # generate general interpretation
  general_interpretation <- glue::glue(
    "**Policy Tree Interpretation:** A policy tree uses simple rules (a shallow decision tree) ",
    "to recommend an optimal action. The algorithm uses doubly robust reward estimates ",
    "from a causal forest. We used {train_proportion * 100}% of the data for training and ",
    "the remainder for model evaluation. Each split below shows how distinct subgroups ",
    "are assigned different actions.\n\n"
  )

  cli::cli_alert_success("Generated general interpretation")

  # pull out the nodes for convenience
  nodes <- policy_tree_obj$nodes

  # define helper function to format split values
  format_split_value <- function(var_name, split_val) {
    orig_val <- get_original_value(var_name, split_val, original_df)  # uses your custom function
    if (!is.null(orig_val)) {
      glue::glue("{round(split_val, 3)} (original scale: {orig_val})")
    } else {
      as.character(round(split_val, 3))
    }
  }

  # a depth-2 policy tree has one root split, two children splits, and four leaves
  # root node
  node1 <- nodes[[1]]
  var1 <- split_var_names[1]
  split_val1 <- format_split_value(var1, node1$split_value)

  # left child of root
  node2 <- nodes[[2]]
  var2 <- split_var_names[2]
  split_val2 <- format_split_value(var2, node2$split_value)

  # right child of root
  node3 <- nodes[[3]]
  var3 <- split_var_names[3]
  split_val3 <- format_split_value(var3, node3$split_value)

  # leaves (children of node2 and node3)
  node4 <- nodes[[4]] # left child of node2
  node5 <- nodes[[5]] # right child of node2
  node6 <- nodes[[6]] # left child of node3
  node7 <- nodes[[7]] # right child of node3

  if (length(split_var_names) < 3) {
    cli::cli_abort("Insufficient split variables extracted. Expected at least 3.")
  }

  # create a more paragraph-based interpretation
  # you can adapt the wording further to suit your style
  specific_interpretation <- glue::glue(
    "**Findings for {transform_var(model_name)}:**\n\n",
    "Participants are first split by {transform_var(var1)} at {split_val1}. ",
    "For those with {transform_var(var1)} <= this threshold, the next split is by {transform_var(var2)} at {split_val2}. ",
    "Within that subgroup, individuals with {transform_var(var2)} <= the threshold are recommended **{action_names[node4$action]}**, ",
    "while those with {transform_var(var2)} > the threshold are recommended **{action_names[node5$action]}**.\n\n",
    "For participants with {transform_var(var1)} > {split_val1}, the second split is by {transform_var(var3)} at {split_val3}. ",
    "In this subgroup, individuals with {transform_var(var3)} <= the threshold are recommended **{action_names[node6$action]}**, ",
    "while those with {transform_var(var3)} > the threshold are recommended **{action_names[node7$action]}**.\n\n",
    "This policy tree highlights two key splitting variables (",
    "{transform_var(var1)}, then {transform_var(var2)} or {transform_var(var3)}), ",
    "defining four subgroups that receive different recommended actions."
  )

  cli::cli_alert_success("Generated specific interpretation")

  # combine both parts
  full_interpretation <- glue::glue("{general_interpretation}\n{specific_interpretation}\n")

  # print
  cat(full_interpretation)
  cli::cli_alert_success("Policy tree interpretation completed successfully!")

  # return invisibly
  invisible(full_interpretation)
}

# # old language
# margot_interpret_policy_tree <- function(model, model_name, train_proportion = 0.5,
#                                          custom_action_names = NULL, label_mapping = NULL,
#                                          original_df = NULL, remove_tx_prefix = TRUE,
#                                          remove_z_suffix = TRUE, use_title_case = TRUE) {
#   # Start interpretation
#   cli::cli_alert_info("Starting policy tree interpretation for {model_name}")
#
#   # Use the global transform_var_name function
#   transform_var <- function(var_name) {
#     transform_var_name(var_name, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case)
#   }
#
#   # Extract policy tree object
#   policy_tree_obj <- model$results[[model_name]]$policy_tree_depth_2
#
#   # Extract action names and apply custom names if provided
#   action_names <- policy_tree_obj$action.names
#   if (!is.null(custom_action_names)) {
#     if (length(custom_action_names) != length(action_names)) {
#       cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
#     }
#     action_names <- custom_action_names
#     cli::cli_alert_success("Applied custom action names")
#   }
#
#   # Extract split variables (numeric indices)
#   split_variable_indices <- sapply(policy_tree_obj$nodes, function(node) node$split_variable)
#   split_variable_indices <- unlist(split_variable_indices)  # Ensure it's a numeric vector
#   split_variable_indices <- split_variable_indices[!is.na(split_variable_indices)]
#
#   # Map numeric indices to actual variable names
#   # Ensure policy_tree_obj$columns is a character vector
#   if (!is.vector(policy_tree_obj$columns) && !is.character(policy_tree_obj$columns)) {
#     cli::cli_abort("policy_tree_obj$columns must be a character vector.")
#   }
#
#   split_var_names <- policy_tree_obj$columns[split_variable_indices]
#
#   # Generate general interpretation
#   general_interpretation <- glue::glue(
#     "**Policy Tree Interpretation:** A policy tree obtains simple rule-based policies, ",
#     "where the rule takes the form of a shallow decision tree. The policytree algorithm ",
#     "uses doubly robust reward estimates from a causal forest model to find a shallow, but globally optimal ",
#     "decision tree. We train the model on {train_proportion * 100}% of the data and then ",
#     "evaluate the model on the remainder of the data. The graph helps to clarify whether ",
#     "the leaf node in the test set samples are predicted to have mean outcomes in line ",
#     "with the prescribed policy.\n\n"
#   )
#
#   cli::cli_alert_success("Generated general interpretation")
#
#   # Extract nodes for easier access
#   nodes <- policy_tree_obj$nodes
#
#   # Define helper function to format split value with original scale
#   format_split_value <- function(var_name, split_val) {
#     orig_val <- get_original_value(var_name, split_val, original_df)  # Pass original_df here
#     if (!is.null(orig_val)) {
#       return(glue::glue("at a standardized value of {round(split_val, 3)} (original scale: {orig_val})"))
#     } else {
#       return(glue::glue("at a standardized value of {round(split_val, 3)}"))
#     }
#   }
#
#   # Construct specific_interpretation
#   # Assuming depth 2 tree: root, two children, four leaves.
#
#   # Node 1: root
#   node1 <- nodes[[1]]
#   var1 <- split_var_names[1]
#   split_val1 <- node1$split_value
#   formatted_split_val1 <- format_split_value(var1, split_val1)
#
#   # Node 2: left child of node1
#   node2 <- nodes[[2]]
#   var2 <- split_var_names[2]
#   split_val2 <- node2$split_value
#   formatted_split_val2 <- format_split_value(var2, split_val2)
#
#   # Node 3: right child of node1
#   node3 <- nodes[[3]]
#   var3 <- split_var_names[3]
#   split_val3 <- node3$split_value
#   formatted_split_val3 <- format_split_value(var3, split_val3)
#
#   # Nodes 4-7: leaves
#   node4 <- nodes[[4]] # left child of node2
#   node5 <- nodes[[5]] # right child of node2
#   node6 <- nodes[[6]] # left child of node3
#   node7 <- nodes[[7]] # right child of node3
#
#   # Check if split_var_names has at least 3 variables
#   if (length(split_var_names) < 3) {
#     cli::cli_abort("Insufficient split variables extracted. Expected at least 3.")
#   }
#
#   specific_interpretation <- glue::glue(
#     "**Findings for {transform_var(model_name)}:** \n\n",
#     "1. The first split is based on the variable '{transform_var(var1)}' {formatted_split_val1}.\n\n",
#     "   - If this value is less than or equal to the split value:\n",
#     "     2. The second split is based on '{transform_var(var2)}' {formatted_split_val2}.\n",
#     "        - If this is less than or equal to the split value, the recommended action is: **{action_names[node4$action]}**\n",
#     "        - Otherwise, the recommended action is: **{action_names[node5$action]}**\n\n",
#     "   - If the first split value is greater than the split value:\n",
#     "     3. The second split is based on '{transform_var(var3)}' {formatted_split_val3}.\n",
#     "        - If this is less than or equal to the split value, the recommended action is: **{action_names[node6$action]}**\n",
#     "        - Otherwise, the recommended action is: **{action_names[node7$action]}**\n\n",
#     "This policy tree suggests that an optimal treatment strategy is related to these inflection points in the variables the policy tree identifies, ",
#     "with recommended actions based on subgroups defined by these split points."
#   )
#
#   cli::cli_alert_success("Generated specific interpretation")
#
#   # Combine interpretations
#   full_interpretation <- paste(general_interpretation, specific_interpretation, sep = "\n")
#
#   # Print the full interpretation to the console
#   cat(full_interpretation)
#
#   cli::cli_alert_success("Policy tree interpretation completed successfully! {cli::col_green(cli::symbol$tick)}")
#
#   # Return the full interpretation invisibly
#   invisible(full_interpretation)
# }
