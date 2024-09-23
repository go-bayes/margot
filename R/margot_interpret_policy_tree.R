#' Interpret Policy Tree Results
#'
#' This function creates an interpretation of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates a formatted description of the policy tree, including the main splits and recommended actions.
#'
#' @param model A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to interpret.
#' @param train_proportion numeric value between 0 and 1 for the proportion of data used for training. default is 0.7.
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
#' interpretation <- margot_interpret_policy_tree_new(
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
margot_interpret_policy_tree <- function(model, model_name, train_proportion = 0.7,
                                         custom_action_names = NULL, label_mapping = NULL,
                                         original_df = NULL, remove_tx_prefix = TRUE,
                                         remove_z_suffix = TRUE, use_title_case = TRUE) {
  # Start interpretation
  cli::cli_alert_info("Starting policy tree interpretation for {model_name}")

  # Use the global transform_var_name function
  transform_var <- function(var_name) {
    transform_var_name(var_name, label_mapping, remove_tx_prefix, remove_z_suffix, use_title_case)
  }

  # Extract policy tree object
  policy_tree_obj <- model$results[[model_name]]$policy_tree_depth_2

  # Extract action names and apply custom names if provided
  action_names <- policy_tree_obj$action.names
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != length(action_names)) {
      cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
    }
    action_names <- custom_action_names
    cli::cli_alert_success("Applied custom action names")
  }

  # Extract split variables (numeric indices)
  split_variable_indices <- sapply(policy_tree_obj$nodes, function(node) node$split_variable)
  split_variable_indices <- unlist(split_variable_indices)  # Ensure it's a numeric vector
  split_variable_indices <- split_variable_indices[!is.na(split_variable_indices)]

  # Map numeric indices to actual variable names
  # Ensure policy_tree_obj$columns is a character vector
  if (!is.vector(policy_tree_obj$columns) && !is.character(policy_tree_obj$columns)) {
    cli::cli_abort("policy_tree_obj$columns must be a character vector.")
  }

  split_var_names <- policy_tree_obj$columns[split_variable_indices]

  # Generate general interpretation
  general_interpretation <- glue::glue(
    "**Policy Tree Interpretation:** A policy tree obtains simple rule-based policies, ",
    "where the rule takes the form of a shallow decision tree. The policytree algorithm ",
    "uses doubly robust reward estimates from grf to find a shallow, but globally optimal ",
    "decision tree. We train the model on {train_proportion * 100}% of the data and then ",
    "evaluate the model on the remainder of the data. The graph helps to clarify whether ",
    "the leaf node in the test set samples are predicted to have mean outcomes in line ",
    "with the prescribed policy.\n\n"
  )

  cli::cli_alert_success("Generated general interpretation")

  # Extract nodes for easier access
  nodes <- policy_tree_obj$nodes

  # Define helper function to format split value with original scale
  format_split_value <- function(var_name, split_val) {
    orig_val <- get_original_value(var_name, split_val, original_df)  # Pass original_df here
    if (!is.null(orig_val)) {
      return(glue::glue("at a standardized value of {round(split_val, 3)} (original scale: {orig_val})"))
    } else {
      return(glue::glue("at a standardized value of {round(split_val, 3)}"))
    }
  }

  # Construct specific_interpretation
  # Assuming depth 2 tree: root, two children, four leaves.

  # Node 1: root
  node1 <- nodes[[1]]
  var1 <- split_var_names[1]
  split_val1 <- node1$split_value
  formatted_split_val1 <- format_split_value(var1, split_val1)

  # Node 2: left child of node1
  node2 <- nodes[[2]]
  var2 <- split_var_names[2]
  split_val2 <- node2$split_value
  formatted_split_val2 <- format_split_value(var2, split_val2)

  # Node 3: right child of node1
  node3 <- nodes[[3]]
  var3 <- split_var_names[3]
  split_val3 <- node3$split_value
  formatted_split_val3 <- format_split_value(var3, split_val3)

  # Nodes 4-7: leaves
  node4 <- nodes[[4]] # left child of node2
  node5 <- nodes[[5]] # right child of node2
  node6 <- nodes[[6]] # left child of node3
  node7 <- nodes[[7]] # right child of node3

  # Check if split_var_names has at least 3 variables
  if (length(split_var_names) < 3) {
    cli::cli_abort("Insufficient split variables extracted. Expected at least 3.")
  }

  specific_interpretation <- glue::glue(
    "**Findings for {transform_var(model_name)}:** \n\n",
    "1. The first split is based on the variable '{transform_var(var1)}' {formatted_split_val1}.\n\n",
    "   - If this value is less than or equal to the split value:\n",
    "     2. The second split is based on '{transform_var(var2)}' {formatted_split_val2}.\n",
    "        - If this is less than or equal to the split value, the recommended action is: **{action_names[node4$action]}**\n",
    "        - Otherwise, the recommended action is: **{action_names[node5$action]}**\n\n",
    "   - If the first split value is greater than the split value:\n",
    "     3. The second split is based on '{transform_var(var3)}' {formatted_split_val3}.\n",
    "        - If this is less than or equal to the split value, the recommended action is: **{action_names[node6$action]}**\n",
    "        - Otherwise, the recommended action is: **{action_names[node7$action]}**\n\n",
    "This policy tree suggests that an optimal treatment strategy is related to these inflection points in the variables the policy tree identifies, ",
    "with recommended actions based on subgroups defined by these split points."
  )

  cli::cli_alert_success("Generated specific interpretation")

  # Combine interpretations
  full_interpretation <- paste(general_interpretation, specific_interpretation, sep = "\n")

  # Print the full interpretation to the console
  cat(full_interpretation)

  cli::cli_alert_success("Policy tree interpretation completed successfully! {cli::col_green(cli::symbol$tick)}")

  # Return the full interpretation invisibly
  invisible(full_interpretation)
}
#' margot_interpret_policy_tree <- function(model, model_name, train_proportion = 0.7,
#'                                          custom_action_names = NULL, label_mapping = NULL,
#'                                          original_df = NULL, remove_tx_prefix = TRUE,
#'                                          remove_z_suffix = TRUE, use_title_case = TRUE) {
#'   # Start interpretation
#'   cli::cli_alert_info("Starting policy tree interpretation for {model_name}")
#'
#'   # Extract policy tree object
#'   policy_tree_obj <- model$results[[model_name]]$policy_tree_depth_2
#'
#'   # Extract action names and apply custom names if provided
#'   action_names <- policy_tree_obj$action.names
#'   if (!is.null(custom_action_names)) {
#'     if (length(custom_action_names) != length(action_names)) {
#'       cli::cli_abort("The number of custom action names must match the number of actions in the policy tree.")
#'     }
#'     action_names <- custom_action_names
#'     cli::cli_alert_success("Applied custom action names")
#'   }
#'
#'   # Extract split variables (numeric indices)
#'   split_variable_indices <- sapply(policy_tree_obj$nodes, function(node) node$split_variable)
#'   split_variable_indices <- unlist(split_variable_indices)  # Ensure it's a numeric vector
#'   split_variable_indices <- split_variable_indices[!is.na(split_variable_indices)]
#'
#'   # Map numeric indices to actual variable names
#'   # Ensure policy_tree_obj$columns is a character vector
#'   if (!is.vector(policy_tree_obj$columns) && !is.character(policy_tree_obj$columns)) {
#'     cli::cli_abort("policy_tree_obj$columns must be a character vector.")
#'   }
#'
#'   split_var_names <- policy_tree_obj$columns[split_variable_indices]
#'
#'   # Function to transform variable names for display
#'   transform_var_name <- function(var_name) {
#'     display_name <- var_name
#'
#'     # Remove 'model_' prefix if present (for model names)
#'     if (startsWith(display_name, "model_")) {
#'       display_name <- sub("^model_", "", display_name)
#'     }
#'
#'     # Apply label mapping first, if exists
#'     if (!is.null(label_mapping) && display_name %in% names(label_mapping)) {
#'       mapped_label <- label_mapping[[display_name]]
#'       cli::cli_alert_info("Applied label mapping: {var_name} -> {mapped_label}")
#'       return(mapped_label)
#'     }
#'
#'     # Else, check if it's a t0_ variable corresponding to a t2_ in label_mapping
#'     if (startsWith(display_name, "t0_")) {
#'       t2_var <- sub("^t0_", "t2_", display_name)
#'       if (!is.null(label_mapping) && t2_var %in% names(label_mapping)) {
#'         mapped_label <- label_mapping[[t2_var]]
#'         cli::cli_alert_info("Applied label mapping via t2_ equivalent: {var_name} -> {mapped_label}")
#'         return(mapped_label)
#'       }
#'     }
#'
#'     # Else, apply transformations
#'     if (remove_tx_prefix) display_name <- sub("^t[0-9]+_", "", display_name)
#'     if (remove_z_suffix) display_name <- sub("_z$", "", display_name)
#'     display_name <- gsub("_", " ", display_name)
#'
#'     if (use_title_case) {
#'       display_name <- tools::toTitleCase(display_name)
#'       # Replace "Nz" with "NZ"
#'       display_name <- gsub("Nz", "NZ", display_name)
#'     }
#'
#'     # Notify if transformed
#'     if (display_name != var_name) {
#'       cli::cli_alert_info("Transformed label: {var_name} -> {display_name}")
#'     }
#'
#'     return(display_name)
#'   }
#'
#'   # Function to get original scale value
#'   get_original_value <- function(var_name, split_value) {
#'     if (is.null(original_df)) return(NULL)
#'
#'     orig_var <- var_name
#'     # If variable was z-transformed, assume original_df has the original variable without '_z'
#'     if (grepl("_z$", orig_var)) {
#'       orig_var <- sub("_z$", "", orig_var)
#'     }
#'
#'     if (!(orig_var %in% names(original_df))) {
#'       cli::cli_warn("Original variable '{orig_var}' not found in original_df. Skipping original scale value.")
#'       return(NULL)
#'     }
#'
#'     orig_data <- original_df[[orig_var]]
#'
#'     # Calculate mean and sd from original_df
#'     orig_mean <- mean(orig_data, na.rm = TRUE)
#'     orig_sd <- sd(orig_data, na.rm = TRUE)
#'
#'     # Back-transform z-score to original scale
#'     original_value <- orig_mean + split_value * orig_sd
#'
#'     return(round(original_value, 4))
#'   }
#'
#'   # Generate general interpretation
#'   general_interpretation <- glue::glue(
#'     "**Policy Tree Interpretation:** A policy tree obtains simple rule-based policies, ",
#'     "where the rule takes the form of a shallow decision tree. The policytree algorithm ",
#'     "uses doubly robust reward estimates from grf to find a shallow, but globally optimal ",
#'     "decision tree. We train the model on {train_proportion * 100}% of the data and then ",
#'     "evaluate the model on the remainder of the data. The graph helps to clarify whether ",
#'     "the leaf node in the test set samples are predicted to have mean outcomes in line ",
#'     "with the prescribed policy.\n\n"
#'   )
#'
#'   cli::cli_alert_success("Generated general interpretation")
#'
#'   # Extract nodes for easier access
#'   nodes <- policy_tree_obj$nodes
#'
#'   # Define helper function to format split value with original scale
#'   format_split_value <- function(var_name, split_val) {
#'     orig_val <- get_original_value(var_name, split_val)
#'     if (!is.null(orig_val)) {
#'       return(glue::glue("at a standardized value of {round(split_val, 4)} (original scale: {orig_val})"))
#'     } else {
#'       return(glue::glue("at a standardized value of {round(split_val, 4)}"))
#'     }
#'   }
#'
#'   # Construct specific_interpretation
#'   # Assuming depth 2 tree: root, two children, four leaves.
#'
#'   # Node 1: root
#'   node1 <- nodes[[1]]
#'   var1 <- split_var_names[1]
#'   split_val1 <- node1$split_value
#'   formatted_split_val1 <- format_split_value(var1, split_val1)
#'
#'   # Node 2: left child of node1
#'   node2 <- nodes[[2]]
#'   var2 <- split_var_names[2]
#'   split_val2 <- node2$split_value
#'   formatted_split_val2 <- format_split_value(var2, split_val2)
#'
#'   # Node 3: right child of node1
#'   node3 <- nodes[[3]]
#'   var3 <- split_var_names[3]
#'   split_val3 <- node3$split_value
#'   formatted_split_val3 <- format_split_value(var3, split_val3)
#'
#'   # Nodes 4-7: leaves
#'   node4 <- nodes[[4]] # left child of node2
#'   node5 <- nodes[[5]] # right child of node2
#'   node6 <- nodes[[6]] # left child of node3
#'   node7 <- nodes[[7]] # right child of node3
#'
#'   # Check if split_var_names has at least 3 variables
#'   if (length(split_var_names) < 3) {
#'     cli::cli_abort("Insufficient split variables extracted. Expected at least 3.")
#'   }
#'
#'   specific_interpretation <- glue::glue(
#'     "**Findings for {transform_var_name(model_name)}:** \n\n",
#'     "1. The first split is based on the variable '{transform_var_name(var1)}' {formatted_split_val1}.\n\n",
#'     "   - If this value is less than or equal to the split value:\n",
#'     "     2. The second split is based on '{transform_var_name(var2)}' {formatted_split_val2}.\n",
#'     "        - If this is less than or equal to the split value, the recommended action is: **{action_names[node4$action]}**\n",
#'     "        - Otherwise, the recommended action is: **{action_names[node5$action]}**\n\n",
#'     "   - If the first split value is greater than the split value:\n",
#'     "     3. The second split is based on '{transform_var_name(var3)}' {formatted_split_val3}.\n",
#'     "        - If this is less than or equal to the split value, the recommended action is: **{action_names[node6$action]}**\n",
#'     "        - Otherwise, the recommended action is: **{action_names[node7$action]}**\n\n",
#'     "This policy tree suggests that an optimal treatment strategy is related to these inflection points in the variables the policy tree identifies, ",
#'     "with recommended actions based on subgroups defined by these split points."
#'   )
#'
#'   cli::cli_alert_success("Generated specific interpretation")
#'
#'   # Combine interpretations
#'   full_interpretation <- paste(general_interpretation, specific_interpretation, sep = "\n")
#'
#'   # Print the full interpretation to the console
#'   cat(full_interpretation)
#'
#'   cli::cli_alert_success("Policy tree interpretation completed successfully! {cli::col_green(cli::symbol$tick)}")
#'
#'   # Return the full interpretation invisibly
#'   invisible(full_interpretation)
#' }
#'
#'
#' # Function to transform labels
#' #' @keywords internal
#' transform_var_name <- function(var_name) {
#'   display_name <- var_name
#'
#'   # Remove 'model_' prefix if present (for model names)
#'   if (startsWith(display_name, "model_")) {
#'     display_name <- sub("^model_", "", display_name)
#'   }
#'
#'   # Apply label mapping first, if exists
#'   if (!is.null(label_mapping) && display_name %in% names(label_mapping)) {
#'     mapped_label <- label_mapping[[display_name]]
#'     cli::cli_alert_info("Applied label mapping: {var_name} -> {mapped_label}")
#'     return(mapped_label)
#'   }
#'
#'   # Else, check if it's a t0_ variable corresponding to a t2_ in label_mapping
#'   if (startsWith(display_name, "t0_")) {
#'     t2_var <- sub("^t0_", "t2_", display_name)
#'     if (!is.null(label_mapping) && t2_var %in% names(label_mapping)) {
#'       mapped_label <- label_mapping[[t2_var]]
#'       cli::cli_alert_info("Applied label mapping via t2_ equivalent: {var_name} -> {mapped_label}")
#'       return(mapped_label)
#'     }
#'   }
#'
#'   # Else, apply transformations
#'   if (remove_tx_prefix) display_name <- sub("^t[0-9]+_", "", display_name)
#'   if (remove_z_suffix) display_name <- sub("_z$", "", display_name)
#'   display_name <- gsub("_", " ", display_name)
#'
#'   if (use_title_case) {
#'     display_name <- tools::toTitleCase(display_name)
#'     # Replace "Nz" with "NZ"
#'     display_name <- gsub("Nz", "NZ", display_name)
#'   }
#'
#'   # Notify if transformed
#'   if (display_name != var_name) {
#'     cli::cli_alert_info("Transformed label: {var_name} -> {display_name}")
#'   }
#'
#'   return(display_name)
#' }
