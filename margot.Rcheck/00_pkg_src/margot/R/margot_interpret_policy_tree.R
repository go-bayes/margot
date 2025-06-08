#' Interpret Policy Tree Results
#'
#' This function creates an interpretation of policy tree results from a causal forest or multi-arm causal forest model.
#' It generates a formatted description of the policy tree, including the main splits and recommended actions.
#'
#' @param model A list containing the results from a multi-arm causal forest model.
#' @param model_name A string specifying which model's results to interpret.
#' @param max_depth Integer, 1 or 2; which stored tree to interpret.
#' @param train_proportion Numeric value between 0 and 1 for the proportion of data used for training. Default is 0.5.
#' @param custom_action_names Optional vector of custom names for the actions. Must match the number of actions in the policy tree.
#' @param label_mapping Optional list that maps variable names to custom labels.
#' @param original_df Optional dataframe with untransformed variables, used to display split values on the data scale.
#' @param remove_tx_prefix Logical indicating whether to remove prefixes like t0_ from variable names. Default is TRUE.
#' @param remove_z_suffix Logical indicating whether to remove the _z suffix from variable names. Default is TRUE.
#' @param use_title_case Logical indicating whether to convert variable names to title case. Default is TRUE.
#'
#' @return Invisibly returns a string containing the interpretation; also prints it to the console.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @importFrom glue glue
#' @export
margot_interpret_policy_tree <- function(model,
                                         model_name,
                                         max_depth        = 2L,
                                         train_proportion = 0.5,
                                         custom_action_names = NULL,
                                         label_mapping    = NULL,
                                         original_df      = NULL,
                                         remove_tx_prefix = TRUE,
                                         remove_z_suffix  = TRUE,
                                         use_title_case   = TRUE) {
  cli::cli_alert_info("Starting policy tree interpretation for {model_name} (depth {max_depth})")

  # helper to relabel variables
  transform_var <- function(var) {
    transform_var_name(var, label_mapping,
                       remove_tx_prefix, remove_z_suffix, use_title_case)
  }

  tag <- paste0("policy_tree_depth_", max_depth)
  policy_tree_obj <- model$results[[model_name]][[tag]]
  if (is.null(policy_tree_obj)) {
    cli::cli_abort("No {tag} found for {model_name}")
  }

  # pull action names
  action_names <- policy_tree_obj$action.names
  if (!is.null(custom_action_names)) {
    if (length(custom_action_names) != length(action_names)) {
      cli::cli_abort("Length of custom_action_names must match number of actions")
    }
    action_names <- custom_action_names
    cli::cli_alert_success("Applied custom action names")
  }
  act_labels <- vapply(action_names, transform_var, "")

  # get nodes
  nodes <- policy_tree_obj$nodes
  cols  <- policy_tree_obj$columns

  # helper to format split
  format_split <- function(var, val) {
    orig <- get_original_value(var, val, original_df)
    if (!is.null(orig)) {
      glue::glue("{round(val,3)} (original: {orig})")
    } else {
      round(val,3)
    }
  }

  # general intro
  intro <- glue::glue(
    "A shallow policy tree recommends actions based on two splits for depth=2, or one split for depth=1.\n",
    "We trained on {train_proportion*100}% of the data and evaluated on the rest.\n\n"
  )
  cli::cli_alert_success("Generated general interpretation")

  if (max_depth == 1L) {
    # single split → two leaves
    n1 <- nodes[[1]]
    var1 <- cols[n1$split_variable]
    sp1  <- format_split(var1, n1$split_value)
    leaf_left  <- nodes[[2]]$action
    leaf_right <- nodes[[3]]$action

    text <- glue::glue(
      "**Findings for {transform_var(model_name)}:**\n\n",
      "Participants are split on {transform_var(var1)} at {sp1}. ",
      "Those with {transform_var(var1)} ≤ threshold are recommended **{act_labels[leaf_left]}**, ",
      "and those with {transform_var(var1)} > threshold are recommended **{act_labels[leaf_right]}**.\n"
    )
  } else {
    # depth=2 → four leaves
    n1 <- nodes[[1]]; var1 <- cols[n1$split_variable]; sp1  <- format_split(var1, n1$split_value)
    n2 <- nodes[[2]]; var2 <- cols[n2$split_variable]; sp2  <- format_split(var2, n2$split_value)
    n3 <- nodes[[3]]; var3 <- cols[n3$split_variable]; sp3  <- format_split(var3, n3$split_value)
    leaf_22 <- nodes[[4]]$action
    leaf_23 <- nodes[[5]]$action
    leaf_32 <- nodes[[6]]$action
    leaf_33 <- nodes[[7]]$action

    text <- glue::glue(
      "**Findings for {transform_var(model_name)}:**\n\n",
      "Split 1: {transform_var(var1)} ≤ {sp1}.  ",
      "Within that subgroup, split 2a: {transform_var(var2)} ≤ {sp2}, ",
      "→ **{act_labels[leaf_22]}**; ",
      "{transform_var(var2)} > {sp2} → **{act_labels[leaf_23]}**.\n\n",
      "Split 2: {transform_var(var1)} > {sp1}.  ",
      "Within that subgroup, split 2b: {transform_var(var3)} ≤ {sp3}, ",
      "→ **{act_labels[leaf_32]}**; ",
      "{transform_var(var3)} > {sp3} → **{act_labels[leaf_33]}**.\n"
    )
  }

  full <- paste0(intro, text, "\n")
  cat(full)
  cli::cli_alert_success("Policy tree interpretation completed!")
  invisible(full)
}
