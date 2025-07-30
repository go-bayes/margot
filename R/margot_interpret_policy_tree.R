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
#' @param include_conditional_means Logical indicating whether to include conditional means information if available. Default is TRUE.
#' @param use_math_notation Logical indicating whether to use mathematical notation (E[Y(a)|X]) or plain language. Default is FALSE for clarity.
#' @param output_format Character string specifying output format: "prose" (default) for flowing narrative text, or "bullet" for structured bullet points.
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
                                         use_title_case   = TRUE,
                                         include_conditional_means = TRUE,
                                         use_math_notation = FALSE,
                                         output_format = c("prose", "bullet")) {
  cli::cli_alert_info("Starting policy tree interpretation for {model_name} (depth {max_depth})")
  
  output_format <- match.arg(output_format)

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
  if (output_format == "prose") {
    intro <- ""  # prose format will include intro in the main text
  } else {
    intro <- glue::glue(
      "The policy tree identifies subgroups with different treatment effects by splitting the data based on key variables. ",
      "This analysis used depth-{max_depth} trees with {train_proportion*100}% of the sample for training ",
      "and {(1-train_proportion)*100}% for evaluation.\n\n"
    )
  }
  cli::cli_alert_success("Generated general interpretation")

  if (max_depth == 1L) {
    # single split → two leaves
    n1 <- nodes[[1]]
    var1 <- cols[n1$split_variable]
    sp1  <- format_split(var1, n1$split_value)
    leaf_left  <- nodes[[2]]$action
    leaf_right <- nodes[[3]]$action

    if (output_format == "prose") {
      text <- glue::glue(
        "#### Findings for {transform_var(model_name)} at the end of study\n\n",
        "The policy-tree analysis divided the sample on baseline {transform_var(var1)}. ",
        "Respondents who scored ≤ {sp1} formed one branch and were assigned to the {act_labels[leaf_left]} policy. ",
        "Those with baseline {transform_var(var1)} > {sp1} formed the second branch and were assigned to {act_labels[leaf_right]}.\n\n"
      )
    } else {
      text <- glue::glue(
        "**Findings for {transform_var(model_name)} at the end of study:**\n\n",
        "Participants are split on baseline {transform_var(var1)} at {sp1}. ",
        "Those with baseline {transform_var(var1)} ≤ threshold are recommended **{act_labels[leaf_left]}**, ",
        "and those with baseline {transform_var(var1)} > threshold are recommended **{act_labels[leaf_right]}**.\n"
      )
    }
  } else {
    # depth=2 → four leaves
    n1 <- nodes[[1]]; var1 <- cols[n1$split_variable]; sp1  <- format_split(var1, n1$split_value)
    n2 <- nodes[[2]]; var2 <- cols[n2$split_variable]; sp2  <- format_split(var2, n2$split_value)
    n3 <- nodes[[3]]; var3 <- cols[n3$split_variable]; sp3  <- format_split(var3, n3$split_value)
    leaf_22 <- nodes[[4]]$action
    leaf_23 <- nodes[[5]]$action
    leaf_32 <- nodes[[6]]$action
    leaf_33 <- nodes[[7]]$action

    if (output_format == "prose") {
      text <- glue::glue(
        "#### Findings for {transform_var(model_name)} at the end of study\n\n",
        "The policy-tree analysis first divided the sample on baseline {transform_var(var1)}. ",
        "Respondents who scored ≤ {sp1} formed one branch. ",
        "Within this subgroup, a second split occurred on baseline {transform_var(var2)}: ",
        "individuals with baseline {transform_var(var2)} ≤ {sp2} were assigned to the {act_labels[leaf_22]} policy, ",
        "whereas those with baseline {transform_var(var2)} > {sp2} were assigned to {act_labels[leaf_23]}.\n\n",
        "Participants with baseline {transform_var(var1)} scores > {sp1} formed the second major branch. ",
        "In that branch, the tree split on baseline {transform_var(var3)}: ",
        "respondents whose baseline {transform_var(var3)} was ≤ {sp3} were routed to the {act_labels[leaf_32]} policy, ",
        "and those scoring above that threshold were routed to {act_labels[leaf_33]}.\n\n"
      )
    } else {
      text <- glue::glue(
        "**Findings for {transform_var(model_name)} at the end of study:**\n\n",
        "Split 1: baseline {transform_var(var1)} ≤ {sp1}.  ",
        "Within that subgroup, split 2a: baseline {transform_var(var2)} ≤ {sp2}, ",
        "→ **{act_labels[leaf_22]}**; ",
        "baseline {transform_var(var2)} > {sp2} → **{act_labels[leaf_23]}**.\n\n",
        "Split 2: baseline {transform_var(var1)} > {sp1}.  ",
        "Within that subgroup, split 2b: baseline {transform_var(var3)} ≤ {sp3}, ",
        "→ **{act_labels[leaf_32]}**; ",
        "baseline {transform_var(var3)} > {sp3} → **{act_labels[leaf_33]}**.\n"
      )
    }
  }

  # add conditional means interpretation if available
  cond_means_text <- ""
  if (include_conditional_means && !is.null(model$results[[model_name]]$conditional_means)) {
    tryCatch({
      cond_means_text <- compute_conditional_means_interpretation(
        model = model,
        model_name = model_name,
        policy_tree_obj = policy_tree_obj,
        max_depth = max_depth,
        act_labels = act_labels,
        transform_var = transform_var,
        use_math_notation = use_math_notation,
        output_format = output_format,
        original_df = original_df
      )
      if (nzchar(cond_means_text)) {
        cli::cli_alert_success("Added conditional means interpretation")
      }
    }, error = function(e) {
      cli::cli_alert_warning("Could not compute conditional means interpretation: {e$message}")
      # print more detailed error information
      if (exists("e$call")) {
        cli::cli_alert_info("Error occurred at: {deparse(e$call)}")
      }
      cond_means_text <- "\n\n(Conditional means analysis failed due to an error)\n\n"
    })
  }
  
  full <- paste0(intro, text, cond_means_text, "\n")
  cat(full)
  cli::cli_alert_success("Policy tree interpretation completed!")
  invisible(full)
}

#' Compute conditional means interpretation for policy tree leaves
#' @keywords internal
compute_conditional_means_interpretation <- function(model, model_name, policy_tree_obj, 
                                                   max_depth, act_labels, transform_var,
                                                   use_math_notation = FALSE,
                                                   output_format = "bullet",
                                                   original_df = NULL) {
  
  # get conditional means and other needed data
  conditional_means <- model$results[[model_name]]$conditional_means
  if (is.null(conditional_means) || nrow(conditional_means) == 0) {
    return("\n\n(Conditional means not available for this model)\n\n")
  }
  
  # get predictions from plot_data to identify which units fall in which leaf
  plot_data <- model$results[[model_name]]$plot_data
  if (is.null(plot_data) || is.null(plot_data$predictions)) {
    return("\n\n(Policy tree test data not available for conditional means analysis)\n\n")
  }
  
  # for binary treatment, conditional_means has 2 columns (control, treatment)
  # the policy tree predictions tell us which action is recommended for each unit
  
  # get the test indices used for evaluation
  test_predictions <- plot_data$predictions
  test_X <- plot_data$X_test_full
  
  if (is.null(test_X)) {
    test_X <- plot_data$X_test  # fallback to restricted covariates
  }
  
  # get covariates for conditional means computation
  covariates <- model$covariates
  if (is.null(covariates)) {
    return("")
  }
  
  # match test indices to full data indices
  # note: plot_data contains test set only, need to map back
  not_missing <- model$not_missing
  if (is.null(not_missing)) {
    not_missing <- which(complete.cases(covariates))
  }
  
  # the test indices correspond to a subset of the data
  # we need to get the matching conditional means rows
  # test_X should have rownames that are the original row indices
  test_row_indices <- rownames(test_X)
  
  # DEBUG: print sizes
  # cli::cli_alert_info("DEBUG: nrow(test_X) = {nrow(test_X)}, nrow(conditional_means) = {nrow(conditional_means)}")
  # if (!is.null(test_row_indices)) {
  #   cli::cli_alert_info("DEBUG: test_row_indices[1:5] = {paste(head(test_row_indices, 5), collapse=', ')}")
  # }
  
  if (!is.null(test_row_indices)) {
    # convert to numeric indices
    test_row_indices <- as.numeric(test_row_indices)
    
    # check if these are valid indices for conditional_means
    max_idx <- max(test_row_indices)
    if (max_idx > nrow(conditional_means)) {
      # indices don't match - fall back to using just the test set size
      n_test <- nrow(test_X)
      if (n_test > nrow(conditional_means)) {
        return("\n\n(Conditional means analysis not available due to data size mismatch)\n\n")
      }
      # use sequential matching - this assumes test set is a contiguous subset
      test_conditional_means <- conditional_means[seq_len(n_test), , drop = FALSE]
    } else {
      # use the actual indices
      test_conditional_means <- conditional_means[test_row_indices, , drop = FALSE]
    }
  } else {
    # no rownames - use sequential matching
    n_test <- nrow(test_X)
    if (n_test > nrow(conditional_means)) {
      return("\n\n(Conditional means analysis not available due to data size mismatch)\n\n")
    }
    test_conditional_means <- conditional_means[seq_len(n_test), , drop = FALSE]
  }
  
  if (output_format == "prose") {
    text <- "\n#### Treatment-effect heterogeneity\n\n"
    
    if (max_depth == 2L) {
      text <- paste0(text, 
        "A depth-two policy tree therefore produced four terminal leaves. ",
        "Conditional average treatment effects (CATEs) were estimated within each leaf:\n\n"
      )
    } else {
      text <- paste0(text,
        "The policy tree produced two terminal leaves. ",
        "Conditional average treatment effects (CATEs) were estimated within each leaf:\n\n"
      )
    }
  } else {
    text <- "\n\n**Treatment Effect Heterogeneity**\n\n"
    
    # Add description based on notation preference
    if (use_math_notation) {
      text <- paste0(text, 
        "The following table shows conditional mean outcomes E[Y(a)|X∈leaf] for each treatment arm a∈{0,1}, ",
        "where Y(a) denotes the potential outcome under treatment a. ",
        "The conditional average treatment effect (CATE) is τ(x) = E[Y(1)|X∈leaf] - E[Y(0)|X∈leaf].\n\n"
      )
    } else {
      text <- paste0(text, 
        "The following analysis shows how treatment effects vary across different subgroups identified by the policy tree.\n\n"
      )
    }
  }
  
  if (max_depth == 1L) {
    # two leaves for depth-1 tree
    nodes <- policy_tree_obj$nodes
    
    # leaf assignments based on first split
    var1 <- policy_tree_obj$columns[nodes[[1]]$split_variable]
    split1 <- nodes[[1]]$split_value
    
    # identify units in each leaf using the test data
    if (!is.null(test_X) && var1 %in% colnames(test_X)) {
      left_leaf_idx <- which(test_X[, var1] <= split1)
      right_leaf_idx <- which(test_X[, var1] > split1)
      
      # compute average conditional means for each leaf
      result_text <- compute_leaf_means(
        leaf_idx = list(left = left_leaf_idx, right = right_leaf_idx),
        predictions = test_predictions,
        conditional_means = test_conditional_means,
        act_labels = act_labels,
        leaf_names = c(
          paste0("baseline ", transform_var(var1), " ≤ ", round(split1, 3)),
          paste0("baseline ", transform_var(var1), " > ", round(split1, 3))
        ),
        use_math_notation = use_math_notation,
        output_format = output_format,
        original_df = original_df,
        model_name = model_name
      )
      text <- paste0(text, result_text)
      
      # add overall statistics for depth-1
      tryCatch({
        n_total <- length(test_predictions)
        if (n_total > 0) {
          n_control <- sum(test_predictions == 1)
          n_treatment <- sum(test_predictions == 2)
          pct_control <- round(100 * n_control / n_total, 1)
          pct_treatment <- round(100 * n_treatment / n_total, 1)
          
          if (output_format == "prose") {
            text <- paste0(text,
              "\n#### Overall policy performance\n\n",
              "Across the full test set (N = ", formatC(n_total, format="d", big.mark=","), "), ",
              "the policy prescribes ", tolower(act_labels[1]), " for ",
              formatC(n_control, format="d", big.mark=","), " participants (", pct_control, "%) ",
              "and ", tolower(act_labels[2]), " for ",
              formatC(n_treatment, format="d", big.mark=","), " participants (", pct_treatment, "%).\n"
            )
          } else {
            text <- paste0(text,
              "\n**Summary Statistics**\n\n",
              "Total units in test set: n = ", n_total, "\n",
              "Units assigned to ", act_labels[1], ": ", n_control, " (", pct_control, "%)\n",
              "Units assigned to ", act_labels[2], ": ", n_treatment, " (", pct_treatment, "%)\n\n"
            )
          }
        }
      }, error = function(e) {
        # fail silently
      })
    }
    
  } else {
    # four leaves for depth-2 tree
    text <- paste0(text, "Policy tree has four leaves. Conditional mean analysis for depth-2 trees ",
                   "shows treatment effects within each subgroup defined by the two-level splits.\n\n")
    
    # extract nodes for depth-2 tree
    nodes <- policy_tree_obj$nodes
    
    # first split
    var1 <- policy_tree_obj$columns[nodes[[1]]$split_variable]
    split1 <- nodes[[1]]$split_value
    
    # second level splits
    var2 <- policy_tree_obj$columns[nodes[[2]]$split_variable]
    split2 <- nodes[[2]]$split_value
    var3 <- policy_tree_obj$columns[nodes[[3]]$split_variable]
    split3 <- nodes[[3]]$split_value
    
    # identify units in each leaf using the test data
    if (!is.null(test_X) && all(c(var1, var2, var3) %in% colnames(test_X))) {
      # define the four leaves based on the tree structure
      # leaf 1: var1 <= split1 & var2 <= split2
      # leaf 2: var1 <= split1 & var2 > split2
      # leaf 3: var1 > split1 & var3 <= split3
      # leaf 4: var1 > split1 & var3 > split3
      
      left_idx <- which(test_X[, var1] <= split1)
      right_idx <- which(test_X[, var1] > split1)
      
      # ensure we have valid indices before subsetting
      if (length(left_idx) > 0) {
        left_var2_vals <- test_X[left_idx, var2]
        leaf1_idx <- left_idx[left_var2_vals <= split2]
        leaf2_idx <- left_idx[left_var2_vals > split2]
      } else {
        leaf1_idx <- integer(0)
        leaf2_idx <- integer(0)
      }
      
      if (length(right_idx) > 0) {
        right_var3_vals <- test_X[right_idx, var3]
        leaf3_idx <- right_idx[right_var3_vals <= split3]
        leaf4_idx <- right_idx[right_var3_vals > split3]
      } else {
        leaf3_idx <- integer(0)
        leaf4_idx <- integer(0)
      }
      
      # compute average conditional means for each leaf
      leaf_indices <- list(leaf1_idx, leaf2_idx, leaf3_idx, leaf4_idx)
      leaf_names <- c(
        paste0("baseline ", transform_var(var1), " ≤ ", round(split1, 3), " & baseline ", 
               transform_var(var2), " ≤ ", round(split2, 3)),
        paste0("baseline ", transform_var(var1), " ≤ ", round(split1, 3), " & baseline ", 
               transform_var(var2), " > ", round(split2, 3)),
        paste0("baseline ", transform_var(var1), " > ", round(split1, 3), " & baseline ", 
               transform_var(var3), " ≤ ", round(split3, 3)),
        paste0("baseline ", transform_var(var1), " > ", round(split1, 3), " & baseline ", 
               transform_var(var3), " > ", round(split3, 3))
      )
      
      result_text <- compute_leaf_means(
        leaf_idx = leaf_indices,
        predictions = test_predictions,
        conditional_means = test_conditional_means,
        act_labels = act_labels,
        leaf_names = leaf_names,
        use_math_notation = use_math_notation,
        output_format = output_format,
        original_df = original_df,
        model_name = model_name
      )
      text <- paste0(text, result_text)
      
      # add overall statistics
      tryCatch({
        n_control <- sum(test_predictions == 1)  # assuming 1 = control, 2 = treatment
        n_treatment <- sum(test_predictions == 2)
        n_total <- length(test_predictions)
        
        if (n_total > 0) {
          pct_control <- round(100 * n_control / n_total, 1)
          pct_treatment <- round(100 * n_treatment / n_total, 1)
          
          
          # Calculate summary statistics
          effects <- numeric()
          sizes <- numeric()
          for (j in seq_along(leaf_indices)) {
            if (length(leaf_indices[[j]]) > 0) {
              leaf_cm <- test_conditional_means[leaf_indices[[j]], , drop = FALSE]
              if (nrow(leaf_cm) > 0 && ncol(leaf_cm) == 2) {
                avg_cm <- colMeans(leaf_cm, na.rm = TRUE)
                effects <- c(effects, avg_cm[2] - avg_cm[1])
                sizes <- c(sizes, length(leaf_indices[[j]]))
              }
            }
          }
          
          weighted_avg_effect <- if (length(effects) > 0 && sum(sizes) > 0) {
            sum(effects * sizes) / sum(sizes)
          } else {
            NA
          }
          
          if (output_format == "prose") {
            text <- paste0(text,
              "\n#### Overall policy performance\n\n",
              "Across the full test set (N = ", formatC(n_total, format="d", big.mark=","), "), ",
              "the policy prescribes ", tolower(act_labels[1]), " for ",
              formatC(n_control, format="d", big.mark=","), " participants (", pct_control, "%) ",
              "and ", tolower(act_labels[2]), " for ",
              formatC(n_treatment, format="d", big.mark=","), " participants (", pct_treatment, "%). "
            )
            
            if (!is.na(weighted_avg_effect)) {
              # Transform to original scale if possible
              weighted_avg_display <- weighted_avg_effect
              original_scale_overall <- ""
              
              # Get transformation info if available
              transform_info <- NULL
              if (!is.null(original_df) && !is.null(model_name)) {
                transform_info <- get_outcome_transformation_info(model_name, original_df)
              }
              
              if (!is.null(transform_info)) {
                if (transform_info$has_z && !transform_info$has_log) {
                  # simple z-transformation: multiply by SD
                  weighted_avg_display <- weighted_avg_effect * transform_info$orig_sd
                  original_scale_overall <- paste0(" (original scale: ", format_minimal_decimals(weighted_avg_display), " units)")
                } else if (transform_info$has_z && transform_info$has_log) {
                  # log+z transformation: use multiplicative interpretation
                  units_info <- detect_variable_units(transform_info$original_var)
                  
                  # calculate effect on log scale
                  delta_log_overall <- weighted_avg_effect * transform_info$log_sd
                  ratio_overall <- exp(delta_log_overall)
                  pct_change_overall <- (ratio_overall - 1) * 100
                  change_word <- if (pct_change_overall >= 0) "increase" else "decrease"
                  
                  # calculate absolute change based on population mean
                  # use display mean if available (for corrected population statistics)
                  log_mean_overall <- if (!is.null(transform_info$use_display_mean) && transform_info$use_display_mean) {
                    transform_info$log_mean_display
                  } else {
                    transform_info$log_mean
                  }
                  pop_mean_overall <- exp(log_mean_overall) - transform_info$log_offset
                  if (!is.null(units_info$scale_factor)) {
                    pop_mean_overall <- pop_mean_overall * units_info$scale_factor
                  }
                  abs_change_overall <- pop_mean_overall * (ratio_overall - 1)
                  
                  # format based on unit type
                  if (units_info$type == "monetary") {
                    original_scale_overall <- sprintf(" (%.0f%% average multiplicative %s, ~%s%s average %s)", 
                                                    abs(pct_change_overall), change_word,
                                                    units_info$symbol, format_minimal_decimals(abs(abs_change_overall)),
                                                    change_word)
                  } else {
                    original_scale_overall <- sprintf(" (%.0f%% average %s)", 
                                                    abs(pct_change_overall), change_word)
                  }
                }
              }
              
              # Adjust wording based on transformation type
              if (!is.null(transform_info) && transform_info$has_z && transform_info$has_log) {
                # For log outcomes, emphasize percentage change
                text <- paste0(text,
                  "Weighting the leaf-level CATEs by subgroup size yields an average treatment effect of ",
                  format_minimal_decimals(weighted_avg_effect), original_scale_overall, ". ",
                  "In other words, if the recommended treatment decisions were implemented for every unit ",
                  "in the test set, the mean outcome would be expected to ",
                  ifelse(pct_change_overall > 0, "increase", "decrease"), " by approximately ",
                  sprintf("%.0f%%", abs(pct_change_overall)),
                  " relative to universal ", 
                  tolower(act_labels[1]), ".\n"
                )
              } else {
                # For non-log outcomes, use original wording
                text <- paste0(text,
                  "Weighting the leaf-level CATEs by subgroup size yields an average treatment effect of ",
                  format_minimal_decimals(weighted_avg_effect), original_scale_overall, ". ",
                  "In other words, if the recommended treatment decisions were implemented for every unit ",
                  "in the test set, the mean outcome would be expected to ",
                  ifelse(weighted_avg_effect > 0, "rise", "fall"), " by roughly ",
                  format_minimal_decimals(abs(weighted_avg_effect)), " units",
                  ifelse(nchar(original_scale_overall) > 0, " on the standardized scale", ""),
                  " relative to universal ", 
                  tolower(act_labels[1]), ".\n"
                )
              }
            }
          } else {
            text <- paste0(text, 
              "\n**Summary Statistics**\n\n",
              "Total units in test set: n = ", n_total, "\n",
              "Units assigned to ", act_labels[1], ": ", n_control, " (", pct_control, "%)\n",
              "Units assigned to ", act_labels[2], ": ", n_treatment, " (", pct_treatment, "%)\n"
            )
            
            if (!is.na(weighted_avg_effect)) {
              # Transform to original scale if possible
              original_scale_overall <- ""
              
              # Get transformation info if available
              transform_info <- NULL
              if (!is.null(original_df) && !is.null(model_name)) {
                transform_info <- get_outcome_transformation_info(model_name, original_df)
              }
              
              if (!is.null(transform_info)) {
                if (transform_info$has_z && !transform_info$has_log) {
                  # simple z-transformation: multiply by SD
                  weighted_avg_display <- weighted_avg_effect * transform_info$orig_sd
                  original_scale_overall <- paste0(" (original scale: ", format_minimal_decimals(weighted_avg_display), " units)")
                } else if (transform_info$has_z && transform_info$has_log) {
                  # log+z transformation: use multiplicative interpretation
                  units_info <- detect_variable_units(transform_info$original_var)
                  
                  # calculate effect on log scale
                  delta_log_overall <- weighted_avg_effect * transform_info$log_sd
                  ratio_overall <- exp(delta_log_overall)
                  pct_change_overall <- (ratio_overall - 1) * 100
                  change_word <- if (pct_change_overall >= 0) "increase" else "decrease"
                  
                  # calculate absolute change based on population mean
                  # use display mean if available (for corrected population statistics)
                  log_mean_overall <- if (!is.null(transform_info$use_display_mean) && transform_info$use_display_mean) {
                    transform_info$log_mean_display
                  } else {
                    transform_info$log_mean
                  }
                  pop_mean_overall <- exp(log_mean_overall) - transform_info$log_offset
                  if (!is.null(units_info$scale_factor)) {
                    pop_mean_overall <- pop_mean_overall * units_info$scale_factor
                  }
                  abs_change_overall <- pop_mean_overall * (ratio_overall - 1)
                  
                  # format based on unit type
                  if (units_info$type == "monetary") {
                    original_scale_overall <- sprintf(" (%.0f%% average multiplicative %s, ~%s%s average %s)", 
                                                    abs(pct_change_overall), change_word,
                                                    units_info$symbol, format_minimal_decimals(abs(abs_change_overall)),
                                                    change_word)
                  } else {
                    original_scale_overall <- sprintf(" (%.0f%% average %s)", 
                                                    abs(pct_change_overall), change_word)
                  }
                }
              }
              
              text <- paste0(text,
                "Weighted average treatment effect: ", format_minimal_decimals(weighted_avg_effect), original_scale_overall, "\n"
              )
            }
            
            text <- paste0(text, "\n")
          }
        }
      }, error = function(e) {
        # fail silently
      })
    } else {
      # fallback to simple summary if we can't compute leaf means
      tryCatch({
        n_control <- sum(test_predictions == 1)
        n_treatment <- sum(test_predictions == 2)
        
        if (n_control > 0 && n_treatment > 0) {
          text <- paste0(text, 
            "Among the test set:\n",
            "- ", n_control, " units were assigned to ", act_labels[1], "\n",
            "- ", n_treatment, " units were assigned to ", act_labels[2], "\n\n",
            "(Detailed leaf analysis not available due to missing covariate data)\n\n"
          )
        }
      }, error = function(e) {
        # fail silently
      })
    }
  }
  
  return(text)
}

#' Compute average conditional means within leaves
#' @keywords internal
compute_leaf_means <- function(leaf_idx, predictions, conditional_means, act_labels, leaf_names, 
                             use_math_notation = FALSE, output_format = "bullet",
                             original_df = NULL, model_name = NULL, display_original_scale = TRUE) {
  text <- ""
  
  # get transformation info if available
  transform_info <- NULL
  if (display_original_scale && !is.null(original_df) && !is.null(model_name)) {
    transform_info <- get_outcome_transformation_info(model_name, original_df)
  }
  
  # handle both list and non-list inputs
  if (!is.list(leaf_idx)) {
    leaf_idx <- list(leaf_idx)
  }
  
  for (i in seq_along(leaf_idx)) {
    idx <- leaf_idx[[i]]
    if (length(idx) == 0) next
    
    # get the conditional means for units in this leaf
    leaf_cm <- conditional_means[idx, , drop = FALSE]
    if (nrow(leaf_cm) == 0) next
    
    # compute average conditional means
    avg_cm <- colMeans(leaf_cm, na.rm = TRUE)
    
    # get predominant action in this leaf
    leaf_preds <- predictions[idx]
    if (length(leaf_preds) == 0) next
    
    pred_table <- table(leaf_preds)
    if (length(pred_table) == 0) next
    
    # ensure we have valid action indices
    pred_names <- names(sort(pred_table, decreasing = TRUE))
    if (length(pred_names) == 0) next
    
    predominant_action <- as.integer(pred_names[1])
    if (is.na(predominant_action) || predominant_action < 1 || predominant_action > length(act_labels)) {
      predominant_action <- 1  # default to first action
    }
    
    # compute proportion assigned to each treatment
    prop_control <- sum(leaf_preds == 1) / length(leaf_preds)
    prop_treat <- sum(leaf_preds == 2) / length(leaf_preds)
    
    # compute treatment effect (difference in conditional means)
    if (length(avg_cm) == 2) {
      treatment_effect <- avg_cm[2] - avg_cm[1]
      
      # transform to original scale if possible
      treatment_effect_display <- NULL
      original_scale_text <- ""
      if (!is.null(transform_info)) {
        if (transform_info$has_z && !transform_info$has_log) {
          # simple z-transformation: multiply by SD
          treatment_effect_display <- treatment_effect * transform_info$orig_sd
          original_scale_text <- paste0(" (original scale: ", format_minimal_decimals(treatment_effect_display), " units)")
        } else if (transform_info$has_z && transform_info$has_log) {
          # log+z transformation: use multiplicative interpretation
          # detect units
          units_info <- detect_variable_units(transform_info$original_var)
          
          # calculate the effect on the log scale
          delta_log <- treatment_effect * transform_info$log_sd
          
          # get multiplicative effect
          ratio <- exp(delta_log)
          
          # calculate percentage change
          pct_change <- (ratio - 1) * 100
          
          # calculate mean on original scale for absolute change
          # use display mean if available (for corrected population statistics)
          log_mean_to_use <- if (!is.null(transform_info$use_display_mean) && transform_info$use_display_mean) {
            transform_info$log_mean_display
          } else {
            transform_info$log_mean
          }
          pop_mean_orig <- exp(log_mean_to_use) - transform_info$log_offset
          
          # apply scale factor if needed (e.g., hours to minutes)
          if (!is.null(units_info$scale_factor)) {
            pop_mean_orig <- pop_mean_orig * units_info$scale_factor
          }
          
          # calculate absolute change
          abs_change <- pop_mean_orig * (ratio - 1)
          
          # format based on unit type
          change_word <- if (pct_change >= 0) "increase" else "decrease"
          abs_pct_change <- abs(pct_change)
          
          if (units_info$type == "monetary") {
            original_scale_text <- sprintf(" (%.0f%% multiplicative %s, ~%s%s average %s)",
                                         abs_pct_change, change_word,
                                         units_info$symbol, format_minimal_decimals(abs(abs_change)),
                                         change_word)
          } else if (units_info$type == "time") {
            original_scale_text <- sprintf(" (%.0f%% multiplicative %s, ~%s %s average %s)",
                                         abs_pct_change, change_word,
                                         format_minimal_decimals(abs(abs_change)),
                                         units_info$name,
                                         change_word)
          } else {
            # generic format for other types
            original_scale_text <- sprintf(" (%.1fx multiplicative effect)",
                                         ratio)
          }
        }
      }
      
      if (output_format == "prose") {
        # prose format for flowing text
        pct_assigned <- round(100 * max(prop_control, prop_treat), 1)
        pct_of_test <- round(100 * length(idx) / length(predictions), 1)
        
        # determine if everyone or partial assignment
        assignment_text <- if (pct_assigned == 100) {
          paste0("every participant in this group was recommended ", tolower(act_labels[predominant_action]))
        } else {
          paste0(pct_assigned, "% were recommended ", tolower(act_labels[predominant_action]))
        }
        
        text <- paste0(text,
          "• **Leaf ", i, "—", tolower(leaf_names[i]), "** ",
          "(n = ", formatC(length(idx), format="d", big.mark=","), "; ", pct_of_test, "% of the test set): ",
          assignment_text, ". "
        )
        
        # add outcomes with minimal decimals
        text <- paste0(text,
          "The mean outcome under control was ", format_minimal_decimals(avg_cm[1]), ", ",
          "the mean under treatment was ", format_minimal_decimals(avg_cm[2]), ", ",
          "yielding a CATE of ", format_minimal_decimals(treatment_effect), original_scale_text, ".\n\n"
        )
        
      } else if (use_math_notation) {
        text <- paste0(text,
          "**Leaf ", i, " (", leaf_names[i], ")**\n",
          "n = ", length(idx), "; ",
          "assigned treatment: ", act_labels[predominant_action], " (",
          round(100 * max(prop_control, prop_treat), 1), "%)\n\n",
          "E[Y(0)|X∈leaf] = ", format_minimal_decimals(avg_cm[1]), "\n",
          "E[Y(1)|X∈leaf] = ", format_minimal_decimals(avg_cm[2]), "\n", 
          "CATE = ", format_minimal_decimals(treatment_effect), original_scale_text, "\n\n"
        )
      } else {
        # clear, simple format
        text <- paste0(text,
          "**Leaf ", i, " (", leaf_names[i], ")**\n",
          "Sample size: ", length(idx), " (", round(100 * length(idx) / length(predictions), 1), "% of test set)\n",
          "Recommended treatment: ", act_labels[predominant_action], " (",
          round(100 * max(prop_control, prop_treat), 1), "% of this group)\n\n",
          "Control outcome: ", format_minimal_decimals(avg_cm[1]), "\n",
          "Treatment outcome: ", format_minimal_decimals(avg_cm[2]), "\n", 
          "Treatment effect (CATE): ", format_minimal_decimals(treatment_effect), original_scale_text, "\n"
        )
        
        text <- paste0(text, "\n")
      }
    }
  }
  
  return(text)
}
