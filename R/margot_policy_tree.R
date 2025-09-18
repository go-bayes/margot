#' Recompute Policy Trees with Custom Parameters
#'
#' @description
#' Computes policy trees for causal forest models with flexible covariate selection
#' and train/test split options. This function provides a direct way to generate
#' policy trees without running full causal forest analysis, paralleling the
#' functionality of margot_rate() and margot_qini().
#'
#' @param model_results List returned by margot_causal_forest() or margot_flip_forests(),
#'   containing results and optionally covariates and data.
#' @param model_names Optional character vector specifying which models to process.
#'   Default NULL (all models).
#' @param custom_covariates Character vector of covariate names to use for policy trees.
#'   If NULL, uses the original top variables from the model.
#' @param exclude_covariates Character vector of covariate names or patterns to exclude.
#'   Supports exact matches and regex patterns (e.g., "_log" excludes all variables
#'   containing "_log").
#' @param covariate_mode Character string specifying how to handle covariates:
#'   \itemize{
#'     \item{"original"}{Use original top variables from model (default)}
#'     \item{"custom"}{Use only the specified custom_covariates}
#'     \item{"add"}{Add custom_covariates to existing top variables}
#'     \item{"all"}{Use all available covariates}
#'   }
#' @param depth Numeric or character specifying which depth(s) to compute:
#'   1 for single split, 2 for two splits, or "both" for both depths (default).
#' @param train_proportion Numeric value between 0 and 1 for the proportion of data
#'   used for training depth-2 trees. Default is 0.5. Note: depth-1 trees use all
#'   available data but only the selected covariates (same as depth-2).
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param verbose Logical; print progress messages (default TRUE).
#' @param seed Integer; base seed for reproducible computations (default 12345).
#' @param tree_method Character string specifying the package to use: "policytree"
#'   (default) or "fastpolicytree". The fastpolicytree package provides ~10x faster
#'   computation with identical results. Falls back to policytree if fastpolicytree
#'   is not installed.
#'
#' @return A list structured similarly to margot_causal_forest() output, containing:
#' \itemize{
#'   \item \code{results}: List where each element corresponds to a model and contains:
#'     \itemize{
#'       \item \code{dr_scores}: Doubly robust scores (original or flipped if available)
#'       \item \code{policy_tree_depth_1}: Single-split policy tree (if requested)
#'       \item \code{policy_tree_depth_2}: Two-split policy tree (if requested and possible)
#'       \item \code{plot_data}: Data for visualization (X_test, X_test_full, predictions)
#'       \item \code{top_vars}: Variables used for policy trees
#'       \item \code{policy_tree_covariates}: Final covariate selection
#'       \item \code{policy_tree_metadata}: Metadata about the computation
#'     }
#'   \item \code{covariates}: The covariate matrix used
#'   \item \code{not_missing}: Indices of complete cases
#'   \item \code{train_proportion}: The train/test split proportion used
#' }
#'
#' @details
#' This function allows you to:
#' \itemize{
#'   \item Exclude specific covariates (e.g., log-transformed variables with "_log")
#'   \item Use custom covariate sets for policy optimization
#'   \item Adjust the train/test split for depth-2 trees
#'   \item Recompute policy trees without re-running causal forests
#' }
#'
#' The output is structured to be compatible with margot_policy(),
#' margot_plot_policy_tree(), margot_plot_policy_combo(), and
#' margot_interpret_policy_tree().
#'
#' @examples
#' \dontrun{
#' # Recompute policy trees with default settings
#' policy_trees <- margot_policy_tree(causal_forest_results)
#'
#' # Exclude log-transformed variables
#' policy_trees_no_log <- margot_policy_tree(
#'   causal_forest_results,
#'   exclude_covariates = "_log"
#' )
#'
#' # Use custom covariates with 80/20 train/test split
#' policy_trees_custom <- margot_policy_tree(
#'   causal_forest_results,
#'   custom_covariates = c("age", "gender", "income"),
#'   covariate_mode = "custom",
#'   train_proportion = 0.8
#' )
#'
#' # Compute only depth-1 trees using all covariates
#' policy_trees_d1 <- margot_policy_tree(
#'   causal_forest_results,
#'   covariate_mode = "all",
#'   depth = 1
#' )
#'
#' # Process specific models with larger training set
#' policy_trees_selected <- margot_policy_tree(
#'   causal_forest_results,
#'   model_names = c("anxiety", "depression"),
#'   train_proportion = 0.8
#' )
#'
#' # Visualize results
#' plot <- margot_plot_policy_tree(policy_trees, "model_anxiety")
#'
#' # Create combined plots
#' plots <- margot_plot_policy_combo(policy_trees, "model_anxiety")
#' print(plots$combined_plot)
#'
#' # Interpret results
#' margot_interpret_policy_tree(policy_trees, "model_anxiety")
#' }
#'
#' @export
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_h2
#' @importFrom policytree policy_tree
#' @importFrom stats setNames complete.cases
margot_policy_tree <- function(model_results,
                               model_names = NULL,
                               custom_covariates = NULL,
                               exclude_covariates = NULL,
                               covariate_mode = c("original", "custom", "add", "all"),
                               depth = "both",
                               train_proportion = 0.5,
                               label_mapping = NULL,
                               verbose = TRUE,
                               seed = 12345,
                               tree_method = c("policytree", "fastpolicytree")) {
  # validate inputs
  if (!is.list(model_results) || !"results" %in% names(model_results)) {
    stop("model_results must be a list containing a 'results' element")
  }

  # validate tree method
  tree_method <- match.arg(tree_method)
  actual_tree_method <- .get_tree_method(tree_method, verbose)

  covariate_mode <- match.arg(covariate_mode)

  # validate depth parameter
  if (is.character(depth)) {
    if (!depth %in% c("both", "1", "2")) {
      stop("depth must be 1, 2, or 'both'")
    }
    if (depth == "both") {
      compute_depth1 <- TRUE
      compute_depth2 <- TRUE
    } else {
      compute_depth1 <- depth == "1"
      compute_depth2 <- depth == "2"
    }
  } else if (is.numeric(depth)) {
    if (!depth %in% c(1, 2)) {
      stop("depth must be 1, 2, or 'both'")
    }
    compute_depth1 <- depth == 1
    compute_depth2 <- depth == 2
  } else {
    stop("depth must be 1, 2, or 'both'")
  }

  # validate train_proportion
  if (train_proportion <= 0 || train_proportion >= 1) {
    stop("train_proportion must be between 0 and 1 (exclusive)")
  }

  # check for required data
  if (is.null(model_results$covariates)) {
    stop("covariates not found in model_results. Ensure save_data = TRUE in margot_causal_forest()")
  }

  # validate custom covariates exist
  if (!is.null(custom_covariates)) {
    available_covars <- colnames(model_results$covariates)
    missing_covars <- setdiff(custom_covariates, available_covars)
    if (length(missing_covars) > 0) {
      stop(paste(
        "The following covariates are not available:",
        paste(missing_covars, collapse = ", ")
      ))
    }
  }

  # determine which models to process
  if (is.null(model_names)) {
    model_names <- names(model_results$results)
  } else {
    # add model_ prefix if needed
    model_names <- ifelse(
      grepl("^model_", model_names),
      model_names,
      paste0("model_", model_names)
    )
    # validate they exist
    missing_models <- setdiff(model_names, names(model_results$results))
    if (length(missing_models) > 0) {
      stop(paste(
        "The following models are not available:",
        paste(missing_models, collapse = ", ")
      ))
    }
  }

  if (verbose) {
    cli::cli_h2("Computing policy trees for {length(model_names)} model{?s}")
    if (!is.null(exclude_covariates)) {
      cli::cli_alert_info("Excluding covariates matching: {paste(exclude_covariates, collapse = ', ')}")
    }
    if (!is.null(custom_covariates)) {
      cli::cli_alert_info("Using custom covariates: {paste(custom_covariates, collapse = ', ')}")
    }
  }

  # get data indices
  covariates <- model_results$covariates
  not_missing <- model_results$not_missing
  if (is.null(not_missing)) {
    not_missing <- which(complete.cases(covariates))
  }

  # create output structure
  output <- list(
    results = list(),
    covariates = covariates,
    not_missing = not_missing,
    train_proportion = train_proportion
  )

  # process each model
  for (model_name in model_names) {
    if (verbose) cli::cli_alert_info("Processing {model_name}")

    model_result <- model_results$results[[model_name]]

    # compute policy trees using the helper function
    updated_model <- compute_policy_trees_for_model(
      model_result = model_result,
      model_name = model_name,
      covariates = covariates,
      not_missing = not_missing,
      custom_covariates = custom_covariates,
      exclude_covariates = exclude_covariates,
      covariate_mode = covariate_mode,
      compute_depth1 = compute_depth1,
      compute_depth2 = compute_depth2,
      train_proportion = train_proportion,
      verbose = verbose,
      seed = seed
    )

    output$results[[model_name]] <- updated_model
  }

  if (verbose) cli::cli_alert_success("Policy tree computation completed")

  class(output) <- c("margot_policy_tree", class(output))
  return(output)
}

#' Compute policy trees for a single model
#' @keywords internal
compute_policy_trees_for_model <- function(model_result,
                                           model_name,
                                           covariates,
                                           not_missing,
                                           custom_covariates,
                                           exclude_covariates,
                                           covariate_mode,
                                           compute_depth1,
                                           compute_depth2,
                                           train_proportion,
                                           verbose,
                                           seed) {
  # set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed + as.integer(as.factor(model_name)))
  }

  # determine covariates to use
  all_covars <- colnames(covariates)

  if (covariate_mode == "all") {
    selected_covars <- all_covars
    if (length(selected_covars) > 20 && verbose) {
      cli::cli_alert_warning(
        "Using all {length(selected_covars)} covariates for policy trees. This may be computationally intensive."
      )
    }
  } else if (covariate_mode == "add" && !is.null(custom_covariates)) {
    # combine existing top vars with custom
    existing_top_vars <- model_result$top_vars
    if (is.null(existing_top_vars)) {
      cli::cli_alert_warning("No top_vars found for {model_name}, using custom covariates only")
      selected_covars <- custom_covariates
    } else {
      selected_covars <- unique(c(existing_top_vars, custom_covariates))
    }
  } else if (covariate_mode == "custom" && !is.null(custom_covariates)) {
    selected_covars <- custom_covariates
  } else {
    # use original top vars
    selected_covars <- model_result$top_vars
    if (is.null(selected_covars)) {
      stop(paste("No top_vars found for", model_name, "and no custom_covariates specified"))
    }
  }

  # ensure selected covariates exist
  selected_covars <- intersect(selected_covars, all_covars)

  # apply exclusions
  if (!is.null(exclude_covariates)) {
    selected_covars <- apply_covariate_exclusions(selected_covars, exclude_covariates, verbose)
  }

  if (length(selected_covars) == 0) {
    stop(paste("No covariates remaining for", model_name, "after applying exclusions"))
  }

  # get dr_scores (use flipped if available)
  dr_scores <- model_result$dr_scores
  if (is.null(dr_scores)) {
    dr_scores <- model_result$dr_scores_flipped
  }
  if (is.null(dr_scores)) {
    stop(paste("No dr_scores found for", model_name))
  }

  # create output structure copying relevant fields
  output <- list(
    dr_scores = dr_scores,
    top_vars = selected_covars
  )

  # copy over existing fields that might be needed
  fields_to_copy <- c(
    "ate", "test_calibration", "custom_table", "tau_hat",
    "rate_result", "rate_qini", "conditional_means",
    "qini_data", "qini_objects", "qini_metadata"
  )
  for (field in fields_to_copy) {
    if (!is.null(model_result[[field]])) {
      output[[field]] <- model_result[[field]]
    }
  }

  # compute depth-1 tree if requested
  if (compute_depth1) {
    output$policy_tree_depth_1 <- .compute_policy_tree(
      covariates[not_missing, selected_covars, drop = FALSE],
      dr_scores[not_missing, ],
      depth = 1,
      tree_method = actual_tree_method
    )
  }

  # compute depth-2 tree if requested
  if (compute_depth2) {
    # handle depth-2 tree
    depth2_covars <- selected_covars
    auto_expanded <- FALSE

    # auto-expand if needed for depth-2
    if (length(selected_covars) < 2 && covariate_mode == "custom" && !is.null(model_result$top_vars)) {
      depth2_covars <- unique(c(selected_covars, model_result$top_vars))

      # apply exclusions to expanded set
      if (!is.null(exclude_covariates)) {
        depth2_covars <- apply_covariate_exclusions(depth2_covars, exclude_covariates, verbose = FALSE)
      }

      if (length(depth2_covars) > length(selected_covars)) {
        auto_expanded <- TRUE
        if (verbose) {
          cli::cli_alert_warning(
            "Only {length(selected_covars)} covariate(s) specified. Auto-adding original variables for depth-2 tree ({length(depth2_covars)} total)"
          )
        }
      }
    }

    # create depth-2 tree if possible
    if (length(depth2_covars) >= 2) {
      # train/test split
      train_size <- floor(train_proportion * length(not_missing))
      train_idx <- sample(not_missing, train_size)
      test_idx <- setdiff(not_missing, train_idx)

      # fit depth-2 tree
      output$policy_tree_depth_2 <- .compute_policy_tree(
        covariates[train_idx, depth2_covars, drop = FALSE],
        dr_scores[train_idx, ],
        depth = 2,
        tree_method = actual_tree_method
      )

      # create plot data
      output$plot_data <- list(
        X_test = covariates[test_idx, depth2_covars, drop = FALSE],
        X_test_full = covariates[test_idx, , drop = FALSE],
        predictions = predict(
          output$policy_tree_depth_2,
          covariates[test_idx, depth2_covars, drop = FALSE]
        )
      )
    } else {
      # not enough covariates for depth-2
      if (verbose) {
        cli::cli_alert_warning(
          "Unable to create depth-2 policy tree. Only {length(depth2_covars)} covariate(s) available (need ≥2)"
        )
      }
      output$policy_tree_depth_2 <- NULL

      # create plot data using depth-1 if available
      if (!is.null(output$policy_tree_depth_1)) {
        test_idx <- sample(not_missing, floor((1 - train_proportion) * length(not_missing)))
        output$plot_data <- list(
          X_test = covariates[test_idx, selected_covars, drop = FALSE],
          X_test_full = covariates[test_idx, , drop = FALSE],
          predictions = predict(
            output$policy_tree_depth_1,
            covariates[test_idx, selected_covars, drop = FALSE]
          )
        )
      }
    }
  }

  # store metadata
  output$policy_tree_covariates <- selected_covars
  output$policy_tree_metadata <- list(
    covariate_mode = covariate_mode,
    train_proportion = train_proportion,
    tree_method = actual_tree_method,
    actual_train_size = if (compute_depth2 && exists("train_idx")) length(train_idx) else NA,
    actual_test_size = if (compute_depth2 && exists("test_idx")) length(test_idx) else NA,
    depth1_covariates = if (compute_depth1) selected_covars else NULL,
    depth2_covariates = if (compute_depth2 && !is.null(output$policy_tree_depth_2)) {
      depth2_covars
    } else {
      NULL
    },
    depth2_auto_expanded = if (compute_depth2) auto_expanded else NA,
    seed_used = seed + as.integer(as.factor(model_name))
  )

  return(output)
}

#' Apply covariate exclusions (copied from margot_recalculate_policy_trees.R)
#' @keywords internal
apply_covariate_exclusions <- function(covariates, exclude_patterns, verbose = TRUE) {
  if (is.null(exclude_patterns) || length(exclude_patterns) == 0) {
    return(covariates)
  }

  excluded <- character(0)

  for (pattern in exclude_patterns) {
    # check for substring match first (e.g., "_log" matches "var_log" and "log_var")
    substring_matches <- covariates[grepl(pattern, covariates, fixed = TRUE)]
    if (length(substring_matches) > 0) {
      excluded <- c(excluded, substring_matches)
    } else {
      # try as regex if no fixed matches
      tryCatch(
        {
          regex_matches <- grep(pattern, covariates, value = TRUE)
          if (length(regex_matches) > 0) {
            excluded <- c(excluded, regex_matches)
          }
        },
        error = function(e) {
          # if regex fails, treat as exact match
          if (pattern %in% covariates) {
            excluded <- c(excluded, pattern)
          }
        }
      )
    }
  }

  excluded <- unique(excluded)
  remaining <- setdiff(covariates, excluded)

  if (verbose && length(excluded) > 0) {
    cli::cli_alert_info("Excluded {length(excluded)} covariate(s): {paste(excluded, collapse = ', ')}")
  }

  return(remaining)
}
