#' Recalculate Policy Trees with Custom Covariates
#'
#' @description
#' This function allows recalculation of policy trees using custom covariate sets.
#' It provides flexibility to explore different covariate specifications for
#' policy tree estimation while maintaining the original causal forest results.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param outcomes_to_recalculate Character vector of outcome names to recalculate.
#'   If NULL (default), recalculates all models marked as needing recalculation.
#' @param custom_covariates Character vector of covariate names to use for policy trees.
#'   If NULL, uses the original top variables from the model.
#' @param exclude_covariates Character vector of covariate names or patterns to exclude.
#'   Supports exact matches and regex patterns (e.g., "^t0_log_" excludes all
#'   variables starting with "t0_log_").
#' @param covariate_mode Character string specifying how to handle covariates:
#'   \itemize{
#'     \item{"original"}{Use original top variables from model (default)}
#'     \item{"custom"}{Use only the specified custom_covariates}
#'     \item{"add"}{Add custom_covariates to existing top variables}
#'     \item{"all"}{Use all available covariates}
#'   }
#' @param model_prefix Character string prefix for model names. Default is "model_".
#' @param verbose Logical indicating whether to display messages. Default is TRUE.
#' @param parallel Logical indicating whether to use parallel processing. Default is FALSE.
#' @param n_cores Number of cores for parallel processing. Default is availableCores() - 1.
#' @param seed Random seed for reproducibility. Default is NULL.
#' @param tree_method Character string specifying which method to use for policy tree computation:
#'   \itemize{
#'     \item{"policytree"}{Use the policytree package (default)}
#'     \item{"fastpolicytree"}{Use the fastpolicytree package (about 10x faster)}
#'   }
#'
#' @return A modified copy of model_results with recalculated policy trees.
#'
#' @details
#' This function is useful for:
#' \itemize{
#'   \item Exploring sensitivity of policy recommendations to covariate selection
#'   \item Excluding covariates with measurement error or other concerns
#'   \item Using domain knowledge to select policy-relevant covariates
#'   \item Testing robustness of policy tree findings
#' }
#'
#' The function preserves all original model results and only updates:
#' \itemize{
#'   \item policy_tree_depth_1: Single-split policy tree
#'   \item policy_tree_depth_2: Two-split policy tree (if ≥2 covariates)
#'   \item plot_data: Data for visualization
#'   \item Metadata about which covariates were used
#' }
#'
#' @examples
#' \dontrun{
#' # recalculate with custom covariates
#' results_custom <- margot_recalculate_policy_trees(
#'   model_results,
#'   custom_covariates = c("age", "gender", "income"),
#'   covariate_mode = "custom"
#' )
#'
#' # exclude measurement error variables
#' results_clean <- margot_recalculate_policy_trees(
#'   model_results,
#'   exclude_covariates = c("bmi", "^log_"),
#'   covariate_mode = "original"
#' )
#'
#' # use all covariates
#' results_all <- margot_recalculate_policy_trees(
#'   model_results,
#'   covariate_mode = "all"
#' )
#' }
#'
#' @export
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom policytree policy_tree
#' @importFrom future plan multisession availableCores
#' @importFrom future.apply future_lapply
margot_recalculate_policy_trees <- function(model_results,
                                            outcomes_to_recalculate = NULL,
                                            custom_covariates = NULL,
                                            exclude_covariates = NULL,
                                            covariate_mode = c("original", "custom", "add", "all"),
                                            model_prefix = "model_",
                                            verbose = TRUE,
                                            parallel = FALSE,
                                            n_cores = future::availableCores() - 1,
                                            seed = 12345,
                                            tree_method = c("policytree", "fastpolicytree")) {
  # validate inputs
  if (!is.list(model_results) || !"results" %in% names(model_results)) {
    stop("model_results must be a list containing a 'results' element")
  }

  covariate_mode <- match.arg(covariate_mode)
  tree_method <- match.arg(tree_method)

  # check for required data
  if (is.null(model_results$covariates)) {
    stop("covariates not found in model_results. ensure save_data = TRUE in margot_causal_forest")
  }

  # validate custom covariates exist
  if (!is.null(custom_covariates)) {
    available_covars <- colnames(model_results$covariates)
    missing_covars <- setdiff(custom_covariates, available_covars)
    if (length(missing_covars) > 0) {
      stop(paste(
        "the following covariates are not available:",
        paste(missing_covars, collapse = ", ")
      ))
    }
  }

  # copy to avoid side effects
  results_copy <- model_results

  # determine which outcomes to process
  if (is.null(outcomes_to_recalculate)) {
    # find models that need recalculation
    outcomes_to_recalculate <- names(results_copy$results)[
      vapply(results_copy$results, function(x) {
        isTRUE(x$policy_trees_need_recalculation) ||
          !is.null(custom_covariates) ||
          !is.null(exclude_covariates) ||
          covariate_mode == "all"
      }, logical(1))
    ]
  } else {
    # add model prefix if needed
    outcomes_to_recalculate <- paste0(
      ifelse(grepl(paste0("^", model_prefix), outcomes_to_recalculate), "", model_prefix),
      outcomes_to_recalculate
    )
  }

  # filter to existing models
  outcomes_to_recalculate <- intersect(outcomes_to_recalculate, names(results_copy$results))

  if (length(outcomes_to_recalculate) == 0) {
    if (verbose) cli::cli_alert_info("no outcomes need policy tree recalculation")
    return(results_copy)
  }

  if (verbose) {
    cli::cli_alert_info(
      "recalculating policy trees for {length(outcomes_to_recalculate)} outcomes: {paste(gsub(model_prefix, '', outcomes_to_recalculate), collapse = ', ')}"
    )
  }

  # get data indices
  covariates <- results_copy$covariates
  not_missing <- results_copy$not_missing
  if (is.null(not_missing)) {
    not_missing <- which(complete.cases(covariates))
  }

  # choose sequential or parallel processing
  if (parallel && length(outcomes_to_recalculate) > 1) {
    future::plan(future::multisession, workers = n_cores)
    apply_fun <- future.apply::future_lapply
    if (verbose) cli::cli_alert_info("using parallel processing with {n_cores} cores")
  } else {
    apply_fun <- lapply
  }

  # process each model
  updated_results <- apply_fun(outcomes_to_recalculate, function(model_name) {
    if (verbose && !parallel) cli::cli_alert_info("processing {model_name}")

    model_result <- results_copy$results[[model_name]]

    # recalculate policy trees
    updated_model <- recalculate_policy_trees_single(
      model_result = model_result,
      model_name = model_name,
      covariates = covariates,
      not_missing = not_missing,
      custom_covariates = custom_covariates,
      exclude_covariates = exclude_covariates,
      covariate_mode = covariate_mode,
      verbose = verbose && !parallel,
      seed = seed,
      tree_method = tree_method
    )

    return(updated_model)
  })

  # update results
  names(updated_results) <- outcomes_to_recalculate
  results_copy$results[outcomes_to_recalculate] <- updated_results

  if (verbose) cli::cli_alert_success("finished recalculating policy trees")

  return(results_copy)
}

#' Recalculate policy trees for a single model
#' @keywords internal
recalculate_policy_trees_single <- function(model_result,
                                            model_name,
                                            covariates,
                                            not_missing,
                                            custom_covariates,
                                            exclude_covariates,
                                            covariate_mode,
                                            verbose,
                                            seed,
                                            tree_method) {
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
        "using all {length(selected_covars)} covariates for policy trees. this may be computationally intensive."
      )
    }
  } else if (covariate_mode == "add" && !is.null(custom_covariates)) {
    # combine existing top vars with custom
    existing_top_vars <- model_result$top_vars
    if (is.null(existing_top_vars)) {
      cli::cli_alert_warning("no top_vars found for {model_name}, using custom covariates only")
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
      stop(paste("no top_vars found for", model_name, "and no custom_covariates specified"))
    }
  }

  # ensure selected covariates exist
  selected_covars <- intersect(selected_covars, all_covars)

  # apply exclusions
  if (!is.null(exclude_covariates)) {
    selected_covars <- apply_covariate_exclusions(selected_covars, exclude_covariates, verbose)
  }

  if (length(selected_covars) == 0) {
    stop(paste("no covariates remaining for", model_name, "after applying exclusions"))
  }

  # get dr_scores (use flipped if available)
  dr_scores <- model_result$dr_scores
  if (is.null(dr_scores)) {
    dr_scores <- model_result$dr_scores_flipped
  }
  if (is.null(dr_scores)) {
    stop(paste("no dr_scores found for", model_name))
  }

  # fit depth-1 policy tree
  model_result$policy_tree_depth_1 <- .compute_policy_tree(
    covariates[not_missing, selected_covars, drop = FALSE],
    dr_scores[not_missing, ],
    depth = 1,
    tree_method = tree_method
  )

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
          "only {length(selected_covars)} covariate(s) specified. auto-adding original variables for depth-2 tree ({length(depth2_covars)} total)"
        )
      }
    }
  }

  # create depth-2 tree if possible
  if (length(depth2_covars) >= 2) {
    # train/test split
    train_size <- floor(0.7 * length(not_missing))
    train_idx <- sample(not_missing, train_size)
    test_idx <- setdiff(not_missing, train_idx)

    # fit depth-2 tree
    model_result$policy_tree_depth_2 <- .compute_policy_tree(
      covariates[train_idx, depth2_covars, drop = FALSE],
      dr_scores[train_idx, ],
      depth = 2,
      tree_method = tree_method
    )

    # create plot data
    model_result$plot_data <- list(
      X_test = covariates[test_idx, depth2_covars, drop = FALSE],
      X_test_full = covariates[test_idx, , drop = FALSE],
      predictions = predict(
        model_result$policy_tree_depth_2,
        covariates[test_idx, depth2_covars, drop = FALSE]
      ),
      test_indices = test_idx
    )
  } else {
    # not enough covariates for depth-2
    if (verbose) {
      cli::cli_alert_warning(
        "unable to create depth-2 policy tree. only {length(depth2_covars)} covariate(s) available (need ≥2)"
      )
    }
    model_result$policy_tree_depth_2 <- NULL

    # create plot data using depth-1
    test_idx <- sample(not_missing, floor(0.3 * length(not_missing)))
    model_result$plot_data <- list(
      X_test = covariates[test_idx, selected_covars, drop = FALSE],
      X_test_full = covariates[test_idx, , drop = FALSE],
      predictions = predict(
        model_result$policy_tree_depth_1,
        covariates[test_idx, selected_covars, drop = FALSE]
      ),
      test_indices = test_idx
    )
  }

  # store metadata
  model_result$policy_tree_covariates <- selected_covars
  model_result$policy_tree_covariate_mode <- covariate_mode
  model_result$policy_tree_depth_1_covariates <- selected_covars
  model_result$policy_tree_depth_2_covariates <- if (!is.null(model_result$policy_tree_depth_2)) {
    depth2_covars
  } else {
    NULL
  }
  model_result$policy_tree_depth_2_auto_expanded <- auto_expanded
  model_result$policy_trees_need_recalculation <- FALSE

  return(model_result)
}

#' Apply covariate exclusions
#' @keywords internal
apply_covariate_exclusions <- function(covariates, exclude_patterns, verbose = TRUE) {
  if (is.null(exclude_patterns) || length(exclude_patterns) == 0) {
    return(covariates)
  }

  excluded <- character(0)

  for (pattern in exclude_patterns) {
    # try as regex first
    tryCatch(
      {
        matches <- grep(pattern, covariates, value = TRUE)
        if (length(matches) > 0) {
          excluded <- c(excluded, matches)
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

  excluded <- unique(excluded)
  remaining <- setdiff(covariates, excluded)

  if (verbose && length(excluded) > 0) {
    cli::cli_alert_info("excluded {length(excluded)} covariate(s): {paste(excluded, collapse = ', ')}")
  }

  return(remaining)
}
