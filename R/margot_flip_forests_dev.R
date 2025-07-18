#' Development Version of Flip CATE Estimates with Custom Covariate Support
#'
#' @description
#' This development function extends margot_flip_forests to allow custom covariate
#' specification for policy tree recalculation. Users can flip CATE estimates,
#' recalculate RATE/QINI results, and explore policy trees with different sets of
#' covariates than those originally selected by margot_causal_forest.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param flip_outcomes A character vector of outcome variable names for which CATE estimates should be flipped.
#'   Can be NULL if only recalculating policy trees with custom covariates.
#' @param custom_covariates A character vector of covariate names to use for policy tree calculation.
#'   If NULL, uses the original top_n_vars from the model.
#' @param exclude_covariates A character vector of covariate names or patterns to exclude from policy trees.
#'   Supports exact matches and regex patterns (e.g., "^t0_log_" to exclude all variables starting with "t0_log_").
#'   Exclusions are applied after covariate selection/combination.
#' @param covariate_mode Character string specifying how to use custom covariates:
#'   \itemize{
#'     \item{"custom"}{Use only the specified custom_covariates}
#'     \item{"add"}{Add custom_covariates to existing top_n_vars}
#'     \item{"all"}{Use all available covariates (warns if > 20)}
#'   }
#' @param model_prefix A character string indicating the prefix used for model names in the results list. Default is "model_".
#' @param recalc_policy Logical; if TRUE (default) recalculates policy trees.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A modified copy of the model_results list with updated CATE estimates and/or recalculated policy trees.
#'
#' @details
#' When only \code{exclude_covariates} is specified (no flipping or custom covariates), the function
#' uses an optimized path that filters the existing top_vars without recalculating policy trees.
#' This significantly improves performance for sensitivity analyses.
#'
#' @examples
#' \dontrun{
#' # flip outcomes and use custom covariates
#' results_dev <- margot_flip_forests_dev(
#'   model_results,
#'   flip_outcomes = c("outcome1", "outcome2"),
#'   custom_covariates = c("age", "gender", "income"),
#'   covariate_mode = "custom"
#' )
#'
#' # only recalculate policy trees with all covariates
#' results_all <- margot_flip_forests_dev(
#'   model_results,
#'   flip_outcomes = NULL,
#'   covariate_mode = "all"
#' )
#' }
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom grf rank_average_treatment_effect
#' @export
margot_flip_forests_dev <- function(model_results,
                                    flip_outcomes = NULL,
                                    custom_covariates = NULL,
                                    exclude_covariates = NULL,
                                    covariate_mode = c("custom", "add", "all"),
                                    model_prefix = "model_",
                                    recalc_policy = TRUE,
                                    verbose = TRUE) {

  # validate inputs
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be a list containing a 'results' element (output from margot_causal_forest)")
  }

  covariate_mode <- match.arg(covariate_mode)

  # check if we have anything to do
  if (is.null(flip_outcomes) && is.null(custom_covariates) && covariate_mode != "all" && is.null(exclude_covariates)) {
    if (verbose) cli::cli_alert_info("no outcomes to flip and no custom covariates specified")
    return(model_results)
  }

  # optimized path for exclusion-only operations
  exclusion_only <- !is.null(exclude_covariates) &&
                    is.null(flip_outcomes) &&
                    is.null(custom_covariates) &&
                    covariate_mode == "custom"

  # validate covariates exist
  if (!is.null(model_results$covariates)) {
    available_covars <- colnames(model_results$covariates)
    if (!is.null(custom_covariates)) {
      missing_covars <- setdiff(custom_covariates, available_covars)
      if (length(missing_covars) > 0) {
        stop(paste("the following covariates are not available:",
                   paste(missing_covars, collapse = ", ")))
      }
    }
  } else if (recalc_policy && (covariate_mode == "all" || !is.null(custom_covariates))) {
    stop("covariates not found in model_results. ensure save_data = TRUE in margot_causal_forest")
  }

  # copy results to avoid side effects
  results_copy <- model_results

  # handle optimized exclusion-only path
  if (exclusion_only) {
    if (verbose) cli::cli_alert_info("using optimized path for exclusion-only operation")

    # process each model
    for (model_name in names(results_copy$results)) {
      model_result <- results_copy$results[[model_name]]

      if (!is.null(model_result$top_vars)) {
        # apply exclusions to existing top_vars
        filtered_vars <- apply_covariate_exclusions(model_result$top_vars, exclude_covariates, verbose)

        if (length(filtered_vars) == 0) {
          if (verbose) cli::cli_alert_warning("after exclusions, 0 covariates remain for {model_name}. no policy trees can be computed.")
          model_result$policy_tree_depth_1 <- NULL
          model_result$policy_tree_depth_2 <- NULL
          model_result$plot_data <- NULL
          model_result$policy_tree_covariates <- character(0)
          model_result$policy_tree_exclusion_applied <- TRUE
        } else if (length(filtered_vars) == 1) {
          if (verbose) cli::cli_alert_warning("after exclusions, only 1 covariate remains for {model_name}. only depth-1 policy tree available.")
          # keep depth-1, remove depth-2
          model_result$policy_tree_depth_2 <- NULL
          # update plot_data to use depth-1 predictions
          if (!is.null(model_result$plot_data)) {
            model_result$plot_data$predictions <- predict(
              model_result$policy_tree_depth_1,
              model_result$plot_data$X_test_full[, filtered_vars, drop = FALSE]
            )
            model_result$plot_data$X_test <- model_result$plot_data$X_test_full[, filtered_vars, drop = FALSE]
          }
          model_result$policy_tree_covariates <- filtered_vars
          model_result$policy_tree_depth_1_covariates <- filtered_vars
          model_result$policy_tree_depth_2_covariates <- NULL
          model_result$policy_tree_exclusion_applied <- TRUE
        } else {
          # 2+ variables - update metadata only
          if (verbose) cli::cli_alert_success("after exclusions, {length(filtered_vars)} covariates remain for {model_name}")
          model_result$policy_tree_covariates <- filtered_vars
          model_result$top_vars <- filtered_vars
          model_result$policy_tree_exclusion_applied <- TRUE
          # note: existing policy trees remain valid, just with filtered covariates
        }

        results_copy$results[[model_name]] <- model_result
      }
    }

    results_copy$exclusion_only_optimization <- TRUE
    return(results_copy)
  }

  # initialize models_to_flip
  models_to_flip <- character(0)

  # handle flipping if requested
  if (!is.null(flip_outcomes) && length(flip_outcomes) > 0) {
    # identify models to flip
    all_models <- names(results_copy$results)
    models_to_flip <- paste0(ifelse(grepl(paste0("^", model_prefix), flip_outcomes), "", model_prefix),
                             flip_outcomes)
    models_to_flip <- models_to_flip[models_to_flip %in% all_models]

    if (length(models_to_flip) == 0) {
      warning("none of the specified outcomes match any models in the results")
    } else {
      if (verbose) {
        cli::cli_alert_info(paste("flipping cate estimates for", length(models_to_flip),
                                  "outcomes:", paste(gsub(model_prefix, "", models_to_flip), collapse = ", ")))
      }

      # process each model (same as original function)
      for (model_name in models_to_flip) {
        if (verbose) cli::cli_alert_info(paste("processing", model_name))
        model_result <- results_copy$results[[model_name]]
        if (is.null(model_result) || !is.list(model_result)) {
          if (verbose) cli::cli_alert_warning(paste("skipping", model_name, "- invalid or missing results"))
          next
        }

        # 1. flip tau_hat
        if (!is.null(model_result$tau_hat)) {
          model_result$tau_hat_original <- model_result$tau_hat
          model_result$tau_hat <- -model_result$tau_hat
          if (verbose) cli::cli_alert_info(paste("flipped tau_hat for", model_name))
        }

        # 2. recalc RATE and QINI RATE
        if (!is.null(results_copy$full_models) && model_name %in% names(results_copy$full_models)) {
          model_obj <- results_copy$full_models[[model_name]]
          flipped_tau_hat <- model_result$tau_hat

          if (!is.null(model_result$rate_result)) {
            tryCatch({
              model_result$rate_result_original <- model_result$rate_result
              model_result$rate_result <- grf::rank_average_treatment_effect(model_obj, flipped_tau_hat)
              if (verbose) cli::cli_alert_info(paste("recalculated RATE for", model_name))
            }, error = function(e) {
              if (verbose) cli::cli_alert_warning(paste("error recalculating RATE for", model_name, "-", e$message))
            })
          }
          if (!is.null(model_result$rate_qini)) {
            tryCatch({
              model_result$rate_qini_original <- model_result$rate_qini
              model_result$rate_qini <- grf::rank_average_treatment_effect(model_obj,
                                                                           flipped_tau_hat,
                                                                           target = "QINI")
              if (verbose) cli::cli_alert_info(paste("recalculated QINI RATE for", model_name))
            }, error = function(e) {
              if (verbose) cli::cli_alert_warning(paste("error recalculating QINI RATE for", model_name, "-", e$message))
            })
          }
        }

        # 3. mark QINI curves for recalculation
        if (!is.null(model_result$qini_data)) {
          model_result$qini_data_original <- model_result$qini_data
          model_result$qini_needs_recalculation <- TRUE
          if (verbose) cli::cli_alert_info(paste("marked QINI for recalculation for", model_name))
        }

        # 4. flip double-robust scores and flag trees
        if (!is.null(model_result$dr_scores)) {
          dr_scores <- model_result$dr_scores
          tryCatch({
            if (is.matrix(dr_scores)) {
              model_result$dr_scores_original <- dr_scores
              model_result$dr_scores_flipped <- -dr_scores
              model_result$policy_trees_need_recalculation <- TRUE
              if (verbose) cli::cli_alert_info(paste("flipped dr_scores for", model_name))
            } else if (verbose) {
              cli::cli_alert_warning(paste("dr_scores for", model_name, "is not a matrix"))
            }
          }, error = function(e) {
            if (verbose) cli::cli_alert_warning(paste("error processing dr_scores for", model_name, "-", e$message))
          })
        }

        # update the result copy
        results_copy$results[[model_name]] <- model_result
      }

      # record flipped outcomes
      results_copy$flip_outcomes <- gsub(model_prefix, "", models_to_flip)
      results_copy$flip_outcomes_postprocessed <- TRUE
      if (verbose) cli::cli_alert_success("finished flipping specified outcomes")
    }
  }

  # handle policy tree recalculation with custom covariates
  if (recalc_policy) {
    # determine which outcomes to recalculate
    if (!is.null(custom_covariates) || covariate_mode == "all") {
      # if custom covariates specified, recalculate all outcomes
      outcomes_to_recalc <- gsub(model_prefix, "", names(results_copy$results))
    } else if (length(models_to_flip) > 0) {
      # otherwise only recalculate flipped outcomes
      outcomes_to_recalc <- gsub(model_prefix, "", models_to_flip)
    } else {
      outcomes_to_recalc <- character(0)
    }

    if (length(outcomes_to_recalc) > 0) {
      results_copy <- margot_recalculate_policy_trees_dev(
        model_results = results_copy,
        outcomes_to_recalculate = outcomes_to_recalc,
        custom_covariates = custom_covariates,
        exclude_covariates = exclude_covariates,
        covariate_mode = covariate_mode,
        model_prefix = model_prefix,
        verbose = verbose
      )
    }
  }

  return(results_copy)
}

# internal helper for policy tree recalculation with custom covariates
#' @keywords internal
margot_recalculate_policy_trees_dev <- function(model_results,
                                                outcomes_to_recalculate = NULL,
                                                custom_covariates = NULL,
                                                exclude_covariates = NULL,
                                                covariate_mode = "custom",
                                                model_prefix = "model_",
                                                verbose = TRUE,
                                                parallel = FALSE,
                                                n_cores_policy = future::availableCores() - 1,
                                                seed = NULL) {

  if (!is.list(model_results) || !"results" %in% names(model_results))
    stop("model_results must be a list containing a 'results' element")

  results_copy <- model_results

  # which outcomes
  if (is.null(outcomes_to_recalculate)) {
    outcomes_to_recalculate <- names(results_copy$results)[
      vapply(results_copy$results, function(x)
        isTRUE(x$policy_trees_need_recalculation), logical(1))
    ]
  } else {
    outcomes_to_recalculate <- paste0(
      ifelse(grepl(paste0("^", model_prefix), outcomes_to_recalculate), "", model_prefix),
      outcomes_to_recalculate)
  }

  if (!length(outcomes_to_recalculate)) {
    if (verbose) cli::cli_alert_info("no outcomes need policy tree recalculation")
    return(results_copy)
  }

  if (verbose)
    cli::cli_alert_info("recalculating policy trees for {length(outcomes_to_recalculate)} outcomes: {paste(gsub(model_prefix, '', outcomes_to_recalculate), collapse = ', ')}")

  covariates <- results_copy$covariates
  not_missing <- results_copy$not_missing %||% which(complete.cases(covariates))
  full <- intersect(seq_len(nrow(covariates)), not_missing)

  # choose apply function
  apply_fun <- if (parallel && length(outcomes_to_recalculate) > 1) {
    future::plan(future::multisession, workers = n_cores_policy)
    future.apply::future_lapply
  } else {
    lapply
  }

  updated <- apply_fun(outcomes_to_recalculate, function(model_name) {
    mr <- results_copy$results[[model_name]]
    .margot_fit_policy_trees_dev(model_name, mr, covariates, not_missing, full,
                                 custom_covariates, exclude_covariates, covariate_mode, verbose, seed)
  })
  names(updated) <- outcomes_to_recalculate

  results_copy$results[outcomes_to_recalculate] <- updated
  if (verbose) cli::cli_alert_success("finished recalculating policy trees")
  results_copy
}

# internal helper to fit policy trees with custom covariates
#' @keywords internal
.margot_fit_policy_trees_dev <- function(model_name,
                                        mr,
                                        covariates,
                                        not_missing,
                                        full,
                                        custom_covariates = NULL,
                                        exclude_covariates = NULL,
                                        covariate_mode = "custom",
                                        verbose = TRUE,
                                        seed = NULL) {

  if (!isTRUE(mr$policy_trees_need_recalculation) &&
      is.null(custom_covariates) &&
      is.null(exclude_covariates) &&
      covariate_mode != "all")
    return(mr)

  if (!is.null(seed)) set.seed(seed + as.integer(as.factor(model_name)))

  # determine which covariates to use
  all_covars <- colnames(covariates)

  if (covariate_mode == "all") {
    selected_covars <- all_covars
    if (length(selected_covars) > 20 && verbose) {
      cli::cli_alert_warning("using all {length(selected_covars)} covariates for policy trees. this may be computationally intensive.")
    }
  } else if (covariate_mode == "add" && !is.null(custom_covariates)) {
    # combine existing top vars with custom
    existing_top_vars <- mr$top_vars
    selected_covars <- unique(c(existing_top_vars, custom_covariates))
  } else if (covariate_mode == "custom" && !is.null(custom_covariates)) {
    selected_covars <- custom_covariates
  } else {
    # default to existing top vars
    selected_covars <- mr$top_vars
  }

  # ensure selected covariates exist
  selected_covars <- intersect(selected_covars, all_covars)

  # apply exclusions
  selected_covars <- apply_covariate_exclusions(selected_covars, exclude_covariates, verbose)

  if (length(selected_covars) == 0) {
    stop(paste("no covariates remaining for", model_name, "after applying exclusions"))
  }

  if (verbose) {
    cli::cli_alert_info("recalculating policy trees for {model_name} using {length(selected_covars)} covariates")
  }

  # check for dr_scores (use flipped if available, otherwise original)
  dr_scores_to_use <- if (!is.null(mr$dr_scores_flipped)) {
    mr$dr_scores_flipped
  } else if (!is.null(mr$dr_scores)) {
    mr$dr_scores
  } else {
    stop(paste("no dr_scores found for", model_name))
  }

  # depth-1 using all selected predictors
  mr$policy_tree_depth_1 <- policytree::policy_tree(
    covariates[full, selected_covars, drop = FALSE],
    dr_scores_to_use[full, ],
    depth = 1
  )

  # handle depth-2 tree creation
  depth2_covars <- selected_covars
  auto_expanded <- FALSE

  # if we have fewer than 2 covariates and we're in custom mode, auto-expand
  if (length(selected_covars) < 2 && covariate_mode == "custom" && !is.null(mr$top_vars)) {
    # combine custom covariates with original top vars
    depth2_covars <- unique(c(selected_covars, mr$top_vars))

    # apply exclusions to the expanded set
    depth2_covars <- apply_covariate_exclusions(depth2_covars, exclude_covariates, verbose = FALSE)

    # only set auto_expanded if we actually gained covariates
    if (length(depth2_covars) > length(selected_covars)) {
      auto_expanded <- TRUE

      if (verbose) {
        cli::cli_alert_warning("only {length(selected_covars)} custom covariate(s) specified. automatically adding original top variables for depth-2 tree. depth-2 will use {length(depth2_covars)} covariates.")
      }
    }
  }

  # create depth-2 tree if we have enough covariates
  if (length(depth2_covars) >= 2) {
    # depth-2 on selected vars with train/test split
    train_size <- floor(0.7 * length(not_missing))
    train_idx <- sample(not_missing, train_size)

    mr$policy_tree_depth_2 <- policytree::policy_tree(
      covariates[train_idx, depth2_covars, drop = FALSE],
      dr_scores_to_use[train_idx, ],
      depth = 2
    )

    test_idx <- setdiff(not_missing, train_idx)
    mr$plot_data <- list(
      X_test = covariates[test_idx, depth2_covars, drop = FALSE],
      X_test_full = covariates[test_idx, , drop = FALSE],
      predictions = predict(mr$policy_tree_depth_2, covariates[test_idx, depth2_covars, drop = FALSE])
    )
  } else {
    # still not enough covariates even after expansion
    if (verbose) {
      cli::cli_alert_warning("unable to create depth-2 policy tree. only {length(depth2_covars)} covariate(s) available, but at least 2 are required.")
    }
    mr$policy_tree_depth_2 <- NULL

    # create plot_data using depth-1 predictions
    test_idx <- setdiff(not_missing, full)
    if (length(test_idx) > 0) {
      mr$plot_data <- list(
        X_test = covariates[test_idx, selected_covars, drop = FALSE],
        X_test_full = covariates[test_idx, , drop = FALSE],
        predictions = predict(mr$policy_tree_depth_1, covariates[test_idx, selected_covars, drop = FALSE])
      )
    } else {
      mr$plot_data <- NULL
    }
  }

  # store metadata about which covariates were used
  mr$policy_tree_covariates <- selected_covars
  mr$policy_tree_covariate_mode <- covariate_mode
  mr$policy_tree_depth_1_covariates <- selected_covars
  mr$policy_tree_depth_2_covariates <- if(!is.null(mr$policy_tree_depth_2)) depth2_covars else NULL
  mr$policy_tree_depth_2_auto_expanded <- auto_expanded
  mr$policy_trees_need_recalculation <- FALSE

  mr
}

# helper function (same as in original)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# helper function to apply exclusions
#' @keywords internal
apply_covariate_exclusions <- function(covariates, exclude_patterns, verbose = TRUE) {
  if (is.null(exclude_patterns) || length(exclude_patterns) == 0) {
    return(covariates)
  }

  original_count <- length(covariates)
  excluded <- character(0)

  for (pattern in exclude_patterns) {
    # always try as regex first
    tryCatch({
      matches <- grep(pattern, covariates, value = TRUE)
      if (length(matches) > 0) {
        excluded <- c(excluded, matches)
      }
    }, error = function(e) {
      # if regex fails, treat as exact match
      if (pattern %in% covariates) {
        excluded <- c(excluded, pattern)
      }
    })
  }

  excluded <- unique(excluded)
  remaining <- setdiff(covariates, excluded)

  if (verbose && length(excluded) > 0) {
    cli::cli_alert_info("excluded {length(excluded)} covariate(s): {paste(excluded, collapse = ', ')}")
  }

  return(remaining)
}

# examples
# # exclude specific variables with measurement error
# results_dev <- margot_flip_forests_dev(
#   models_binary,
#   flip_outcomes = flip_outcomes,
#   exclude_covariates = c("t0_hlth_bmi", "^t0_log_"),
#   covariate_mode = "all"
# )
#
# # combine custom selection with exclusions
# results_dev <- margot_flip_forests_dev(
#   models_binary,
#   custom_covariates = c("t0_religion_church", "t0_age", "t0_log_income"),
#   exclude_covariates = "^t0_log_",  # removes t0_log_income
#   covariate_mode = "custom"
# )
