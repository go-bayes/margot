# updated: recalc policy trees with optional parallelisation ---------------
#' @keywords internal
margot_recalculate_policy_trees <- function(model_results,
                                            outcomes_to_recalculate = NULL,
                                            model_prefix = "model_",
                                            verbose = TRUE,
                                            parallel = FALSE,
                                            n_cores_policy = future::availableCores() - 1,
                                            seed = 12345) {
  if (!is.list(model_results) || !"results" %in% names(model_results)) {
    stop("model_results must be a list containing a 'results' element")
  }

  results_copy <- model_results

  # which outcomes ----------------------------------------------------------
  if (is.null(outcomes_to_recalculate)) {
    outcomes_to_recalculate <- names(results_copy$results)[
      vapply(results_copy$results, function(x) {
        isTRUE(x$policy_trees_need_recalculation)
      }, logical(1))
    ]
  } else {
    outcomes_to_recalculate <- paste0(
      ifelse(grepl(paste0("^", model_prefix), outcomes_to_recalculate), "", model_prefix),
      outcomes_to_recalculate
    )
  }
  if (!length(outcomes_to_recalculate)) {
    if (verbose) cli::cli_alert_info("no outcomes need policy tree recalculation")
    return(results_copy)
  }
  if (verbose) {
    cli::cli_alert_info("recalculating policy trees for {length(outcomes_to_recalculate)} outcomes: {paste(gsub(model_prefix, '', outcomes_to_recalculate), collapse = ', ')}")
  }

  covariates <- results_copy$covariates
  not_missing <- results_copy$not_missing %||% which(complete.cases(covariates))
  full <- intersect(seq_len(nrow(covariates)), not_missing)

  # choose apply ------------------------------------------------------------
  apply_fun <- if (parallel && length(outcomes_to_recalculate) > 1) {
    future::plan(future::multisession, workers = n_cores_policy)
    future.apply::future_lapply
  } else {
    lapply
  }

  updated <- apply_fun(outcomes_to_recalculate, function(model_name) {
    mr <- results_copy$results[[model_name]]
    .margot_fit_policy_trees(model_name, mr, covariates, not_missing, full, verbose, seed)
  })
  names(updated) <- outcomes_to_recalculate

  results_copy$results[outcomes_to_recalculate] <- updated
  if (verbose) cli::cli_alert_success("finished recalculating policy trees")
  results_copy
}

# helper
`%||%` <- function(a, b) if (!is.null(a)) a else b


#' Flip (Reverse) Causal Forest Treatment Effects
#'
#' @description
#' Creates new models with reversed treatment effects by flipping the outcome variable
#' and recomputing the entire causal forest from scratch. New models are appended with
#' "_r" suffix to indicate reduced/reversed outcomes.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param flip_outcomes A character vector of outcome variable names for which models should be flipped.
#' @param model_prefix A character string indicating the prefix used for model names in the results list. Default is "model_".
#' @param grf_defaults A list of parameters to pass to the GRF causal forest models. Default is NULL,
#'        which attempts to extract parameters from the original fitted models. Providing explicit values
#'        ensures consistency between original and flipped models. Common parameters include num.trees,
#'        honesty, honesty.fraction, alpha, min.node.size, and mtry.
#' @param parallel Logical indicating whether to use parallel processing. Default is FALSE.
#' @param n_cores Number of cores to use for parallel processing. Default is availableCores() - 1.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#' @param remove_original Logical indicating whether to remove the original (non-flipped) models after
#'        creating their flipped versions. Default is TRUE. When TRUE, only the flipped models with "_r"
#'        suffix remain; when FALSE, both original and flipped models are kept.
#' @param qini_treatment_cost Scalar treatment cost per unit for QINI calculations. Default 1.
#'        Lower values (e.g., 0.2) represent cheap treatments creating steeper QINI curves;
#'        higher values (e.g., 5) represent expensive treatments creating shallower curves.
#' @param seed Random seed for reproducible QINI curve generation. Default 12345 matches the
#'        default seed used in compute_qini_improved for consistency.
#'
#' @return A modified copy of model_results with flipped models (with "_r" suffix). If remove_original
#'         is TRUE (default), original models are removed; otherwise both versions are kept.
#'
#' @details
#' This function creates entirely new models by:
#' 1. Extracting the original data for each outcome
#' 2. Flipping the outcome variable (Y_flipped = -Y)
#' 3. Calling margot_causal_forest() with the flipped outcome
#' 4. Storing the result with "_r" suffix
#' 5. Optionally removing the original models (default behavior)
#'
#' This ensures all components (forest model, CATE estimates, QINI curves, policy trees,
#' E-values, etc.) are consistently computed based on the flipped outcome.
#'
#' When grf_defaults is NULL, the function attempts to extract key GRF parameters from
#' the first fitted model to ensure consistency between original and flipped models.
#'
#' @examples
#' \dontrun{
#' # Flip models and remove originals (default)
#' results_flipped <- margot_flip_forests(
#'   results,
#'   flip_outcomes = c("anxiety", "depression")
#' )
#' # Results now contain anxiety_r and depression_r, originals removed
#'
#' # Keep both original and flipped models
#' results_both <- margot_flip_forests(
#'   results,
#'   flip_outcomes = c("anxiety", "depression"),
#'   remove_original = FALSE
#' )
#' # Results contain both anxiety/depression and anxiety_r/depression_r
#'
#' # Use custom GRF parameters for flipped models
#' results_custom <- margot_flip_forests(
#'   results,
#'   flip_outcomes = c("anxiety", "depression"),
#'   grf_defaults = list(num.trees = 4000, honesty = TRUE, honesty.fraction = 0.5)
#' )
#' }
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_h1
#' @export
margot_flip_forests <- function(model_results,
                                flip_outcomes,
                                model_prefix = "model_",
                                grf_defaults = NULL,
                                parallel = FALSE,
                                n_cores = future::availableCores() - 1,
                                verbose = TRUE,
                                remove_original = TRUE,
                                qini_treatment_cost = 1,
                                train_proportion = NULL,
                                use_train_test_split = NULL,
                                seed = 12345) {
  # validate inputs
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be a list containing a 'results' element (output from margot_causal_forest)")
  }
  if (!is.character(flip_outcomes) || length(flip_outcomes) == 0) {
    warning("flip_outcomes is empty or not a character vector, no flipping will be performed")
    return(model_results)
  }

  # check if original results were computed with marginal only
  if (!is.null(model_results$computation_status) &&
    !model_results$computation_status$heterogeneity_computed) {
    stop(
      "Cannot flip forests when original results were computed with compute_marginal_only = TRUE.\n",
      "Rerun margot_causal_forest() with compute_marginal_only = FALSE and appropriate train_proportion ",
      "to enable forest flipping."
    )
  }

  # check for required components
  has_data <- !is.null(model_results$data)
  has_covariates <- !is.null(model_results$covariates)
  has_W <- !is.null(model_results$W)

  if (!has_data || !has_covariates || !has_W) {
    stop("model_results must contain 'data', 'covariates', and 'W' components for full recomputation")
  }

  # ensure outcomes have model prefix
  flip_outcomes_prefixed <- ifelse(
    grepl(paste0("^", model_prefix), flip_outcomes),
    flip_outcomes,
    paste0(model_prefix, flip_outcomes)
  )

  # filter to existing models
  existing_models <- intersect(flip_outcomes_prefixed, names(model_results$results))

  if (length(existing_models) == 0) {
    warning("No matching models found for the specified outcomes")
    return(model_results)
  }

  if (verbose) {
    cli::cli_h1("Flipping Treatment Effects via Full Recomputation")
    cli::cli_alert_info("Processing {length(existing_models)} model{?s}")
  }

  # create a copy to avoid modifying the original
  results_copy <- model_results

  # extract components we'll need for all models
  covariates <- model_results$covariates
  W <- model_results$W
  weights <- model_results$weights %||% NULL

  # prepare outcome list for margot_causal_forest
  outcome_list <- list()
  outcome_names <- character()

  for (model_name in existing_models) {
    # extract outcome name without prefix
    outcome_name <- gsub(paste0("^", model_prefix), "", model_name)

    # find the outcome data
    outcome_data <- NULL
    if (outcome_name %in% names(model_results$data)) {
      outcome_data <- model_results$data[[outcome_name]]
    } else if (model_name %in% names(model_results$data)) {
      outcome_data <- model_results$data[[model_name]]
    } else {
      # try variations
      possible_keys <- c(
        paste0("t0_", outcome_name),
        paste0("t1_", outcome_name),
        paste0("t2_", outcome_name),
        gsub("_z$", "", outcome_name)
      )
      for (key in possible_keys) {
        if (key %in% names(model_results$data)) {
          outcome_data <- model_results$data[[key]]
          break
        }
      }
    }

    if (is.null(outcome_data)) {
      if (verbose) cli::cli_alert_warning("Cannot find outcome data for {model_name}, skipping")
      next
    }

    # flip the outcome
    flipped_outcome_name <- paste0(outcome_name, "_r")
    outcome_list[[flipped_outcome_name]] <- -outcome_data
    outcome_names <- c(outcome_names, flipped_outcome_name)

    if (verbose) cli::cli_alert_info("Prepared flipped outcome: {outcome_name} → {flipped_outcome_name}")
  }

  if (length(outcome_list) == 0) {
    warning("No outcomes could be processed")
    return(model_results)
  }

  # create data frame with flipped outcomes
  flipped_data <- as.data.frame(outcome_list)

  # extract grf parameters if not provided
  if (is.null(grf_defaults)) {
    # try to extract from first fitted forest model
    first_model_name <- existing_models[1]
    if (!is.null(model_results$full_models) &&
      first_model_name %in% names(model_results$full_models) &&
      !is.null(model_results$full_models[[first_model_name]])) {
      forest_obj <- model_results$full_models[[first_model_name]]

      # extract key parameters from the fitted forest
      extracted_params <- list()

      # check for common grf parameters
      if (!is.null(forest_obj$num.trees)) extracted_params$num.trees <- forest_obj$num.trees
      if (!is.null(forest_obj$honesty)) extracted_params$honesty <- forest_obj$honesty
      if (!is.null(forest_obj$honesty.fraction)) extracted_params$honesty.fraction <- forest_obj$honesty.fraction
      if (!is.null(forest_obj$alpha)) extracted_params$alpha <- forest_obj$alpha
      if (!is.null(forest_obj$min.node.size)) extracted_params$min.node.size <- forest_obj$min.node.size
      if (!is.null(forest_obj$mtry)) extracted_params$mtry <- forest_obj$mtry
      if (!is.null(forest_obj$imbalance.penalty)) extracted_params$imbalance.penalty <- forest_obj$imbalance.penalty
      if (!is.null(forest_obj$stabilize.splits)) extracted_params$stabilize.splits <- forest_obj$stabilize.splits
      if (!is.null(forest_obj$ci.group.size)) extracted_params$ci.group.size <- forest_obj$ci.group.size
      if (!is.null(forest_obj$tune.parameters)) extracted_params$tune.parameters <- forest_obj$tune.parameters

      if (length(extracted_params) > 0) {
        grf_defaults <- extracted_params
        if (verbose) {
          cli::cli_alert_info("Extracted GRF parameters from original model: {paste(names(grf_defaults), collapse = ', ')}")
        }
      } else {
        grf_defaults <- list()
        if (verbose) {
          cli::cli_alert_info("Could not extract GRF parameters, using defaults")
        }
      }
    } else {
      # check if saved in model results (future-proofing)
      first_model <- model_results$results[[existing_models[1]]]
      if (!is.null(first_model$grf_params)) {
        grf_defaults <- first_model$grf_params
        if (verbose) {
          cli::cli_alert_info("Using saved GRF parameters from model results")
        }
      } else {
        grf_defaults <- list()
        if (verbose) {
          cli::cli_alert_info("No GRF parameters found, using defaults")
        }
      }
    }
  } else {
    if (verbose) {
      cli::cli_alert_info("Using provided GRF parameters: {paste(names(grf_defaults), collapse = ', ')}")
    }
  }

  # determine other parameters
  first_model <- model_results$results[[existing_models[1]]]
  save_data <- !is.null(model_results$data)
  save_models <- !is.null(model_results$full_models)

  # additional check for required heterogeneity components
  if (is.null(first_model$dr_scores) || is.null(first_model$top_vars)) {
    stop(
      "Cannot flip forests: required heterogeneity components (dr_scores, top_vars) not found.\n",
      "This typically occurs when compute_marginal_only = TRUE was used.\n",
      "Rerun margot_causal_forest() with compute_marginal_only = FALSE."
    )
  }

  # check if computation_params exists (new structure)
  if (!is.null(model_results$computation_params)) {
    # inherit from saved computation parameters
    compute_marginal_only <- model_results$computation_params$compute_marginal_only %||% FALSE
    compute_rate <- model_results$computation_params$compute_rate %||% TRUE
    # qini_split parameter removed - always use honest evaluation
    qini_treatment_cost <- model_results$computation_params$qini_treatment_cost %||% qini_treatment_cost
    top_n_vars <- model_results$computation_params$top_n_vars %||% 15
    if (verbose) cli::cli_alert_info("Inheriting computation parameters from original results")
  } else {
    # fallback to old detection method for backward compatibility
    # if compute_heterogeneity exists, invert it; otherwise check if heterogeneity was computed
    if (!is.null(model_results$computation_params$compute_heterogeneity)) {
      compute_marginal_only <- !model_results$computation_params$compute_heterogeneity
    } else {
      compute_marginal_only <- FALSE # assume full analysis for backward compatibility
    }
    compute_rate <- !is.null(first_model$rate_result)
    # qini_split parameter removed - always use honest evaluation
    top_n_vars <- length(first_model$top_vars) %||% 15
  }

  # extract train_proportion from original results if not provided
  if (is.null(train_proportion)) {
    # try to get from split_info
    if (!is.null(first_model$split_info$train_proportion)) {
      train_proportion <- first_model$split_info$train_proportion
    } else {
      train_proportion <- 0.5 # default matching margot_causal_forest
    }
  }

  # extract use_train_test_split if not provided
  if (is.null(use_train_test_split)) {
    # check if original used train/test split
    if (!is.null(first_model$split_info$use_train_test_split)) {
      use_train_test_split <- first_model$split_info$use_train_test_split
    } else {
      use_train_test_split <- TRUE # default matching margot_causal_forest
    }
  }

  compute_conditional_means <- !is.null(first_model$conditional_means)

  # check if we should use parallel processing
  if (parallel && length(outcome_names) > 1) {
    if (verbose) cli::cli_alert_info("Using parallel processing with {n_cores} cores")

    # call parallel version
    flipped_results <- margot_causal_forest_parallel(
      data = flipped_data,
      outcome_vars = outcome_names,
      covariates = covariates,
      W = W,
      weights = weights,
      grf_defaults = grf_defaults,
      save_data = save_data,
      compute_rate = compute_rate,
      top_n_vars = top_n_vars,
      save_models = save_models,
      train_proportion = train_proportion,
      compute_conditional_means = compute_conditional_means,
      compute_marginal_only = compute_marginal_only,
      n_cores = n_cores,
      verbose = verbose,
      qini_treatment_cost = qini_treatment_cost,
      use_train_test_split = use_train_test_split,
      seed = seed
    )
  } else {
    # call sequential version
    flipped_results <- margot_causal_forest(
      data = flipped_data,
      outcome_vars = outcome_names,
      covariates = covariates,
      W = W,
      weights = weights,
      grf_defaults = grf_defaults,
      save_data = save_data,
      compute_rate = compute_rate,
      top_n_vars = top_n_vars,
      save_models = save_models,
      train_proportion = train_proportion,
      compute_conditional_means = compute_conditional_means,
      compute_marginal_only = compute_marginal_only,
      verbose = verbose,
      qini_treatment_cost = qini_treatment_cost,
      use_train_test_split = use_train_test_split,
      seed = seed
    )
  }

  # merge flipped results into the original results
  if (!is.null(flipped_results$results)) {
    for (flipped_model_name in names(flipped_results$results)) {
      # margot_causal_forest already adds "model_" prefix, so use as-is
      results_copy$results[[flipped_model_name]] <-
        flipped_results$results[[flipped_model_name]]
    }

    # also merge full_models if present
    if (!is.null(flipped_results$full_models)) {
      for (flipped_model_name in names(flipped_results$full_models)) {
        # margot_causal_forest already adds "model_" prefix, so use as-is
        results_copy$full_models[[flipped_model_name]] <-
          flipped_results$full_models[[flipped_model_name]]
      }
    }

    # also merge flipped data if present
    if (!is.null(flipped_results$data) && !is.null(results_copy$data)) {
      for (outcome_name in names(flipped_results$data)) {
        results_copy$data[[outcome_name]] <- flipped_results$data[[outcome_name]]
      }
    }

    # remove original models if requested
    if (remove_original) {
      # remove from results
      for (model_name in existing_models) {
        results_copy$results[[model_name]] <- NULL
        if (verbose) {
          cli::cli_alert_info("Removed original model: {gsub(model_prefix, '', model_name)}")
        }
      }

      # remove from full_models if present
      if (!is.null(results_copy$full_models)) {
        for (model_name in existing_models) {
          results_copy$full_models[[model_name]] <- NULL
        }
      }

      # remove from data if present
      if (!is.null(results_copy$data)) {
        for (model_name in existing_models) {
          # remove data with various possible keys
          outcome_name <- gsub(paste0("^", model_prefix), "", model_name)
          results_copy$data[[outcome_name]] <- NULL
          results_copy$data[[model_name]] <- NULL
        }
      }
    }

    # rebuild combined table from all results (original + flipped or just flipped)
    if (!is.null(results_copy$results) && length(results_copy$results) > 0) {
      all_tables <- lapply(results_copy$results, function(res) {
        if (!is.null(res$custom_table)) {
          return(res$custom_table)
        }
        NULL
      })

      # filter out NULLs
      all_tables <- all_tables[!sapply(all_tables, is.null)]

      if (length(all_tables) > 0) {
        results_copy$combined_table <- do.call(rbind, all_tables)
        rownames(results_copy$combined_table) <- gsub("^model_", "", names(results_copy$results)[!sapply(all_tables, is.null)])
      }
    }

    if (verbose) {
      cli::cli_alert_success("Flipping complete for {length(flipped_results$results)} model{?s}")
    }
  }

  return(results_copy)
}


# helper: trim cf_out down to essentials for each model
#' @keywords internal
margot_lighten_for_flip <- function(cf_out, models) {
  list(
    results     = cf_out$results[models],
    full_models = cf_out$full_models[models]
  )
}

# internal: flip one model (runs on each worker)
#' @keywords internal
.margot_flip_model <- function(model_name,
                               results_list,
                               full_models_list,
                               verbose = TRUE) {
  mdl <- results_list[[model_name]]
  if (!is.list(mdl)) {
    if (verbose) cli::cli_alert_warning("skipping {model_name} – invalid result")
    return(NULL)
  }

  # 1. flip tau_hat ---------------------------------------------------------
  if (!is.null(mdl$tau_hat)) {
    mdl$tau_hat_original <- mdl$tau_hat
    mdl$tau_hat <- -mdl$tau_hat
  }

  # 2. recalc RATE / QINI ---------------------------------------------------
  fm <- full_models_list[[model_name]]
  if (!is.null(fm)) {
    th <- mdl$tau_hat

    if (!is.null(mdl$rate_result)) {
      mdl$rate_result_original <- mdl$rate_result
      mdl$rate_result <- grf::rank_average_treatment_effect(fm, th)
      attr(mdl$rate_result, "target") <- "AUTOC"
    }
    if (!is.null(mdl$rate_qini)) {
      mdl$rate_qini_original <- mdl$rate_qini
      mdl$rate_qini <- grf::rank_average_treatment_effect(fm, th, target = "QINI")
      attr(mdl$rate_qini, "target") <- "QINI"
    }
  }

  # 3. mark qini curve for later regen -------------------------------------
  if (!is.null(mdl$qini_data)) {
    mdl$qini_data_original <- mdl$qini_data
    mdl$qini_needs_recalculation <- TRUE
  }

  # 4. flip dr scores + flag policy trees ----------------------------------
  if (is.matrix(mdl$dr_scores)) {
    mdl$dr_scores_original <- mdl$dr_scores
    mdl$dr_scores_flipped <- -mdl$dr_scores
    mdl$policy_trees_need_recalculation <- TRUE
  }

  mdl
}

# internal helper – compute policy trees for one model ---------------------
#' @keywords internal
.margot_fit_policy_trees <- function(model_name,
                                     mr,
                                     covariates,
                                     not_missing,
                                     full,
                                     verbose = TRUE,
                                     seed = 12345) {
  if (!isTRUE(mr$policy_trees_need_recalculation)) {
    return(mr)
  }

  if (!is.null(seed)) set.seed(seed + as.integer(as.factor(model_name)))
  if (verbose) cli::cli_alert_info("recalculating policy trees for {model_name}")

  # depth‑1 using top vars (not all predictors) ---------------------------
  mr$policy_tree_depth_1 <- policytree::policy_tree(
    covariates[full, mr$top_vars, drop = FALSE],
    mr$dr_scores_flipped[full, ],
    depth = 1
  )

  # depth‑2 on top vars -----------------------------------------------------
  # check if original model used train/test split
  if (!is.null(mr$split_info) && !is.null(mr$split_info$train_indices)) {
    # use the same train/test split as the original model
    train_idx <- mr$split_info$train_indices
    test_idx <- mr$split_info$test_indices
    if (verbose) cli::cli_alert_info("using original train/test split for policy trees")
  } else {
    # create new split (matching original behavior when no split info)
    train_proportion <- mr$split_info$train_proportion %||% 0.5
    train_size <- floor(train_proportion * length(not_missing))
    train_idx <- sample(not_missing, train_size)
    test_idx <- setdiff(not_missing, train_idx)
    if (verbose) cli::cli_alert_info("creating new train/test split for policy trees")
  }

  mr$policy_tree_depth_2 <- policytree::policy_tree(
    covariates[train_idx, mr$top_vars, drop = FALSE],
    mr$dr_scores_flipped[train_idx, ],
    depth = 2
  )

  mr$plot_data <- list(
    X_test       = covariates[test_idx, mr$top_vars, drop = FALSE],
    X_test_full  = covariates[test_idx, , drop = FALSE],
    predictions  = predict(mr$policy_tree_depth_2, covariates[test_idx, mr$top_vars, drop = FALSE])
  )

  mr$policy_trees_need_recalculation <- FALSE
  mr
}
