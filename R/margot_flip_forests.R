# updated: recalc policy trees with optional parallelisation ---------------
#' @keywords internal
margot_recalculate_policy_trees <- function(model_results,
                                            outcomes_to_recalculate = NULL,
                                            model_prefix           = "model_",
                                            verbose                = TRUE,
                                            parallel               = FALSE,
                                            n_cores_policy         = future::availableCores() - 1,
                                            seed                   = NULL) {
  if (!is.list(model_results) || !"results" %in% names(model_results))
    stop("model_results must be a list containing a 'results' element")

  results_copy <- model_results

  # which outcomes ----------------------------------------------------------
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

  covariates  <- results_copy$covariates
  not_missing <- results_copy$not_missing %||% which(complete.cases(covariates))
  full        <- intersect(seq_len(nrow(covariates)), not_missing)

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
#' Creates new models with reversed treatment effects, appending "_r" to model names.
#' The original models are removed and replaced with properly flipped versions where
#' all statistics (ATE, RATE, E-values, conditional means) are recomputed.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param flip_outcomes A character vector of outcome variable names for which CATE estimates should be flipped.
#' @param model_prefix A character string indicating the prefix used for model names in the results list. Default is "model_".
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A modified copy of model_results with flipped models (with "_r" suffix) replacing originals.
#'
#' @details
#' This function creates entirely new model entries with "_r" suffix rather than modifying
#' existing models. All statistics are properly recomputed for the flipped scenario:
#' - ATE is negated
#' - CATE estimates (tau_hat) are negated
#' - E-value table is recomputed with the flipped ATE
#' - Conditional means columns are swapped (for binary treatment)
#' - RATE and QINI are recomputed with flipped tau_hat
#' - Policy trees are marked for recalculation
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom grf rank_average_treatment_effect average_treatment_effect
#' @export
margot_flip_forests <- function(model_results,
                                flip_outcomes,
                                model_prefix   = "model_",
                                verbose        = TRUE) {
  # validate inputs
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be a list containing a 'results' element (output from margot_causal_forest)")
  }
  if (!is.character(flip_outcomes) || length(flip_outcomes) == 0) {
    warning("flip_outcomes is empty or not a character vector, no flipping will be performed")
    return(model_results)
  }
  
  # check for required components
  if (is.null(model_results$full_models)) {
    stop("full_models not found in model_results. ensure save_models = TRUE in margot_causal_forest")
  }
  
  # check for required data for policy tree and qini recalculation
  need_recalc <- !is.null(model_results$covariates) || !is.null(model_results$data)
  if (!need_recalc && verbose) {
    cli::cli_alert_warning("covariates and/or data not found - policy trees and QINI will not be recalculated")
  }

  # copy results to avoid side effects
  results_copy <- model_results
  
  # get covariates and not_missing indices if available
  covariates <- results_copy$covariates
  not_missing <- results_copy$not_missing
  if (is.null(not_missing) && !is.null(covariates)) {
    not_missing <- which(complete.cases(covariates))
  }

  # identify models to flip
  all_models <- names(results_copy$results)
  models_to_flip <- paste0(ifelse(grepl(paste0("^", model_prefix), flip_outcomes), "", model_prefix),
                           flip_outcomes)
  models_to_flip <- models_to_flip[models_to_flip %in% all_models]
  if (length(models_to_flip) == 0) {
    warning("none of the specified outcomes match any models in the results")
    return(model_results)
  }

  if (verbose) {
    cli::cli_alert_info(paste("creating flipped models for", length(models_to_flip),
                              "outcomes:", paste(gsub(model_prefix, "", models_to_flip), collapse = ", ")))
  }

  # process each model to create new "_r" version
  new_models <- list()
  new_full_models <- list()
  models_to_remove <- c()
  
  for (model_name in models_to_flip) {
    if (verbose) cli::cli_alert_info(paste("creating flipped version of", model_name))
    
    original_result <- results_copy$results[[model_name]]
    if (is.null(original_result) || !is.list(original_result)) {
      if (verbose) cli::cli_alert_warning(paste("skipping", model_name, "- invalid or missing results"))
      next
    }
    
    # get the grf model object
    grf_model <- results_copy$full_models[[model_name]]
    if (is.null(grf_model)) {
      if (verbose) cli::cli_alert_warning(paste("skipping", model_name, "- no model object found"))
      next
    }
    
    # create new model name with _r suffix
    outcome_name <- gsub(model_prefix, "", model_name)
    flipped_model_name <- paste0(model_prefix, outcome_name, "_r")
    
    # create new result object for flipped model
    flipped_result <- list()
    
    # 1. flip and recompute ATE
    if (!is.null(original_result$ate)) {
      flipped_result$ate <- -original_result$ate
    } else {
      # compute if missing
      tryCatch({
        flipped_result$ate <- -round(grf::average_treatment_effect(grf_model), 3)
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning("could not compute ATE for {model_name}: {e$message}")
      })
    }
    
    # 2. recompute custom table (E-values) with flipped ATE
    tryCatch({
      # we need to create a new E-value table for the flipped outcome
      flipped_result$custom_table <- margot::margot_model_evalue(
        grf_model, 
        scale = "RD", 
        new_name = paste0(outcome_name, "_r"), 
        subset = NULL
      )
      # manually flip the effect estimate in the table
      if ("E[Y(1)]-E[Y(0)]" %in% colnames(flipped_result$custom_table)) {
        flipped_result$custom_table[, "E[Y(1)]-E[Y(0)]"] <- -flipped_result$custom_table[, "E[Y(1)]-E[Y(0)]"]
      }
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("could not recompute E-values for {model_name}: {e$message}")
      flipped_result$custom_table <- original_result$custom_table
    })
    
    # 3. flip tau_hat
    if (!is.null(original_result$tau_hat)) {
      flipped_result$tau_hat <- -original_result$tau_hat
    } else {
      flipped_result$tau_hat <- -predict(grf_model)$predictions
    }
    
    # 4. flip conditional means (swap columns for binary treatment)
    if (!is.null(original_result$conditional_means)) {
      cm <- original_result$conditional_means
      if (ncol(cm) == 2) {
        flipped_result$conditional_means <- cm[, c(2, 1), drop = FALSE]
        colnames(flipped_result$conditional_means) <- colnames(cm)
      } else {
        if (verbose) cli::cli_alert_warning("conditional means has {ncol(cm)} columns, expected 2 for binary treatment")
        flipped_result$conditional_means <- cm
      }
    }
    
    # 5. recompute RATE and QINI RATE with flipped tau
    if (!is.null(original_result$rate_result)) {
      tryCatch({
        flipped_result$rate_result <- grf::rank_average_treatment_effect(grf_model, flipped_result$tau_hat)
        if (verbose) cli::cli_alert_info("recomputed RATE for {flipped_model_name}")
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning("error computing RATE for {flipped_model_name}: {e$message}")
      })
    }
    
    if (!is.null(original_result$rate_qini)) {
      tryCatch({
        flipped_result$rate_qini <- grf::rank_average_treatment_effect(grf_model, 
                                                                       flipped_result$tau_hat, 
                                                                       target = "QINI")
        if (verbose) cli::cli_alert_info("recomputed QINI RATE for {flipped_model_name}")
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning("error computing QINI RATE for {flipped_model_name}: {e$message}")
      })
    }
    
    # 6. flip DR scores for policy tree recalculation
    if (!is.null(original_result$dr_scores)) {
      flipped_result$dr_scores <- -original_result$dr_scores
      flipped_result$dr_scores_flipped <- flipped_result$dr_scores  # for compatibility
    }
    
    # 7. recalculate policy trees with flipped DR scores
    if (!is.null(flipped_result$dr_scores) && !is.null(covariates)) {
      tryCatch({
        # recalculate depth-1 policy tree
        flipped_result$policy_tree_depth_1 <- policytree::policy_tree(
          covariates[not_missing, , drop = FALSE],
          flipped_result$dr_scores[not_missing, ],
          depth = 1
        )
        
        # recalculate depth-2 policy tree
        if (!is.null(original_result$top_vars)) {
          train_size <- floor(0.7 * length(not_missing))
          train_idx <- sample(not_missing, train_size)
          
          flipped_result$policy_tree_depth_2 <- policytree::policy_tree(
            covariates[train_idx, original_result$top_vars, drop = FALSE],
            flipped_result$dr_scores[train_idx, ],
            depth = 2
          )
          
          # update plot_data with new predictions
          test_idx <- setdiff(not_missing, train_idx)
          flipped_result$plot_data <- list(
            X_test = covariates[test_idx, original_result$top_vars, drop = FALSE],
            X_test_full = covariates[test_idx, , drop = FALSE],
            predictions = predict(flipped_result$policy_tree_depth_2, 
                                covariates[test_idx, original_result$top_vars, drop = FALSE])
          )
        }
        
        if (verbose) cli::cli_alert_info("recalculated policy trees for {flipped_model_name}")
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning("error recalculating policy trees for {flipped_model_name}: {e$message}")
        flipped_result$policy_trees_need_recalculation <- TRUE
      })
    }
    
    # 8. recalculate QINI with flipped tau_hat
    if (!is.null(original_result$qini_data)) {
      tryCatch({
        # get treatment assignment
        W <- results_copy$W
        
        # try to get outcome data - check multiple possible locations
        outcome_data <- NULL
        
        # try with outcome name (no prefix)
        if (!is.null(results_copy$data[[outcome_name]])) {
          outcome_data <- results_copy$data[[outcome_name]]
          if (verbose) cli::cli_alert_info("found outcome data using key: {outcome_name}")
        }
        # try with full model name
        else if (!is.null(results_copy$data[[model_name]])) {
          outcome_data <- results_copy$data[[model_name]]
          if (verbose) cli::cli_alert_info("found outcome data using key: {model_name}")
        }
        # try checking what keys are available
        else if (!is.null(results_copy$data)) {
          available_keys <- names(results_copy$data)
          if (verbose) cli::cli_alert_info("available data keys: {paste(available_keys, collapse = ', ')}")
          
          # try to find a matching key
          possible_keys <- c(outcome_name, model_name, 
                           gsub("_r$", "", outcome_name),  # try without _r suffix
                           paste0("t2_", outcome_name),     # try with t2_ prefix
                           paste0("t2_", gsub("_r$", "", outcome_name)))
          
          for (key in possible_keys) {
            if (key %in% available_keys) {
              outcome_data <- results_copy$data[[key]]
              if (verbose) cli::cli_alert_info("found outcome data using key: {key}")
              break
            }
          }
        }
        
        if (!is.null(outcome_data) && !is.null(W)) {
          # recalculate qini curves with flipped tau
          qini_result <- compute_qini_curves_binary(
            flipped_result$tau_hat, 
            as.matrix(outcome_data), 
            W, 
            verbose = FALSE
          )
          if (!is.null(qini_result)) {
            flipped_result$qini_data <- qini_result$qini_data
            flipped_result$qini_objects <- qini_result$qini_objects
            if (verbose) cli::cli_alert_info("recalculated QINI curves for {flipped_model_name}")
          }
        } else {
          # fallback: copy original QINI data (it's better than NULL)
          if (verbose) cli::cli_alert_warning("cannot recalculate QINI for {flipped_model_name}: outcome data or W not found")
          if (verbose) cli::cli_alert_info("copying original QINI data as fallback")
          flipped_result$qini_data <- original_result$qini_data
          flipped_result$qini_objects <- original_result$qini_objects
          flipped_result$qini_needs_recalculation <- TRUE
        }
      }, error = function(e) {
        if (verbose) cli::cli_alert_warning("error recalculating QINI for {flipped_model_name}: {e$message}")
        # fallback: copy original QINI data
        flipped_result$qini_data <- original_result$qini_data
        flipped_result$qini_objects <- original_result$qini_objects
        flipped_result$qini_needs_recalculation <- TRUE
      })
    }
    
    # 9. copy other necessary fields
    flipped_result$test_calibration <- original_result$test_calibration
    flipped_result$top_vars <- original_result$top_vars
    flipped_result$blp_top <- original_result$blp_top  # BLP may need recalculation in future
    
    # store the new model
    new_models[[flipped_model_name]] <- flipped_result
    new_full_models[[flipped_model_name]] <- grf_model
    models_to_remove <- c(models_to_remove, model_name)
    
    if (verbose) cli::cli_alert_success("created flipped model {flipped_model_name}")
  }
  
  # add new models and remove originals
  for (nm in names(new_models)) {
    results_copy$results[[nm]] <- new_models[[nm]]
    results_copy$full_models[[nm]] <- new_full_models[[nm]]
  }
  
  for (nm in models_to_remove) {
    results_copy$results[[nm]] <- NULL
    results_copy$full_models[[nm]] <- NULL
  }
  
  # update outcome_vars to reflect new names
  if (!is.null(results_copy$outcome_vars)) {
    flipped_names <- gsub(model_prefix, "", models_to_remove)
    results_copy$outcome_vars <- setdiff(results_copy$outcome_vars, flipped_names)
    results_copy$outcome_vars <- c(results_copy$outcome_vars, paste0(flipped_names, "_r"))
  }
  
  # rebuild combined_table
  if (!is.null(results_copy$combined_table)) {
    all_tables <- lapply(results_copy$results, function(x) x$custom_table)
    all_tables <- all_tables[!sapply(all_tables, is.null)]
    if (length(all_tables) > 0) {
      results_copy$combined_table <- do.call(rbind, all_tables)
      rownames(results_copy$combined_table) <- gsub(model_prefix, "", names(all_tables))
    }
  }
  
  if (verbose) cli::cli_alert_success("finished creating flipped models with _r suffix")
  
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
    mdl$tau_hat          <- -mdl$tau_hat
  }

  # 2. recalc RATE / QINI ---------------------------------------------------
  fm <- full_models_list[[model_name]]
  if (!is.null(fm)) {
    th <- mdl$tau_hat

    if (!is.null(mdl$rate_result)) {
      mdl$rate_result_original <- mdl$rate_result
      mdl$rate_result          <- grf::rank_average_treatment_effect(fm, th)
    }
    if (!is.null(mdl$rate_qini)) {
      mdl$rate_qini_original <- mdl$rate_qini
      mdl$rate_qini          <- grf::rank_average_treatment_effect(fm, th, target = "QINI")
    }
  }

  # 3. mark qini curve for later regen -------------------------------------
  if (!is.null(mdl$qini_data)) {
    mdl$qini_data_original       <- mdl$qini_data
    mdl$qini_needs_recalculation <- TRUE
  }

  # 4. flip dr scores + flag policy trees ----------------------------------
  if (is.matrix(mdl$dr_scores)) {
    mdl$dr_scores_original            <- mdl$dr_scores
    mdl$dr_scores_flipped             <- -mdl$dr_scores
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
                                     seed = NULL) {
  if (!isTRUE(mr$policy_trees_need_recalculation))
    return(mr)

  if (!is.null(seed)) set.seed(seed + as.integer(as.factor(model_name)))
  if (verbose) cli::cli_alert_info("recalculating policy trees for {model_name}")

  # depth‑1 using all predictors -------------------------------------------
  mr$policy_tree_depth_1 <- policytree::policy_tree(
    covariates[full, , drop = FALSE],
    mr$dr_scores_flipped[full, ],
    depth = 1
  )

  # depth‑2 on top vars -----------------------------------------------------
  train_size <- floor(0.7 * length(not_missing))
  train_idx  <- sample(not_missing, train_size)

  mr$policy_tree_depth_2 <- policytree::policy_tree(
    covariates[train_idx, mr$top_vars, drop = FALSE],
    mr$dr_scores_flipped[train_idx, ],
    depth = 2
  )

  test_idx   <- setdiff(not_missing, train_idx)
  mr$plot_data <- list(
    X_test       = covariates[test_idx, mr$top_vars, drop = FALSE],
    X_test_full  = covariates[test_idx, ,            drop = FALSE],
    predictions  = predict(mr$policy_tree_depth_2, covariates[test_idx, mr$top_vars, drop = FALSE])
  )

  mr$policy_trees_need_recalculation <- FALSE
  mr
}

#' Parallel flip of CATE estimates with reproducible RNG and memory guard
#'
#' @inheritParams margot_flip_forests
#' @param max_size_GB numeric. globals cap passed to `future.globals.maxSize` (default 2 GB).
#' @param seed `TRUE` for automatic parallel-safe RNG, or an integer for deterministic streams.
#' @param parallel_policy logical. if TRUE, policy-tree refits are parallelised.
#' @param n_policy_cores integer. number of workers for policy-tree refits.
#' @importFrom future plan multisession availableCores
#' @importFrom future.apply future_lapply
#' @name margot_flip_forests_parallel
#' @keywords internal
margot_flip_forests_parallel <- function(model_results,
                                         flip_outcomes,
                                         model_prefix   = "model_",
                                         recalc_policy  = TRUE,
                                         parallel_policy = FALSE,
                                         n_policy_cores  = future::availableCores() - 1,
                                         verbose        = TRUE,
                                         n_cores        = future::availableCores() - 1,
                                         max_size_GB    = 2,
                                         seed           = TRUE) {
  stopifnot(is.list(model_results), "results" %in% names(model_results))
  if (!is.character(flip_outcomes) || !length(flip_outcomes)) return(model_results)

  targets <- paste0(ifelse(grepl(paste0("^", model_prefix), flip_outcomes), "", model_prefix), flip_outcomes)
  targets <- intersect(targets, names(model_results$results))
  if (!length(targets)) return(model_results)

  if (verbose)
    cli::cli_alert_info("flipping cate estimates for {length(targets)} outcomes: {paste(gsub(model_prefix, '', targets), collapse = ', ')}")

  light <- margot_lighten_for_flip(model_results, targets)
  options(future.globals.maxSize = max_size_GB * 1024^3)

  future::plan(future::multisession, workers = n_cores)
  if (verbose) cli::cli_alert_info("using {n_cores} workers and a {max_size_GB} gb globals cap")

  flipped <- future.apply::future_lapply(
    targets, .margot_flip_model,
    results_list     = light$results,
    full_models_list = light$full_models,
    verbose          = verbose,
    future.seed      = seed
  )
  names(flipped) <- targets

  model_results$results[targets] <- flipped
  model_results$flip_outcomes               <- gsub(model_prefix, "", targets)
  model_results$flip_outcomes_postprocessed <- TRUE

  if (recalc_policy && length(targets)) {
    plain <- gsub(model_prefix, "", targets)
    model_results <- margot_recalculate_policy_trees(
      model_results = model_results,
      outcomes_to_recalculate = plain,
      model_prefix = model_prefix,
      verbose = verbose,
      parallel = parallel_policy,
      n_cores = n_policy_cores,
      seed = if (isTRUE(seed)) NULL else seed
    )
  }

  model_results
}
