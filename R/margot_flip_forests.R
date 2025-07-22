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
#' Creates new models with reversed treatment effects by flipping the outcome variable
#' and recomputing the entire causal forest from scratch. New models are appended with
#' "_r" suffix to indicate reduced/reversed outcomes.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param flip_outcomes A character vector of outcome variable names for which models should be flipped.
#' @param model_prefix A character string indicating the prefix used for model names in the results list. Default is "model_".
#' @param parallel Logical indicating whether to use parallel processing. Default is FALSE.
#' @param n_cores Number of cores to use for parallel processing. Default is availableCores() - 1.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A modified copy of model_results with flipped models (with "_r" suffix) added.
#'
#' @details
#' This function creates entirely new models by:
#' 1. Extracting the original data for each outcome
#' 2. Flipping the outcome variable (Y_flipped = -Y)
#' 3. Calling margot_causal_forest() with the flipped outcome
#' 4. Storing the result with "_r" suffix
#' 
#' This ensures all components (forest model, CATE estimates, QINI curves, policy trees,
#' E-values, etc.) are consistently computed based on the flipped outcome.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_h1
#' @export
margot_flip_forests <- function(model_results,
                                flip_outcomes,
                                model_prefix   = "model_",
                                parallel       = FALSE,
                                n_cores        = future::availableCores() - 1,
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
  
  # extract other parameters from original run if available
  first_model <- model_results$results[[existing_models[1]]]
  grf_defaults <- list()
  if (!is.null(first_model$grf_params)) {
    grf_defaults <- first_model$grf_params
  }
  
  # determine other parameters
  save_data <- !is.null(model_results$data)
  compute_rate <- !is.null(first_model$rate_result)
  save_models <- !is.null(model_results$full_models)
  top_n_vars <- length(first_model$top_vars) %||% 15
  train_proportion <- 0.7  # default
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
      n_cores = n_cores,
      verbose = verbose
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
      verbose = verbose
    )
  }
  
  # merge flipped results into the original results
  if (!is.null(flipped_results$results)) {
    for (flipped_model_name in names(flipped_results$results)) {
      results_copy$results[[paste0(model_prefix, flipped_model_name)]] <- 
        flipped_results$results[[flipped_model_name]]
    }
    
    # also merge full_models if present
    if (!is.null(flipped_results$full_models)) {
      for (flipped_model_name in names(flipped_results$full_models)) {
        results_copy$full_models[[paste0(model_prefix, flipped_model_name)]] <- 
          flipped_results$full_models[[flipped_model_name]]
      }
    }
    
    # rebuild combined table from all results (original + flipped)
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