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


#' Flip CATE Estimates and Recalculate Policy Trees for Selected Outcomes
#'
#' @description
#' This function post-processes the results from margot_causal_forest to flip CATE estimates,
#' RATE results, QINI RATE results, QINI curves, and (by default) recalculates the policy trees
#' for the specified outcomes. If \code{recalc_policy = FALSE}, tree regeneration is skipped.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param flip_outcomes A character vector of outcome variable names for which CATE estimates should be flipped.
#' @param model_prefix A character string indicating the prefix used for model names in the results list. Default is "model_".
#' @param recalc_policy Logical; if TRUE (default) recalculates policy trees for flipped outcomes.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A modified copy of the model_results list with flipped CATE estimates and recalculated policy trees.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom grf rank_average_treatment_effect
#' @export
margot_flip_forests <- function(model_results,
                                flip_outcomes,
                                model_prefix   = "model_",
                                recalc_policy  = TRUE,
                                verbose        = TRUE) {
  # validate inputs
  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be a list containing a 'results' element (output from margot_causal_forest)")
  }
  if (!is.character(flip_outcomes) || length(flip_outcomes) == 0) {
    warning("flip_outcomes is empty or not a character vector, no flipping will be performed")
    return(model_results)
  }

  # copy results to avoid side effects
  results_copy <- model_results

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
    cli::cli_alert_info(paste("flipping cate estimates for", length(models_to_flip),
                              "outcomes:", paste(gsub(model_prefix, "", models_to_flip), collapse = ", ")))
  }

  # process each model
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
      model_result$tau_hat          <- -model_result$tau_hat
      if (verbose) cli::cli_alert_info(paste("flipped tau_hat for", model_name))
    }

    # 2. recalc RATE and QINI RATE
    if (!is.null(results_copy$full_models) && model_name %in% names(results_copy$full_models)) {
      model_obj       <- results_copy$full_models[[model_name]]
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
      model_result$qini_data_original      <- model_result$qini_data
      model_result$qini_needs_recalculation <- TRUE
      if (verbose) cli::cli_alert_info(paste("marked QINI for recalculation for", model_name))
    }

    # 4. flip double-robust scores and flag trees
    if (!is.null(model_result$dr_scores)) {
      dr_scores <- model_result$dr_scores
      tryCatch({
        if (is.matrix(dr_scores)) {
          model_result$dr_scores_original        <- dr_scores
          model_result$dr_scores_flipped         <- -dr_scores
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
  results_copy$flip_outcomes               <- gsub(model_prefix, "", models_to_flip)
  results_copy$flip_outcomes_postprocessed <- TRUE
  if (verbose) cli::cli_alert_success("finished flipping specified outcomes")

  # optionally recalc policy trees
  if (recalc_policy && length(models_to_flip)) {
    plain_names <- gsub(model_prefix, "", models_to_flip)
    results_copy <- margot_recalculate_policy_trees(
      model_results           = results_copy,
      outcomes_to_recalculate = plain_names,
      model_prefix            = model_prefix,
      verbose                 = verbose
    )
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
#' @export
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
      model_results, plain, model_prefix, verbose,
      parallel   = parallel_policy,
      n_cores_policy = n_policy_cores,
      seed       = if (isTRUE(seed)) NULL else seed
    )
  }

  model_results
}


#' #' recalculate policy trees with flipped double-robust scores
#' #'
#' #' @param model_results list returned by `margot_flip_forests()`
#' #' @param outcomes_to_recalculate character vector of outcome names
#' #' @param model_prefix prefix used for model names inside `results`
#' #' @param verbose logical; print progress messages?
#' #'
#' #' @return updated `model_results` with fresh depth-1/2 trees and
#' #'         a plot_data list that now contains both X_test *and*
#' #'         X_test_full.
#' #' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' #' @importFrom policytree policy_tree
#' #' @keywords internal
#' margot_recalculate_policy_trees <- function(model_results,
#'                                             outcomes_to_recalculate = NULL,
#'                                             model_prefix           = "model_",
#'                                             verbose                = TRUE) {
#'
#'   if (!is.list(model_results) || !"results" %in% names(model_results))
#'     stop("model_results must be a list containing a 'results' element")
#'
#'   results_copy <- model_results
#'
#'   # ── which outcomes need work? ────────────────────────────────────────────
#'   if (is.null(outcomes_to_recalculate)) {
#'     outcomes_to_recalculate <- names(results_copy$results)[
#'       vapply(results_copy$results, function(x)
#'         isTRUE(x$policy_trees_need_recalculation), logical(1))
#'     ]
#'   } else {
#'     outcomes_to_recalculate <- paste0(
#'       ifelse(grepl(paste0("^", model_prefix), outcomes_to_recalculate), "",
#'              model_prefix),
#'       outcomes_to_recalculate
#'     )
#'   }
#'   if (!length(outcomes_to_recalculate)) {
#'     if (verbose) cli::cli_alert_info("no outcomes need policy tree recalculation")
#'     return(results_copy)
#'   }
#'   if (verbose)
#'     cli::cli_alert_info(
#'       "recalculating policy trees for {length(outcomes_to_recalculate)} outcomes: {paste(gsub(model_prefix, \"\", outcomes_to_recalculate), collapse = \", \")}"
#'     )
#'
#'   covariates  <- results_copy$covariates
#'   not_missing <- results_copy$not_missing %||% which(complete.cases(covariates))
#'   full        <- intersect(seq_len(nrow(covariates)), not_missing)
#'
#'   for (model_name in outcomes_to_recalculate) {
#'
#'     if (verbose) cli::cli_alert_info("recalculating policy trees for {model_name}")
#'     mr <- results_copy$results[[model_name]]
#'
#'     # sanity checks ---------------------------------------------------------
#'     if (is.null(mr$dr_scores_flipped) || is.null(mr$top_vars)) {
#'       if (verbose) cli::cli_alert_warning(
#'         "skipping {model_name} – missing flipped dr_scores or top_vars"
#'       )
#'       next
#'     }
#'
#'     # depth-1 tree on *all* covariates -------------------------------------
#'     mr$policy_tree_depth_1 <-
#'       policytree::policy_tree(covariates[full, , drop = FALSE],
#'                               mr$dr_scores_flipped[full, ], depth = 1)
#'
#'     # depth-2 tree on top_vars ---------------------------------------------
#'     train_size <- floor(0.7 * length(not_missing))
#'     train_idx  <- sample(not_missing, train_size)
#'
#'     mr$policy_tree_depth_2 <-
#'       policytree::policy_tree(covariates[train_idx, mr$top_vars, drop = FALSE],
#'                               mr$dr_scores_flipped[train_idx, ], depth = 2)
#'
#'     # test subset for plotting ---------------------------------------------
#'     test_idx   <- setdiff(not_missing, train_idx)
#'
#'     X_test      <- covariates[test_idx, mr$top_vars, drop = FALSE]
#'     X_test_full <- covariates[test_idx, ,            drop = FALSE]  # ← NEW
#'
#'     mr$plot_data <- list(
#'       X_test       = X_test,
#'       X_test_full  = X_test_full,                                   # ← NEW
#'       predictions  = predict(mr$policy_tree_depth_2, X_test)
#'     )
#'
#'     # clear flag and save back ---------------------------------------------
#'     mr$policy_trees_need_recalculation <- FALSE
#'     results_copy$results[[model_name]] <- mr
#'   }
#'
#'   if (verbose) cli::cli_alert_success("finished recalculating policy trees")
#'   results_copy
#' }
#' #' margot_flip_forests_parallel <- function(model_results,
#'                                          flip_outcomes,
#'                                          model_prefix   = "model_",
#'                                          recalc_policy  = TRUE,
#'                                          verbose        = TRUE,
#'                                          n_cores        = future::availableCores() - 1) {
#'   # validate inputs
#'   if (!is.list(model_results) || !("results" %in% names(model_results))) {
#'     stop("model_results must be a list containing a 'results' element (output from margot_causal_forest)")
#'   }
#'   if (!is.character(flip_outcomes) || length(flip_outcomes) == 0) {
#'     warning("flip_outcomes is empty or not a character vector, no flipping will be performed")
#'     return(model_results)
#'   }
#'
#'   # copy to avoid side-effects
#'   results_copy <- model_results
#'
#'   # identify models to flip
#'   all_models <- names(results_copy$results)
#'   models_to_flip <- paste0(
#'     ifelse(grepl(paste0("^", model_prefix), flip_outcomes), "", model_prefix),
#'     flip_outcomes
#'   )
#'   models_to_flip <- intersect(models_to_flip, all_models)
#'   if (length(models_to_flip) == 0) {
#'     warning("none of the specified outcomes match any models in the results")
#'     return(model_results)
#'   }
#'
#'   if (verbose) {
#'     cli::cli_alert_info(
#'       paste("flipping cate estimates for", length(models_to_flip),
#'             "outcomes:", paste(gsub(model_prefix, "", models_to_flip), collapse = ", "))
#'     )
#'   }
#'
#'   # plan futures
#'   future::plan(future::multisession, workers = n_cores)
#'   if (verbose) cli::cli_alert_info(paste0("using ", n_cores, " workers"))
#'
#'   # process flips in parallel
#'   flipped_list <- future.apply::future_lapply(models_to_flip, function(model_name) {
#'     if (verbose) cli::cli_alert_info(paste0("processing ", model_name))
#'     model_result <- results_copy$results[[model_name]]
#'     if (!is.list(model_result)) {
#'       if (verbose) cli::cli_alert_warning(paste0("skipping ", model_name, " - invalid result"))
#'       return(NULL)
#'     }
#'     # wrap operations in tryCatch to capture errors
#'     tryCatch({
#'       # 1. flip tau_hat
#'       if (!is.null(model_result$tau_hat)) {
#'         model_result$tau_hat_original <- model_result$tau_hat
#'         model_result$tau_hat          <- -model_result$tau_hat
#'       }
#'       # 2. recalc RATE and QINI
#'       if (!is.null(results_copy$full_models) && model_name %in% names(results_copy$full_models)) {
#'         model_obj       <- results_copy$full_models[[model_name]]
#'         flipped_tau_hat <- model_result$tau_hat
#'         if (!is.null(model_result$rate_result)) {
#'           model_result$rate_result_original <- model_result$rate_result
#'           model_result$rate_result          <- grf::rank_average_treatment_effect(model_obj, flipped_tau_hat)
#'         }
#'         if (!is.null(model_result$rate_qini)) {
#'           model_result$rate_qini_original <- model_result$rate_qini
#'           model_result$rate_qini          <- grf::rank_average_treatment_effect(model_obj,
#'                                                                                 flipped_tau_hat,
#'                                                                                 target = "QINI")
#'         }
#'       }
#'       # 3. mark QINI curves for recalculation
#'       if (!is.null(model_result$qini_data)) {
#'         model_result$qini_data_original      <- model_result$qini_data
#'         model_result$qini_needs_recalculation <- TRUE
#'       }
#'       # 4. flip DR scores and flag trees
#'       if (!is.null(model_result$dr_scores) && is.matrix(model_result$dr_scores)) {
#'         model_result$dr_scores_original         <- model_result$dr_scores
#'         model_result$dr_scores_flipped          <- -model_result$dr_scores
#'         model_result$policy_trees_need_recalculation <- TRUE
#'       }
#'       # return updated model_result
#'       model_result
#'     }, error = function(e) {
#'       cli::cli_alert_danger(paste0("error processing ", model_name, ": ", e$message))
#'       NULL
#'     })
#'   })
#'   names(flipped_list) <- models_to_flip
#'
#'   # update copy with flipped results
#'   for (nm in models_to_flip) {
#'     if (!is.null(flipped_list[[nm]])) {
#'       results_copy$results[[nm]] <- flipped_list[[nm]]
#'     }
#'   }
#'
#'   # record flipped outcomes
#'   results_copy$flip_outcomes               <- gsub(model_prefix, "", models_to_flip)
#'   results_copy$flip_outcomes_postprocessed <- TRUE
#'   if (verbose) cli::cli_alert_success("finished flipping specified outcomes")
#'
#'   # optionally recalc policy trees sequentially
#'   if (recalc_policy && length(models_to_flip)) {
#'     plain_names <- gsub(model_prefix, "", models_to_flip)
#'     results_copy <- margot_recalculate_policy_trees(
#'       model_results           = results_copy,
#'       outcomes_to_recalculate = plain_names,
#'       model_prefix            = model_prefix,
#'       verbose                 = verbose
#'     )
#'   }
#'
#'   results_copy
#' }

