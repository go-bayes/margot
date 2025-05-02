#' Flip CATE Estimates and Recalculate Policy Trees for Selected Outcomes
#'
#' @description
#' This function post-processes the results from margot_causal_forest to flip CATE estimates,
#' RATE results, QINI RATE results, QINI curves, and (by default) recalculates the policy trees
#' for the specified outcomes.  If `recalc_policy = FALSE`, tree regeneration is skipped.
#'
#' @param model_results A list containing the model results from margot_causal_forest().
#' @param flip_outcomes A character vector of outcome variable names for which CATE estimates should be flipped.
#' @param model_prefix A character string indicating the prefix used for model names in the results list. Default is "model_.".
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
      model_result$tau_hat         <- -model_result$tau_hat
      if (verbose) cli::cli_alert_info(paste("flipped tau_hat for", model_name))
    }

    # 2. recalc RATE and QINI RATE
    if (!is.null(results_copy$full_models) && model_name %in% names(results_copy$full_models)) {
      model_obj        <- results_copy$full_models[[model_name]]
      flipped_tau_hat  <- model_result$tau_hat

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
          model_result$dr_scores_original   <- dr_scores
          model_result$dr_scores_flipped    <- -dr_scores
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
  results_copy$flip_outcomes              <- gsub(model_prefix, "", models_to_flip)
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


#' Recalculate Policy Trees with Flipped Double Robust Scores
#'
#' @description
#' This function recalculates policy trees using flipped double robust scores for outcomes
#' that were processed by margot_flip_forests().
#'
#' @param model_results A list containing the model results after running margot_flip_forests().
#' @param outcomes_to_recalculate A character vector of outcome names to recalculate policy trees for.
#'                               If NULL, recalculates all models marked as needing recalculation.
#' @param model_prefix A character string indicating the prefix used for model names. Default is "model_.".
#' @param verbose Logical indicating whether to display detailed messages. Default is TRUE.
#'
#' @return An updated copy of model_results with recalculated policy trees.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom policytree policy_tree
#' @keywords internal
margot_recalculate_policy_trees <- function(model_results,
                                            outcomes_to_recalculate = NULL,
                                            model_prefix           = "model_",
                                            verbose                = TRUE) {

  if (!is.list(model_results) || !("results" %in% names(model_results))) {
    stop("model_results must be a list containing a 'results' element")
  }
  results_copy <- model_results

  # select outcomes needing recalculation
  if (is.null(outcomes_to_recalculate)) {
    outcomes_to_recalculate <- names(results_copy$results)
    outcomes_to_recalculate <- outcomes_to_recalculate[
      vapply(results_copy$results, function(x) {
        !is.null(x$policy_trees_need_recalculation) && x$policy_trees_need_recalculation
      }, logical(1))]
  } else {
    outcomes_to_recalculate <- paste0(
      ifelse(grepl(paste0("^", model_prefix), outcomes_to_recalculate), "", model_prefix),
      outcomes_to_recalculate)
  }
  if (!length(outcomes_to_recalculate)) {
    if (verbose) cli::cli_alert_info("no outcomes need policy tree recalculation")
    return(results_copy)
  }
  if (verbose) {
    cli::cli_alert_info(paste("recalculating policy trees for", length(outcomes_to_recalculate),
                              "outcomes:", paste(gsub(model_prefix, "", outcomes_to_recalculate), collapse = ", ")))
  }

  covariates <- model_results$covariates
  not_missing <- model_results$not_missing
  if (is.null(not_missing))
    not_missing <- which(complete.cases(covariates))
  full <- intersect(seq_len(nrow(covariates)), not_missing)

  for (model_name in outcomes_to_recalculate) {
    if (verbose) cli::cli_alert_info(paste("recalculating policy trees for", model_name))
    mr <- results_copy$results[[model_name]]
    if (is.null(mr$dr_scores_flipped) || is.null(mr$top_vars)) {
      if (verbose) cli::cli_alert_warning(paste("skipping", model_name, "- missing flipped dr_scores or top_vars"))
      next
    }
    # depth 1
    try({
      mr$policy_tree_depth_1 <-
        policytree::policy_tree(covariates[full, ], mr$dr_scores_flipped[full, ], depth = 1)
      if (verbose) cli::cli_alert_info(paste("recalculated policy_tree_depth_1 for", model_name))
    }, silent = TRUE)
    # depth 2
    train_size <- floor(0.7 * length(not_missing))
    train_idxs <- sample(not_missing, train_size)
    try({
      mr$policy_tree_depth_2 <-
        policytree::policy_tree(covariates[train_idxs, mr$top_vars], mr$dr_scores_flipped[train_idxs, ], depth = 2)
      if (verbose) cli::cli_alert_info(paste("recalculated policy_tree_depth_2 for", model_name))
      test_idxs <- setdiff(not_missing, train_idxs)
      mr$plot_data <- list(
        X_test      = covariates[test_idxs, mr$top_vars],
        predictions = predict(mr$policy_tree_depth_2, covariates[test_idxs, mr$top_vars])
      )
    }, silent = TRUE)

    # clear flag
    mr$policy_trees_need_recalculation <- FALSE
    results_copy$results[[model_name]] <- mr
  }

  if (verbose) cli::cli_alert_success("finished recalculating policy trees")
  return(results_copy)
}
