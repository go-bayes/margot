#' Compute QINI Curves (Alternative Implementation)
#'
#' @description
#' Computes QINI curves using existing data in margot_causal_forest results.
#' This version works with the standard output structure without requiring
#' save_models = TRUE.
#'
#' @param margot_result Output from margot_causal_forest()
#' @param model_names Character vector of model names to process (NULL = all)
#' @param seed Random seed for reproducibility
#' @param n_bootstrap Number of bootstrap replicates for confidence intervals
#' @param verbose Print progress messages
#' @param spend_levels Numeric vector of spend levels for diff_gain_summaries (default 0.1)
#' @param label_mapping Ignored (for compatibility)
#'
#' @export
margot_qini_alternative <- function(margot_result,
                                    model_names = NULL,
                                    seed = 12345,
                                    n_bootstrap = 200,
                                    verbose = TRUE,
                                    spend_levels = c(0.1, 0.4),
                                    label_mapping = NULL) {
  # validate inputs
  if (!is.list(margot_result) || !("results" %in% names(margot_result))) {
    stop("margot_result must be output from margot_causal_forest()")
  }

  # check for saved data
  if (is.null(margot_result$data)) {
    stop("margot_qini() requires margot_causal_forest() to be run with save_data = TRUE")
  }

  # determine which models to process
  if (is.null(model_names)) {
    model_names <- names(margot_result$results)
  }

  # process each model
  for (model_name in model_names) {
    if (verbose) {
      cli::cli_h2("Computing QINI curves for {model_name}")
    }

    # get model components
    model_obj <- margot_result$results[[model_name]]

    # check if tau_hat exists
    if (is.null(model_obj$tau_hat)) {
      cli::cli_alert_warning("No tau_hat found for {model_name}. Skipping.")
      next
    }

    # get outcome name
    outcome_name <- sub("^model_", "", model_name)

    # extract Y and W
    if (outcome_name %in% names(margot_result$data)) {
      Y <- margot_result$data[[outcome_name]]
    } else {
      cli::cli_alert_warning("Outcome {outcome_name} not found in data. Skipping.")
      next
    }

    W <- margot_result$W
    if (is.null(W)) {
      cli::cli_alert_warning("Treatment variable W not found. Skipping.")
      next
    }

    # handle missing data
    if (!is.null(margot_result$not_missing)) {
      complete_idx <- margot_result$not_missing
      Y <- Y[complete_idx]
      W <- W[complete_idx]
    }

    # determine indices to use
    test_indices <- NULL

    # check for train/test split
    if (!is.null(model_obj$split_info) &&
      !is.null(model_obj$split_info$use_train_test_split) &&
      model_obj$split_info$use_train_test_split) {
      test_indices <- model_obj$split_info$test_indices
      if (verbose) {
        cli::cli_alert_info("Using test set from split_info (n = {length(test_indices)})")
      }
    } else {
      # use all data
      test_indices <- seq_len(length(Y))
      if (verbose) {
        cli::cli_alert_info("Using all data (n = {length(test_indices)})")
      }
    }

    # subset to test indices
    Y_test <- Y[test_indices]
    W_test <- W[test_indices]
    tau_hat_test <- model_obj$tau_hat[test_indices]

    # fit evaluation forest on test data to get consistent DR scores
    # this ensures DR scores are computed from the same forest used for ATE
    if (verbose) {
      cli::cli_alert_info("Fitting evaluation forest on test data for consistent DR scores")
    }

    set.seed(seed)

    # get weights for test data
    weights_test <- if (!is.null(margot_result$weights)) {
      margot_result$weights[test_indices]
    } else {
      NULL
    }

    # fit evaluation forest
    eval_forest <- grf::causal_forest(
      X = margot_result$covariates[test_indices, ],
      Y = Y_test,
      W = W_test,
      sample.weights = weights_test,
      num.trees = 2000 # match default from margot_causal_forest
    )

    # get DR scores from evaluation forest
    DR_scores <- grf::get_scores(eval_forest, drop = TRUE)

    # also compute and store ATE from evaluation forest for consistency
    ate_eval <- grf::average_treatment_effect(eval_forest)
    margot_result$results[[model_name]]$ate_evaluation_forest <- round(ate_eval, 3)

    if (verbose) {
      cli::cli_alert_info("ATE from evaluation forest: {ate_eval['estimate']}")
      # add diagnostic info
      cli::cli_alert_info("DR scores shape: {nrow(DR_scores)} x {ncol(DR_scores)}")
      cli::cli_alert_info("tau_hat range: [{round(min(tau_hat_test), 3)}, {round(max(tau_hat_test), 3)}]")
    }

    # compute QINI curves
    if (verbose) {
      cli::cli_alert_info("Computing CATE-based QINI curve")
    }

    # cate-based targeting
    qini_cate <- maq::maq(
      reward = as.matrix(tau_hat_test),
      cost = matrix(1, length(tau_hat_test), 1),
      DR.scores = DR_scores,
      R = n_bootstrap,
      seed = seed
    )

    if (verbose) {
      cli::cli_alert_info("Computing no-priority baseline")
    }

    # no-priority baseline
    qini_baseline <- tryCatch(
      {
        maq::maq(
          reward = as.matrix(tau_hat_test),
          cost = matrix(1, length(tau_hat_test), 1),
          DR.scores = DR_scores,
          target.with.covariates = FALSE,
          R = n_bootstrap,
          seed = seed
        )
      },
      error = function(e) {
        cli::cli_alert_warning("Failed to compute baseline QINI: {e$message}")
        NULL
      }
    )

    # check if baseline computation failed
    if (is.null(qini_baseline)) {
      cli::cli_alert_warning("Skipping {model_name} due to baseline computation failure")
      next
    }

    # update existing qini components
    margot_result$results[[model_name]]$qini_objects <- list(
      cate = qini_cate,
      ate = qini_baseline
    )

    # create qini_data with error handling for empty paths
    cate_path <- qini_cate[["_path"]]
    baseline_path <- qini_baseline[["_path"]]

    # check if paths exist and have data
    if (is.null(cate_path) || length(cate_path$spend) == 0) {
      cli::cli_alert_warning("CATE path is empty for {model_name}")
      next
    }

    if (is.null(baseline_path) || length(baseline_path$spend) == 0) {
      cli::cli_alert_warning("Baseline path is empty for {model_name}")
      # create minimal baseline with just endpoints
      baseline_df <- data.frame(
        proportion = c(0, 1),
        gain = c(0, 0),
        curve = "ate"
      )
    } else {
      baseline_df <- data.frame(
        proportion = baseline_path$spend,
        gain = baseline_path$gain,
        curve = "ate"
      )
    }

    cate_df <- data.frame(
      proportion = cate_path$spend,
      gain = cate_path$gain,
      curve = "cate"
    )

    margot_result$results[[model_name]]$qini_data <- rbind(cate_df, baseline_df)

    # update qini_metadata
    margot_result$results[[model_name]]$qini_metadata <- list(
      n_test = length(test_indices),
      test_indices = test_indices,
      baseline_method = "grf_standard",
      timestamp = Sys.time()
    )

    # compute diff_gain_summaries
    if (verbose) {
      cli::cli_alert_info("Computing diff_gain_summaries at spend levels: {paste(spend_levels, collapse=', ')}")
    }

    diff_gain_summaries <- list()

    # create temporary mc_result structure for margot_summary_cate_difference_gain
    temp_mc_result <- list(
      results = setNames(list(margot_result$results[[model_name]]), model_name)
    )

    for (spend in spend_levels) {
      if (verbose) {
        cli::cli_alert_info("Computing difference gain at spend={spend}")
      }

      tryCatch(
        {
          diff_gain_summaries[[paste0("spend_", spend)]] <- margot_summary_cate_difference_gain(
            mc_result = temp_mc_result,
            outcome_var = model_name,
            reference_curve = "ate",
            comparison_curve = "cate",
            spend = spend
          )
        },
        error = function(e) {
          cli::cli_alert_warning("Failed to compute diff_gain_summary at spend={spend}: {e$message}")
        }
      )
    }

    # store diff_gain_summaries
    margot_result$results[[model_name]]$diff_gain_summaries <- diff_gain_summaries

    if (verbose) {
      cli::cli_alert_success("QINI curves and diff_gain_summaries computed for {model_name}")
    }
  }

  # add metadata
  margot_result$qini_computed <- TRUE
  margot_result$qini_timestamp <- Sys.time()
  margot_result$qini_method <- "grf_standard"

  return(margot_result)
}
