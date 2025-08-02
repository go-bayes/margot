#' Compute QINI Curves Following GRF/MAQ Methodology
#'
#' @description
#' Computes QINI curves for causal forest models using the exact methodology
#' from GRF/MAQ packages. Uses test set data when train/test splits were used
#' in margot_causal_forest().
#'
#' @param margot_result Output from margot_causal_forest()
#' @param model_names Character vector of model names to process. If NULL (default),
#'   processes all models.
#' @param seed Integer. Random seed for reproducibility (default 12345)
#' @param n_bootstrap Integer. Number of bootstrap replicates for confidence
#'   intervals (default 200)
#' @param verbose Logical. Print progress messages (default TRUE)
#'
#' @return The input margot_result object with added qini_results component containing:
#'   \item{cate}{MAQ object for CATE-based targeting}
#'   \item{baseline}{MAQ object for no-priority baseline}
#'   \item{test_indices}{Indices used for QINI computation}
#'   \item{n_test}{Number of observations used}
#'
#' @details
#' This function implements the standard QINI curve methodology from the GRF
#' and MAQ packages. It computes two curves:
#'
#' 1. CATE curve: Prioritizes treatment based on estimated individual treatment effects
#' 2. Baseline curve: No-priority assignment (random allocation)
#'
#' When margot_causal_forest() was run with use_train_test_split = TRUE,
#' QINI curves are computed on the test set only, following best practices
#' for avoiding overfitting in policy evaluation.
#'
#' @examples
#' \dontrun{
#' # Run causal forest with train/test split
#' cf_results <- margot_causal_forest(
#'   data = mydata,
#'   outcome_vars = c("outcome1", "outcome2"),
#'   covariates = covariates,
#'   W = treatment,
#'   use_train_test_split = TRUE
#' )
#'
#' # Compute QINI curves
#' cf_results <- margot_qini(cf_results)
#'
#' # Plot results
#' margot_plot_qini(cf_results, model_name = "model_outcome1")
#' }
#'
#' @export
#' @importFrom maq maq get_ipw_scores
#' @importFrom cli cli_h2 cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom grf causal_forest
margot_qini <- function(margot_result,
                        model_names = NULL,
                        seed = 12345,
                        n_bootstrap = 200,
                        verbose = TRUE,
                        spend_levels = NULL, # for compatibility, not used
                        label_mapping = NULL) { # for compatibility, not used

  # validate inputs
  if (!is.list(margot_result) || !("results" %in% names(margot_result))) {
    stop("margot_result must be output from margot_causal_forest()")
  }

  # check required components
  if (is.null(margot_result$full_models)) {
    stop("margot_qini() requires margot_causal_forest() to be run with save_models = TRUE")
  }

  if (is.null(margot_result$data)) {
    stop("margot_qini() requires margot_causal_forest() to be run with save_data = TRUE")
  }

  # determine which models to process
  if (is.null(model_names)) {
    model_names <- names(margot_result$results)
  } else {
    # validate model names exist
    invalid_models <- setdiff(model_names, names(margot_result$results))
    if (length(invalid_models) > 0) {
      stop("Models not found: ", paste(invalid_models, collapse = ", "))
    }
  }


  # process each model
  for (model_name in model_names) {
    if (verbose) {
      cli::cli_h2("Computing QINI curves for {model_name}")
    }

    # get model components
    model_obj <- margot_result$results[[model_name]]

    # get the causal forest model from full_models
    cf_model <- margot_result$full_models[[model_name]]

    if (is.null(cf_model)) {
      cli::cli_alert_warning("Model not found in full_models for {model_name}. Skipping.")
      cli::cli_alert_info("Available models: {paste(names(margot_result$full_models), collapse = ', ')}")
      next
    }

    # extract data
    Y <- cf_model$Y.orig
    W <- cf_model$W.orig
    X <- cf_model$X.orig

    # if original data not in model, try to reconstruct from saved data
    if (is.null(Y) || is.null(W) || is.null(X)) {
      if (!is.null(margot_result$data)) {
        if (verbose) {
          cli::cli_alert_info("Extracting data from saved margot_result")
        }

        # get outcome name
        outcome_name <- sub("^model_", "", model_name)

        if (outcome_name %in% names(margot_result$data)) {
          Y <- margot_result$data[[outcome_name]]
        } else {
          cli::cli_alert_warning("Outcome {outcome_name} not found. Skipping.")
          next
        }

        # get treatment - should be stored in margot_result
        W <- margot_result$W
        if (is.null(W)) {
          cli::cli_alert_warning("Treatment variable not found. Skipping.")
          next
        }

        # get covariates
        X <- as.matrix(margot_result$covariates)

        # handle missing data
        if (!is.null(margot_result$not_missing)) {
          complete_idx <- margot_result$not_missing
          Y <- Y[complete_idx]
          W <- W[complete_idx]
          X <- X[complete_idx, , drop = FALSE]
        }
      } else {
        cli::cli_alert_warning("Cannot extract data for {model_name}. Skipping.")
        next
      }
    }

    # determine indices to use for QINI computation
    test_indices <- NULL

    # check for train/test split information
    if (!is.null(model_obj$split_info) &&
      !is.null(model_obj$split_info$use_train_test_split) &&
      model_obj$split_info$use_train_test_split) {
      # use test indices from split_info
      test_indices <- model_obj$split_info$test_indices
      if (verbose) {
        cli::cli_alert_info("Using test set from split_info (n = {length(test_indices)})")
      }
    } else if (!is.null(model_obj$qini_metadata) &&
      !is.null(model_obj$qini_metadata$test_indices)) {
      # alternative: check qini_metadata
      test_indices <- model_obj$qini_metadata$test_indices
      if (verbose) {
        cli::cli_alert_info("Using test set from qini_metadata (n = {length(test_indices)})")
      }
    } else {
      # use all data
      test_indices <- seq_len(length(Y))
      if (verbose) {
        cli::cli_alert_info("Using all data (n = {length(test_indices)})")
      }
    }

    # subset data to test indices
    Y_test <- Y[test_indices]
    W_test <- W[test_indices]
    X_test <- X[test_indices, , drop = FALSE]

    # get tau_hat for test set
    # if we have the model, predict on test set
    # otherwise use stored tau_hat
    if (!is.null(cf_model) && !is.null(X_test)) {
      if (verbose) {
        cli::cli_alert_info("Predicting treatment effects on test set")
      }
      tau_hat_test <- predict(cf_model, X_test)$predictions
    } else if (!is.null(model_obj$tau_hat)) {
      # use stored tau_hat
      tau_hat_test <- model_obj$tau_hat[test_indices]
    } else {
      cli::cli_alert_warning("Cannot obtain treatment effect predictions. Skipping.")
      next
    }

    # compute IPW scores
    if (verbose) {
      cli::cli_alert_info("Computing IPW scores")
    }

    # ensure Y is matrix format and W is factor
    Y_mat <- as.matrix(Y_test)
    W_factor <- as.factor(W_test)

    # compute IPW scores using maq
    set.seed(seed)
    IPW_scores <- maq::get_ipw_scores(Y_mat, W_factor)

    # compute QINI curves
    if (verbose) {
      cli::cli_alert_info("Computing CATE-based QINI curve")
    }

    # cate-based targeting
    qini_cate <- maq::maq(
      reward = as.matrix(tau_hat_test),
      cost = matrix(1, length(tau_hat_test), 1),
      DR.scores = IPW_scores,
      R = n_bootstrap,
      seed = seed
    )

    if (verbose) {
      cli::cli_alert_info("Computing no-priority baseline")
    }

    # no-priority baseline (random assignment)
    qini_baseline <- maq::maq(
      reward = as.matrix(tau_hat_test),
      cost = matrix(1, length(tau_hat_test), 1),
      DR.scores = IPW_scores,
      target.with.covariates = FALSE,
      R = n_bootstrap,
      seed = seed
    )

    # update existing qini components in results
    margot_result$results[[model_name]]$qini_objects <- list(
      cate = qini_cate,
      ate = qini_baseline # keep "ate" name for backward compatibility
    )

    # extract and update qini_data
    # create data frame from maq objects
    # handle potentially different path lengths
    cate_spend <- qini_cate[["_path"]]$spend
    cate_gain <- qini_cate[["_path"]]$gain
    baseline_spend <- qini_baseline[["_path"]]$spend
    baseline_gain <- qini_baseline[["_path"]]$gain

    # create separate data frames and combine
    cate_df <- data.frame(
      proportion = cate_spend,
      gain = cate_gain,
      curve = "cate"
    )

    baseline_df <- data.frame(
      proportion = baseline_spend,
      gain = baseline_gain,
      curve = "ate"
    )

    margot_result$results[[model_name]]$qini_data <- rbind(cate_df, baseline_df)

    # update qini_metadata
    margot_result$results[[model_name]]$qini_metadata <- list(
      n_test = length(test_indices),
      test_indices = test_indices,
      baseline_method = "grf_standard",
      timestamp = Sys.time()
    )

    if (verbose) {
      cli::cli_alert_success("QINI curves computed for {model_name}")
    }
  }

  # add metadata
  margot_result$qini_computed <- TRUE
  margot_result$qini_timestamp <- Sys.time()
  margot_result$qini_method <- "grf_standard"

  return(margot_result)
}
