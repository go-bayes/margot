#' Recompute QINI Curves Using AIPW Scores
#'
#' @description
#' Recomputes QINI curves for binary treatment causal forest models using
#' Augmented Inverse Probability Weighted (AIPW) scores instead of IPW scores.
#' This provides more robust estimates, especially for observational data where
#' treatment assignment may be confounded.
#'
#' @param margot_result A list returned by \code{margot_causal_forest()} with
#'   \code{save_models = TRUE} and \code{save_data = TRUE}.
#' @param model_name Character string specifying which model to recompute.
#'   If NULL (default), all models will be recomputed.
#' @param method Character string specifying the method to estimate mu.hat.
#'   Options are "regression_forest" (default) or "grf_scores".
#' @param n.trees Number of trees to use for regression forests when
#'   method = "regression_forest". Default is 2000.
#' @param treatment_var Character string specifying the treatment variable name.
#'   If NULL, the function will try to detect it automatically.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#'
#' @return A modified version of the input margot_result with updated QINI
#'   curves based on AIPW scores. The structure remains compatible with all
#'   existing plotting and interpretation functions.
#'
#' @details
#' AIPW scores provide double robustness: the estimator is consistent if either
#' the propensity score model OR the outcome model is correctly specified.
#' This is particularly valuable in observational studies.
#'
#' The function supports two methods for estimating conditional means (mu.hat):
#' \itemize{
#'   \item \strong{regression_forest}: Trains separate regression forests on
#'     control and treated units to estimate E[Y|W=0,X] and E[Y|W=1,X].
#'   \item \strong{grf_scores}: Uses the doubly robust scores from grf's
#'     internal computations (if available).
#' }
#'
#' @examples
#' \dontrun{
#' # Recompute QINI curves with AIPW for all models
#' results_aipw <- margot_recompute_qini_aipw(margot_results)
#'
#' # Recompute for a specific model
#' results_aipw <- margot_recompute_qini_aipw(
#'   margot_results,
#'   model_name = "model_anxiety"
#' )
#'
#' # Use with plotting functions as normal
#' margot_plot_qini(results_aipw, "model_anxiety")
#'
#' # Specify treatment variable if auto-detection fails
#' results_aipw <- margot_recompute_qini_aipw(
#'   margot_results,
#'   treatment_var = "t1_treatment"
#' )
#' }
#'
#' @importFrom grf regression_forest
#' @importFrom maq get_aipw_scores maq
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_h2
#' @importFrom purrr map2_dfr
#'
#' @export
margot_recompute_qini_aipw <- function(margot_result,
                                       model_name = NULL,
                                       method = c("regression_forest", "grf_scores"),
                                       n.trees = 2000,
                                       treatment_var = NULL,
                                       verbose = TRUE) {
  # validate inputs
  method <- match.arg(method)

  if (is.null(margot_result$results)) {
    stop("margot_result must contain a 'results' component")
  }

  # determine which models to process
  if (is.null(model_name)) {
    model_names <- names(margot_result$results)
  } else {
    if (!model_name %in% names(margot_result$results)) {
      stop("Model '", model_name, "' not found in margot_result")
    }
    model_names <- model_name
  }

  # process each model
  for (model_name in model_names) {
    if (verbose) cli::cli_h2("Recomputing QINI with AIPW for {model_name}")

    model_obj <- margot_result$results[[model_name]]

    # check if we have the causal forest model
    # first try model in the results object
    cf_model <- NULL
    if (!is.null(model_obj$model)) {
      cf_model <- model_obj$model
    } else if (!is.null(margot_result$full_models) && !is.null(margot_result$full_models[[model_name]])) {
      # try full_models list
      cf_model <- margot_result$full_models[[model_name]]
    }

    if (is.null(cf_model)) {
      cli::cli_alert_warning("Model object not found for {model_name}. Skipping.")
      next
    }

    # extract tau_hat from model or results
    tau_hat <- NULL
    if (!is.null(cf_model$predictions)) {
      tau_hat <- cf_model$predictions
    } else if (!is.null(model_obj$tau_hat)) {
      tau_hat <- model_obj$tau_hat
    }

    if (is.null(tau_hat)) {
      cli::cli_alert_warning("Treatment effect predictions not found for {model_name}. Skipping.")
      next
    }

    # extract treatment and outcome
    W <- cf_model$W.orig
    Y <- cf_model$Y.orig
    X <- cf_model$X.orig

    # if original data not in model, try to reconstruct from saved data
    if (is.null(W) || is.null(Y) || is.null(X)) {
      if (!is.null(margot_result$data) && !is.null(margot_result$covariates)) {
        if (verbose) cli::cli_alert_info("Using saved data from margot_result")

        # get the outcome variable name (remove "model_" prefix)
        outcome_name <- sub("^model_", "", model_name)

        # extract outcome
        if (outcome_name %in% names(margot_result$data)) {
          Y <- margot_result$data[[outcome_name]]
        } else {
          cli::cli_alert_warning("Outcome {outcome_name} not found in saved data. Skipping.")
          next
        }

        # extract treatment
        if (!is.null(treatment_var)) {
          # use specified treatment variable
          if (treatment_var %in% names(margot_result$data)) {
            W <- margot_result$data[[treatment_var]]
          } else {
            cli::cli_alert_warning("Treatment variable {treatment_var} not found in data. Skipping.")
            next
          }
        } else {
          # try to auto-detect treatment variable
          # typically it's a t0 or t1 variable not in covariates and not the outcome
          W_cols <- grep("^t[0-9]+_", names(margot_result$data), value = TRUE)
          W_cols <- setdiff(W_cols, names(margot_result$covariates))
          W_cols <- setdiff(W_cols, outcome_name)

          if (length(W_cols) == 1) {
            W <- margot_result$data[[W_cols[1]]]
            if (verbose) cli::cli_alert_info("Auto-detected treatment variable: {W_cols[1]}")
          } else if (length(W_cols) > 1) {
            # if multiple candidates, try to pick one that's binary
            binary_cols <- sapply(W_cols, function(col) {
              length(unique(margot_result$data[[col]])) == 2
            })
            if (sum(binary_cols) == 1) {
              W_col <- W_cols[binary_cols]
              W <- margot_result$data[[W_col]]
              if (verbose) cli::cli_alert_info("Auto-detected binary treatment variable: {W_col}")
            } else {
              cli::cli_alert_warning("Multiple treatment candidates found. Please specify treatment_var. Candidates: {paste(W_cols, collapse=', ')}")
              next
            }
          } else {
            cli::cli_alert_warning("Cannot identify treatment variable. Please specify treatment_var.")
            next
          }
        }

        # extract covariates
        X <- as.matrix(margot_result$covariates)

        # use only complete cases
        if (!is.null(margot_result$not_missing)) {
          complete_idx <- margot_result$not_missing
          W <- W[complete_idx]
          Y <- Y[complete_idx]
          X <- X[complete_idx, , drop = FALSE]
          tau_hat <- tau_hat[complete_idx]
        }
      } else {
        cli::cli_alert_warning("Cannot extract data for {model_name}. Skipping.")
        next
      }
    }

    # get propensity scores
    W.hat <- cf_model$W.hat
    if (is.null(W.hat)) {
      # estimate propensity scores if not available
      if (verbose) cli::cli_alert_info("Estimating propensity scores")
      prop_forest <- grf::regression_forest(X, W, num.trees = 2000)
      W.hat <- predict(prop_forest)$predictions
      W.hat <- pmax(0.01, pmin(W.hat, 0.99)) # trim extreme values
    }

    # compute mu.hat based on method
    if (method == "regression_forest") {
      if (verbose) cli::cli_alert_info("Estimating conditional means using regression forests")

      # estimate mu0 = E[Y|W=0,X]
      control_idx <- which(W == 0)
      if (length(control_idx) > 0) {
        rf_control <- grf::regression_forest(
          X[control_idx, , drop = FALSE],
          Y[control_idx],
          num.trees = n.trees
        )
        mu0 <- predict(rf_control, X)$predictions
      } else {
        cli::cli_alert_warning("No control units found. Using mean outcome for mu0.")
        mu0 <- rep(mean(Y), length(Y))
      }

      # estimate mu1 = E[Y|W=1,X]
      treated_idx <- which(W == 1)
      if (length(treated_idx) > 0) {
        rf_treated <- grf::regression_forest(
          X[treated_idx, , drop = FALSE],
          Y[treated_idx],
          num.trees = n.trees
        )
        mu1 <- predict(rf_treated, X)$predictions
      } else {
        cli::cli_alert_warning("No treated units found. Using mean outcome for mu1.")
        mu1 <- rep(mean(Y), length(Y))
      }

      mu.hat <- cbind(mu0 = mu0, mu1 = mu1)
    } else if (method == "grf_scores") {
      # try to use grf's internal scores
      cli::cli_alert_info("Attempting to use GRF internal scores")

      # this method would require access to grf's internal computations
      # for now, fall back to regression forest method
      cli::cli_alert_warning("GRF scores method not yet implemented. Using regression forest method.")

      # recursively call with regression forest method
      return(margot_recompute_qini_aipw(
        margot_result,
        model_name = model_name,
        method = "regression_forest",
        n.trees = n.trees,
        verbose = verbose
      ))
    }

    # compute AIPW scores
    if (verbose) cli::cli_alert_info("Computing AIPW scores")

    aipw_scores <- maq::get_aipw_scores(
      Y = Y,
      W = as.factor(W),
      W.hat = cbind(1 - W.hat, W.hat), # convert to matrix format
      mu.hat = mu.hat
    )

    # recompute QINI curves with AIPW scores
    if (verbose) cli::cli_alert_info("Recomputing QINI curves")

    # cate-based targeting
    cate_qini <- maq::maq(
      reward = as.matrix(tau_hat),
      cost = matrix(1, length(tau_hat), 1),
      DR.scores = aipw_scores,
      R = 200,
      seed = 42
    )

    # ate-based targeting (uniform treatment)
    ate_qini <- maq::maq(
      reward = matrix(rep(mean(tau_hat), length(tau_hat)), ncol = 1),
      cost = matrix(1, length(tau_hat), 1),
      DR.scores = aipw_scores,
      R = 200,
      seed = 42
    )

    # update qini objects
    qini_objects <- list(cate = cate_qini, ate = ate_qini)

    # extract qini data for plotting
    max_idx <- max(sapply(qini_objects, function(q) length(q[["_path"]]$gain)))
    if (max_idx == 0) {
      cli::cli_alert_warning("QINI computation failed for {model_name}")
      next
    }

    # reuse existing extraction function
    qini_data <- purrr::map2_dfr(
      qini_objects,
      names(qini_objects),
      ~ extract_qini_data_binary(.x, .y, max_idx, verbose = FALSE)
    )

    # update the model results
    margot_result$results[[model_name]]$qini_objects <- qini_objects
    margot_result$results[[model_name]]$qini_data <- qini_data

    # add metadata about AIPW
    margot_result$results[[model_name]]$qini_method <- "AIPW"
    margot_result$results[[model_name]]$qini_aipw_details <- list(
      method = method,
      n.trees = n.trees,
      timestamp = Sys.time()
    )

    if (verbose) cli::cli_alert_success("QINI curves recomputed with AIPW for {model_name}")
  }

  # add overall metadata
  margot_result$qini_aipw_recomputed <- TRUE
  margot_result$qini_aipw_timestamp <- Sys.time()

  return(margot_result)
}
