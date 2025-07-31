#' Compute QINI Curves with Improved DR Scores (Internal)
#' 
#' @description
#' Internal function that computes QINI curves using evaluation forest for
#' consistent DR scores. This replaces compute_qini_curves_binary with the
#' improved GRF-standard approach.
#' 
#' @param Y Outcome vector for test set
#' @param W Treatment vector for test set
#' @param X Covariate matrix for test set
#' @param tau_hat CATE predictions for test set
#' @param weights Sample weights for test set (optional)
#' @param seed Random seed
#' @param n_bootstrap Number of bootstrap iterations
#' @param verbose Print progress messages
#' 
#' @return List with qini_data and qini_objects
#' @keywords internal
compute_qini_improved <- function(Y, W, X, tau_hat, weights = NULL, 
                                 seed = 12345, n_bootstrap = 200, 
                                 verbose = TRUE) {
  
  tryCatch({
    if (verbose) cli::cli_alert_info("Computing QINI curves with evaluation forest approach")
    
    # ensure proper formats
    Y <- as.vector(Y)
    W <- as.vector(W)
    tau_hat <- as.vector(tau_hat)
    
    # fit evaluation forest on test data for consistent DR scores
    if (verbose) cli::cli_alert_info("Fitting evaluation forest for DR scores")
    
    set.seed(seed)
    eval_forest <- grf::causal_forest(
      X = X,
      Y = Y,
      W = W,
      sample.weights = weights,
      num.trees = 2000
    )
    
    # get DR scores from evaluation forest
    DR_scores <- grf::get_scores(eval_forest, drop = TRUE)
    
    # compute ATE from evaluation forest for reference
    ate_eval <- grf::average_treatment_effect(eval_forest)
    
    if (verbose) {
      cli::cli_alert_info("Evaluation forest ATE: {round(ate_eval['estimate'], 4)}")
    }
    
    # compute CATE-based QINI
    if (verbose) cli::cli_alert_info("Computing CATE-based QINI curve")
    
    qini_cate <- maq::maq(
      reward = as.matrix(tau_hat),
      cost = matrix(1, length(tau_hat), 1),
      DR.scores = DR_scores,
      R = n_bootstrap,
      seed = seed
    )
    
    # compute no-priority baseline
    if (verbose) cli::cli_alert_info("Computing no-priority baseline")
    
    qini_baseline <- tryCatch({
      maq::maq(
        reward = as.matrix(tau_hat),
        cost = matrix(1, length(tau_hat), 1),
        DR.scores = DR_scores,
        target.with.covariates = FALSE,
        R = n_bootstrap,
        seed = seed
      )
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("Failed to compute baseline: {e$message}")
      NULL
    })
    
    if (is.null(qini_baseline)) {
      if (verbose) cli::cli_alert_warning("Creating fallback baseline")
      # create minimal baseline
      qini_baseline <- list(
        "_path" = list(
          spend = c(0, 1),
          gain = c(0, mean(tau_hat))
        )
      )
    }
    
    # extract paths
    cate_path <- qini_cate[["_path"]]
    baseline_path <- qini_baseline[["_path"]]
    
    # create qini_data
    if (!is.null(cate_path) && length(cate_path$spend) > 0) {
      cate_df <- data.frame(
        proportion = cate_path$spend,
        gain = cate_path$gain,
        curve = "cate"
      )
    } else {
      if (verbose) cli::cli_alert_warning("CATE path is empty")
      return(NULL)
    }
    
    if (!is.null(baseline_path) && length(baseline_path$spend) > 0) {
      baseline_df <- data.frame(
        proportion = baseline_path$spend,
        gain = baseline_path$gain,
        curve = "ate"
      )
    } else {
      # create minimal baseline
      baseline_df <- data.frame(
        proportion = c(0, 1),
        gain = c(0, mean(tau_hat)),
        curve = "ate"
      )
    }
    
    qini_data <- rbind(cate_df, baseline_df)
    
    # return results
    list(
      qini_data = qini_data,
      qini_objects = list(cate = qini_cate, ate = qini_baseline),
      ate_evaluation_forest = round(ate_eval, 3)
    )
    
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning("Error in compute_qini_improved: {e$message}")
    NULL
  })
}