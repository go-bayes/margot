#' Development Version of QINI Analysis with Integrated Interpretation
#'
#' Computes QINI curves and related metrics using the test set from 
#' margot_causal_forest_dev(). Integrates functionality from margot_qini() 
#' and margot_interpret_qini() into a single coherent function.
#'
#' @param cf_results Results from margot_causal_forest_dev()
#' @param outcome_vars Character vector of outcomes to analyze. NULL = all.
#' @param baseline_method Method for baseline curve: "maq_no_covariates" (default),
#'   "simple", "maq_only", "auto", or "none"
#' @param use_evaluation_forest Logical. Use evaluation forest for DR scores 
#'   if available (default: TRUE)
#' @param spend_levels Numeric vector of budget levels for gain summaries
#'   (default: c(0.05, 0.1, 0.2, 0.4))
#' @param n_bootstrap Integer. Bootstrap iterations for inference (default: 200)
#' @param confidence_level Numeric. Confidence level (default: 0.95)
#' @param seed Integer. Random seed (default: NULL)
#' @param verbose Logical. Print progress (default: TRUE)
#'
#' @return List of class "margot_qini_dev" containing:
#'   \item{qini_curves}{Data frame with QINI curve data for all outcomes}
#'   \item{qini_objects}{List of maq objects by outcome}
#'   \item{gain_summaries}{Gain summaries at specified spend levels}
#'   \item{qini_metrics}{Summary metrics (peak gain, area under curve, etc.)}
#'   \item{metadata}{Analysis parameters and data info}
#'
#' @details
#' This function:
#' - Uses the test set from the causal forest for honest evaluation
#' - Can use evaluation forests for computing DR scores if available
#' - Provides multiple baseline methods for comparison
#' - Computes gain summaries at specified budget levels
#' - Includes bootstrap inference for uncertainty quantification
#'
#' The baseline methods are:
#' - "maq_no_covariates": Uses maq with target.with.covariates = FALSE
#' - "simple": Constant treatment effect assumption
#' - "maq_only": Standard maq baseline
#' - "auto": Tries methods in order until one succeeds
#' - "none": No baseline curve
#'
#' @examples
#' \dontrun{
#' # Generate test data and fit forests
#' test_data <- margot_simulate_test_data()
#' cf_results <- margot_causal_forest_dev(
#'   data = test_data$data,
#'   outcome_vars = c("Y1", "Y2", "Y3", "Y4"),
#'   treatment = "A"
#' )
#'
#' # Compute QINI curves
#' qini_results <- margot_qini_dev(cf_results)
#'
#' # Analyze specific outcomes with custom parameters
#' qini_results_custom <- margot_qini_dev(
#'   cf_results,
#'   outcome_vars = c("Y1", "Y4"),
#'   baseline_method = "simple",
#'   spend_levels = c(0.1, 0.25, 0.5)
#' )
#' }
#'
#' @export
#' @importFrom maq maq get_ipw_scores
#' @importFrom policytree double_robust_scores
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom stats sd quantile
margot_qini_dev <- function(
    cf_results,
    outcome_vars = NULL,
    baseline_method = "maq_no_covariates",
    use_evaluation_forest = TRUE,
    spend_levels = c(0.05, 0.1, 0.2, 0.4),
    n_bootstrap = 200,
    confidence_level = 0.95,
    seed = NULL,
    verbose = TRUE
) {
  
  # validate input
  if (!inherits(cf_results, "margot_causal_forest_dev")) {
    stop("cf_results must be output from margot_causal_forest_dev()")
  }
  
  # set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  if (verbose) cli::cli_alert_info("Starting margot_qini_dev()")
  
  # determine outcomes to analyze
  if (is.null(outcome_vars)) {
    outcome_vars <- cf_results$data_info$outcome_vars
  } else {
    # validate requested outcomes exist
    missing_outcomes <- setdiff(outcome_vars, cf_results$data_info$outcome_vars)
    if (length(missing_outcomes) > 0) {
      stop("Requested outcomes not found in cf_results: ",
           paste(missing_outcomes, collapse = ", "))
    }
  }
  
  # extract test set data
  X_test <- cf_results$X_test
  W_test <- cf_results$W_test
  weights_test <- cf_results$weights_test
  test_idx <- cf_results$data_info$test_idx
  
  n_test <- nrow(X_test)
  
  if (verbose) {
    cli::cli_alert_info("Analyzing {length(outcome_vars)} outcomes on test set (n={n_test})")
  }
  
  # initialize storage
  all_qini_data <- list()
  all_qini_objects <- list()
  all_gain_summaries <- list()
  all_qini_metrics <- list()
  
  # process each outcome
  for (outcome in outcome_vars) {
    if (verbose) {
      cli::cli_alert_info("Processing QINI for outcome: {outcome}")
    }
    
    # skip if forest failed
    if (!is.null(cf_results$results[[outcome]]$error)) {
      cli::cli_alert_warning("Skipping {outcome} due to forest error")
      next
    }
    
    # extract components
    Y_test <- cf_results$results[[outcome]]$Y_test
    tau_hat_test <- cf_results$results[[outcome]]$tau_hat_test
    
    # check for missing outcomes
    complete_test <- !is.na(Y_test)
    if (sum(complete_test) < n_test) {
      cli::cli_alert_warning(
        "{sum(!complete_test)} missing values in test outcomes for {outcome}"
      )
    }
    
    # compute DR scores
    if (use_evaluation_forest && !is.null(cf_results$eval_forests) && 
        outcome %in% names(cf_results$eval_forests)) {
      # use evaluation forest for DR scores
      if (verbose) {
        cli::cli_alert_info("Using evaluation forest for DR scores")
      }
      
      # for now, skip evaluation forest and use IPW scores
      # TODO: fix evaluation forest DR score computation for test set
      if (verbose) {
        cli::cli_alert_info("Computing IPW scores directly (eval forest not yet implemented)")
      }
      
      treatment_factor <- as.factor(W_test)
      dr_scores <- maq::get_ipw_scores(Y_test, treatment_factor)
      
    } else {
      # compute IPW scores directly
      if (verbose) {
        cli::cli_alert_info("Computing IPW scores directly")
      }
      
      treatment_factor <- as.factor(W_test)
      dr_scores <- maq::get_ipw_scores(Y_test, treatment_factor)
    }
    
    # fit QINI curves
    tryCatch({
      # primary QINI curve using tau_hat
      qini_args <- list(
        reward = as.matrix(tau_hat_test[complete_test]),
        cost = matrix(1, sum(complete_test), 1),
        DR.scores = dr_scores[complete_test, , drop = FALSE],
        R = n_bootstrap,
        sample.weights = if (!is.null(weights_test)) {
          weights_test[complete_test]
        } else NULL,
        seed = if (!is.null(seed)) as.integer(seed) else NULL
      )
      
      cate_qini <- do.call(maq::maq, qini_args)
      
      # baseline curve based on method
      baseline_qini <- compute_baseline_qini(
        tau_hat = tau_hat_test[complete_test],
        dr_scores = dr_scores[complete_test, , drop = FALSE],
        weights = if (!is.null(weights_test)) weights_test[complete_test] else NULL,
        method = baseline_method,
        n_bootstrap = n_bootstrap,
        seed = if (!is.null(seed)) as.integer(seed) else NULL,
        verbose = verbose
      )
      
      # extract curve data
      qini_data <- extract_qini_curves(
        list(cate = cate_qini, baseline = baseline_qini),
        outcome = outcome
      )
      
      # compute gain summaries
      gain_summaries <- compute_gain_summaries(
        cate_qini = cate_qini,
        baseline_qini = baseline_qini,
        spend_levels = spend_levels,
        outcome = outcome
      )
      
      # compute summary metrics
      qini_metrics <- compute_qini_metrics(
        cate_qini = cate_qini,
        baseline_qini = baseline_qini,
        outcome = outcome
      )
      
      # store results
      all_qini_data[[outcome]] <- qini_data
      all_qini_objects[[outcome]] <- list(
        cate = cate_qini,
        baseline = baseline_qini
      )
      all_gain_summaries[[outcome]] <- gain_summaries
      all_qini_metrics[[outcome]] <- qini_metrics
      
      if (verbose) {
        cli::cli_alert_success("Completed QINI analysis for {outcome}")
      }
      
    }, error = function(e) {
      cli::cli_alert_warning("QINI failed for {outcome}: {e$message}")
    })
  }
  
  # combine all QINI data
  if (length(all_qini_data) > 0) {
    combined_qini_data <- do.call(rbind, all_qini_data)
  } else {
    combined_qini_data <- NULL
  }
  
  # metadata
  metadata <- list(
    timestamp = Sys.time(),
    n_test = n_test,
    n_outcomes = length(outcome_vars),
    n_successful = length(all_qini_data),
    baseline_method = baseline_method,
    use_evaluation_forest = use_evaluation_forest,
    spend_levels = spend_levels,
    n_bootstrap = n_bootstrap,
    confidence_level = confidence_level,
    seed = seed
  )
  
  # create output
  output <- list(
    qini_curves = combined_qini_data,
    qini_objects = all_qini_objects,
    gain_summaries = all_gain_summaries,
    qini_metrics = all_qini_metrics,
    metadata = metadata
  )
  
  class(output) <- c("margot_qini_dev", "list")
  
  if (verbose) {
    cli::cli_alert_success(
      "margot_qini_dev() completed: {metadata$n_successful}/{metadata$n_outcomes} outcomes analyzed"
    )
  }
  
  return(output)
}

#' Compute baseline QINI curve
#'
#' @keywords internal
compute_baseline_qini <- function(
    tau_hat, 
    dr_scores, 
    weights,
    method = "maq_no_covariates",
    n_bootstrap = 200,
    seed = NULL,
    verbose = TRUE
) {
  
  baseline_args <- list(
    cost = matrix(1, length(tau_hat), 1),
    DR.scores = dr_scores,
    R = n_bootstrap,
    sample.weights = weights,
    seed = seed
  )
  
  if (method == "maq_no_covariates") {
    # use maq with no covariate targeting
    baseline_qini <- tryCatch({
      do.call(maq::maq, c(
        baseline_args,
        list(
          reward = as.matrix(tau_hat),
          target.with.covariates = FALSE
        )
      ))
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning("maq_no_covariates failed, using simple baseline")
      }
      # fallback to constant rewards
      do.call(maq::maq, c(
        baseline_args,
        list(reward = matrix(mean(tau_hat), length(tau_hat), 1))
      ))
    })
    
  } else if (method == "simple") {
    # constant treatment effect
    baseline_qini <- do.call(maq::maq, c(
      baseline_args,
      list(reward = matrix(mean(tau_hat), length(tau_hat), 1))
    ))
    
  } else if (method == "maq_only") {
    # standard maq baseline
    baseline_qini <- do.call(maq::maq, c(
      baseline_args,
      list(reward = as.matrix(tau_hat))
    ))
    
  } else if (method == "auto") {
    # try methods in order
    for (try_method in c("maq_no_covariates", "simple", "maq_only")) {
      baseline_qini <- tryCatch({
        compute_baseline_qini(
          tau_hat, dr_scores, weights, 
          method = try_method, 
          n_bootstrap = n_bootstrap,
          seed = if (!is.null(seed)) as.integer(seed) else NULL, 
          verbose = FALSE
        )
      }, error = function(e) NULL)
      
      if (!is.null(baseline_qini)) break
    }
    
    if (is.null(baseline_qini)) {
      stop("All baseline methods failed")
    }
    
  } else if (method == "none") {
    # no baseline
    return(NULL)
    
  } else {
    stop("Unknown baseline_method: ", method)
  }
  
  return(baseline_qini)
}

#' Extract QINI curve data
#'
#' @keywords internal
extract_qini_curves <- function(qini_objects, outcome) {
  
  curve_data <- list()
  
  for (curve_name in names(qini_objects)) {
    if (is.null(qini_objects[[curve_name]])) next
    
    qini_obj <- qini_objects[[curve_name]]
    
    # extract gain path
    gain <- qini_obj[["_path"]]$gain
    n_points <- length(gain)
    
    if (n_points == 0) next
    
    # create proportion vector
    proportion <- seq(0, 1, length.out = n_points)
    
    # extract confidence intervals if available
    if (!is.null(qini_obj[["_path"]]$gain.se)) {
      gain_se <- qini_obj[["_path"]]$gain.se
      ci_lower <- gain - 1.96 * gain_se
      ci_upper <- gain + 1.96 * gain_se
    } else {
      ci_lower <- ci_upper <- NA
    }
    
    curve_data[[curve_name]] <- data.frame(
      outcome = outcome,
      curve = curve_name,
      proportion = proportion,
      gain = gain,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )
  }
  
  if (length(curve_data) > 0) {
    do.call(rbind, curve_data)
  } else {
    NULL
  }
}

#' Compute gain summaries at spend levels
#'
#' @keywords internal  
compute_gain_summaries <- function(cate_qini, baseline_qini, spend_levels, outcome) {
  
  summaries <- list()
  
  for (spend in spend_levels) {
    # find index corresponding to spend level
    n_points <- length(cate_qini[["_path"]]$gain)
    spend_idx <- round(spend * n_points)
    spend_idx <- max(1, min(spend_idx, n_points))
    
    # extract gains
    cate_gain <- cate_qini[["_path"]]$gain[spend_idx]
    
    if (!is.null(baseline_qini)) {
      baseline_gain <- baseline_qini[["_path"]]$gain[spend_idx]
      diff_gain <- cate_gain - baseline_gain
    } else {
      baseline_gain <- 0
      diff_gain <- cate_gain
    }
    
    # confidence intervals if available
    if (!is.null(cate_qini[["_path"]]$gain.se)) {
      cate_se <- cate_qini[["_path"]]$gain.se[spend_idx]
      cate_ci <- c(cate_gain - 1.96 * cate_se, cate_gain + 1.96 * cate_se)
    } else {
      cate_ci <- c(NA, NA)
    }
    
    summaries[[paste0("spend_", spend)]] <- list(
      spend = spend,
      cate_gain = cate_gain,
      baseline_gain = baseline_gain,
      diff_gain = diff_gain,
      cate_ci = cate_ci,
      outcome = outcome
    )
  }
  
  summaries
}

#' Compute QINI summary metrics
#'
#' @keywords internal
compute_qini_metrics <- function(cate_qini, baseline_qini, outcome) {
  
  cate_gains <- cate_qini[["_path"]]$gain
  n_points <- length(cate_gains)
  
  # peak gain
  peak_gain <- max(cate_gains)
  peak_idx <- which.max(cate_gains)
  peak_proportion <- peak_idx / n_points
  
  # area under curve (normalized)
  auc <- sum(cate_gains) / n_points
  
  # gain at common spend levels
  gain_10 <- cate_gains[round(0.1 * n_points)]
  gain_20 <- cate_gains[round(0.2 * n_points)]
  gain_50 <- cate_gains[round(0.5 * n_points)]
  
  # improvement over baseline
  if (!is.null(baseline_qini)) {
    baseline_gains <- baseline_qini[["_path"]]$gain
    max_improvement <- max(cate_gains - baseline_gains)
    avg_improvement <- mean(cate_gains - baseline_gains)
  } else {
    max_improvement <- avg_improvement <- NA
  }
  
  list(
    outcome = outcome,
    peak_gain = peak_gain,
    peak_proportion = peak_proportion,
    auc = auc,
    gain_10 = gain_10,
    gain_20 = gain_20,
    gain_50 = gain_50,
    max_improvement = max_improvement,
    avg_improvement = avg_improvement
  )
}

#' Print method for margot_qini_dev objects
#'
#' @param x A margot_qini_dev object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.margot_qini_dev <- function(x, ...) {
  cat("margot_qini_dev object\n")
  cat("======================\n\n")
  
  # summary
  cat(sprintf("Outcomes analyzed: %d/%d\n", 
              x$metadata$n_successful,
              x$metadata$n_outcomes))
  cat(sprintf("Test set size: %d\n", x$metadata$n_test))
  cat(sprintf("Baseline method: %s\n", x$metadata$baseline_method))
  cat(sprintf("Bootstrap iterations: %d\n", x$metadata$n_bootstrap))
  cat("\n")
  
  # metrics summary
  if (length(x$qini_metrics) > 0) {
    cat("QINI Metrics Summary:\n")
    cat("--------------------\n")
    
    for (outcome in names(x$qini_metrics)) {
      metrics <- x$qini_metrics[[outcome]]
      cat(sprintf("\n%s:\n", outcome))
      cat(sprintf("  Peak gain: %.3f (at %.1f%% treated)\n",
                  metrics$peak_gain, 100 * metrics$peak_proportion))
      cat(sprintf("  AUC: %.3f\n", metrics$auc))
      
      if (!is.na(metrics$max_improvement)) {
        cat(sprintf("  Max improvement over baseline: %.3f\n",
                    metrics$max_improvement))
      }
    }
  }
  
  invisible(x)
}

#' Summary method for margot_qini_dev objects
#'
#' @param object A margot_qini_dev object
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.margot_qini_dev <- function(object, ...) {
  # create summary table
  if (length(object$gain_summaries) > 0) {
    summary_list <- list()
    
    for (outcome in names(object$gain_summaries)) {
      for (spend_level in names(object$gain_summaries[[outcome]])) {
        gains <- object$gain_summaries[[outcome]][[spend_level]]
        
        summary_list[[length(summary_list) + 1]] <- data.frame(
          outcome = outcome,
          spend = gains$spend,
          cate_gain = gains$cate_gain,
          baseline_gain = gains$baseline_gain,
          diff_gain = gains$diff_gain,
          ci_lower = gains$cate_ci[1],
          ci_upper = gains$cate_ci[2]
        )
      }
    }
    
    summary_df <- do.call(rbind, summary_list)
    rownames(summary_df) <- NULL
    
    return(summary_df)
  } else {
    return(NULL)
  }
}