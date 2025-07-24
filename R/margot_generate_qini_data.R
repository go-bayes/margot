#' Generate QINI Data for Visualization
#'
#' This function generates QINI curve data on-demand from a model result.
#' It extracts tau_hat from the model, uses maq to generate the CATE curve,
#' and creates a baseline following various methods.
#'
#' @param model_result A single model result from margot_causal_forest output
#' @param outcome_data The outcome data (Y) for the model
#' @param treatment The treatment assignment vector (W)
#' @param weights Optional weights vector
#' @param baseline_method Method for generating baseline: "auto" (default), "simple", 
#'   "maq_no_covariates", "maq_only", or "none". See details.
#' @param verbose Logical for verbose output
#'
#' @details
#' The baseline_method parameter controls how the no-prioritization baseline is generated:
#' \itemize{
#'   \item "auto": Try maq with target.with.covariates = FALSE first, fall back to simple baseline if it fails
#'   \item "simple": Always use simple baseline (straight line from (0,0) to (1, mean(tau_hat)))
#'   \item "maq_no_covariates": Use maq with target.with.covariates = FALSE (may fail)
#'   \item "maq_only": Use standard maq with constant rewards (may fail)
#'   \item "none": No baseline curve
#' }
#' 
#' The simple baseline represents the expected gain under random allocation,
#' where treating proportion p of units yields gain p * E[tau]. This method
#' always succeeds and provides a robust fallback when maq fails.
#'
#' @return A list with components:
#'   - qini_data: data.frame with columns proportion, gain, curve
#'   - qini_objects: list with cate and ate maq objects (ate may be simplified)
#'
#' @keywords internal
margot_generate_qini_data <- function(model_result, 
                                     outcome_data, 
                                     treatment,
                                     weights = NULL,
                                     baseline_method = c("auto", "simple", "maq_no_covariates", "maq_only", "none"),
                                     verbose = FALSE) {
  
  baseline_method <- match.arg(baseline_method)
  
  # extract tau_hat from the model result
  tau_hat <- model_result$tau_hat
  if (is.null(tau_hat)) {
    stop("tau_hat not found in model result")
  }
  
  # ensure vectors
  tau_hat <- as.vector(tau_hat)
  Y <- as.vector(outcome_data)
  W <- as.vector(treatment)
  
  # get IPW scores
  treatment_factor <- as.factor(W)
  IPW_scores <- maq::get_ipw_scores(Y, treatment_factor)
  
  # generate CATE curve using maq
  cate_qini <- tryCatch({
    maq::maq(
      reward = as.matrix(tau_hat),
      cost = matrix(1, length(tau_hat), 1),
      DR.scores = IPW_scores,
      R = 200
    )
  }, error = function(e) {
    if (verbose) cli::cli_alert_warning("Failed to generate CATE curve: {e$message}")
    NULL
  })
  
  # generate baseline curve based on method
  mean_tau <- mean(tau_hat)
  ate_qini <- NULL
  
  if (baseline_method == "none") {
    # no baseline
    ate_qini <- NULL
  } else if (baseline_method == "simple") {
    # always use simple baseline
    ate_qini <- margot_qini_simple_baseline(
      mean_tau = mean_tau,
      n_points = 100,
      n_units = length(tau_hat)
    )
    if (verbose) cli::cli_alert_info("Using simple baseline from (0,0) to (1, {round(mean_tau, 3)})")
  } else if (baseline_method == "maq_no_covariates") {
    # use maq with target.with.covariates = FALSE
    ate_qini <- tryCatch({
      result <- maq::maq(
        reward = as.matrix(tau_hat),
        cost = matrix(1, length(tau_hat), 1),
        DR.scores = IPW_scores,
        target.with.covariates = FALSE,
        R = 200
      )
      result$baseline_type <- "maq_no_covariates"
      if (verbose) cli::cli_alert_info("Generated baseline using maq with target.with.covariates = FALSE")
      result
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("Failed to generate baseline with maq (no covariates): {e$message}")
      NULL  # will handle below
    })
  } else if (baseline_method == "maq_only") {
    # try standard maq with constant rewards
    ate_qini <- tryCatch({
      result <- maq::maq(
        reward = matrix(rep(mean_tau, length(tau_hat)), ncol = 1),
        cost = matrix(1, length(tau_hat), 1),
        DR.scores = IPW_scores,
        R = 200
      )
      result$baseline_type <- "maq_constant"
      if (verbose) cli::cli_alert_info("Generated baseline using maq with constant rewards")
      result
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("Failed to generate baseline with maq: {e$message}")
      NULL  # will handle below
    })
  } else {  # baseline_method == "auto"
    # try maq with target.with.covariates = FALSE first
    ate_qini <- tryCatch({
      result <- maq::maq(
        reward = as.matrix(tau_hat),
        cost = matrix(1, length(tau_hat), 1),
        DR.scores = IPW_scores,
        target.with.covariates = FALSE,
        R = 200
      )
      result$baseline_type <- "maq_no_covariates"
      if (verbose) cli::cli_alert_info("Generated baseline using maq with target.with.covariates = FALSE")
      result
    }, error = function(e) {
      if (verbose) cli::cli_alert_warning("Failed to generate baseline with maq: {e$message}, using simple baseline")
      # fallback to simple baseline
      margot_qini_simple_baseline(
        mean_tau = mean_tau,
        n_points = 100,
        n_units = length(tau_hat)
      )
    })
  }
  
  # prepare qini objects
  qini_objs <- list()
  if (!is.null(cate_qini)) qini_objs$cate <- cate_qini
  if (!is.null(ate_qini)) qini_objs$ate <- ate_qini
  
  if (length(qini_objs) == 0) {
    return(list(qini_data = NULL, qini_objects = NULL))
  }
  
  # extract data for plotting
  max_idx <- max(sapply(qini_objs, function(q) {
    if (!is.null(q[["_path"]]$gain)) length(q[["_path"]]$gain) else 2
  }))
  
  if (max_idx == 0) {
    return(list(qini_data = NULL, qini_objects = NULL))
  }
  
  # extract data from each curve
  qini_data <- NULL
  
  for (curve_name in names(qini_objs)) {
    qini_obj <- qini_objs[[curve_name]]
    
    # special handling for simple baselines
    if (curve_name == "ate" && inherits(qini_obj, "qini_simple_baseline")) {
      # extract data directly from simple baseline
      proportion <- qini_obj[["_path"]]$spend
      gain <- qini_obj[["_path"]]$gain
      
      # resample to match max_idx if needed
      if (length(proportion) != max_idx) {
        idx <- round(seq(1, length(proportion), length.out = max_idx))
        proportion <- proportion[idx]
        gain <- gain[idx]
      }
      
      curve_data <- data.frame(proportion = proportion, gain = gain, curve = curve_name)
    } else {
      # extract actual gain values
      gain <- qini_obj[["_path"]]$gain
      
      if (is.null(gain) || length(gain) == 0) {
        if (verbose) cli::cli_alert_warning("No gain data for {curve_name}")
        next
      }
      
      # handle length differences
      if (length(gain) < max_idx) {
        gain <- c(gain, rep(tail(gain, 1), max_idx - length(gain)))
      } else if (length(gain) > max_idx) {
        indices <- round(seq(1, length(gain), length.out = max_idx))
        gain <- gain[indices]
      }
      
      gain <- gain[1:max_idx]
      proportion <- seq(0, 1, length.out = max_idx)
      curve_data <- data.frame(proportion = proportion, gain = gain, curve = curve_name)
    }
    
    qini_data <- rbind(qini_data, curve_data)
  }
  
  if (is.null(qini_data) || nrow(qini_data) == 0) {
    return(list(qini_data = NULL, qini_objects = NULL))
  }
  
  return(list(qini_data = qini_data, qini_objects = qini_objs))
}