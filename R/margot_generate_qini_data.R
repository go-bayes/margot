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
#' @param baseline_method Method for generating baseline: "maq_no_covariates" (default), 
#'   "auto", "simple", "maq_only", or "none". See details.
#' @param verbose Logical for verbose output
#' @param treatment_cost Scalar treatment cost per unit. Default 1.
#' @param x_axis Type of x-axis for QINI curves: "proportion" (default) or "budget".
#'   "proportion" shows proportion of population treated (0 to 1).
#'   "budget" shows budget per unit (0 to treatment_cost), matching maq's visualization.
#'
#' @details
#' The baseline_method parameter controls how the no-prioritization baseline is generated:
#' \itemize{
#'   \item "maq_no_covariates": Use maq with target.with.covariates = FALSE (default). 
#'     Automatically falls back to simple baseline if maq fails.
#'   \item "auto": Try maq with target.with.covariates = FALSE first, fall back to simple baseline if it fails
#'   \item "simple": Always use simple baseline (straight line from (0,0) to (1, mean(tau_hat)))
#'   \item "maq_only": Use standard maq with constant rewards (may fail with no fallback)
#'   \item "none": No baseline curve
#' }
#' 
#' The simple baseline represents the expected gain under random allocation,
#' where treating proportion p of units yields gain p * E[tau]. This method
#' always succeeds and provides a robust fallback when maq fails.
#'
#' @return A list with components:
#'   - qini_data: data.frame with columns proportion (or budget), gain, curve
#'   - qini_objects: list with cate and ate maq objects (ate may be simplified)
#'   - x_axis: the x-axis type used ("proportion" or "budget")
#'
#' @keywords internal
margot_generate_qini_data <- function(model_result, 
                                     outcome_data, 
                                     treatment,
                                     weights = NULL,
                                     baseline_method = c("maq_no_covariates", "auto", "simple", "maq_only", "none"),
                                     seed = NULL,
                                     verbose = FALSE,
                                     treatment_cost = 1,
                                     x_axis = c("proportion", "budget")) {
  
  # if baseline_method is not explicitly provided and we have metadata, use the stored method
  if (missing(baseline_method) && !is.null(model_result$qini_metadata$baseline_method)) {
    baseline_method <- model_result$qini_metadata$baseline_method
    if (verbose) {
      cli::cli_alert_info("Using stored baseline method from metadata: {baseline_method}")
    }
  } else {
    baseline_method <- match.arg(baseline_method)
  }
  
  # match x_axis argument
  x_axis <- match.arg(x_axis)
  
  if (verbose) {
    cli::cli_alert_info("Generating QINI curves with baseline method: {baseline_method}")
    cli::cli_alert_info("Using x-axis type: {x_axis}")
  }
  
  # extract tau_hat from the model result
  tau_hat <- model_result$tau_hat
  if (is.null(tau_hat)) {
    stop("tau_hat not found in model result")
  }
  
  if (verbose) {
    cli::cli_alert_info("Initial tau_hat length: {length(tau_hat)}")
    cli::cli_alert_info("qini_metadata present: {!is.null(model_result$qini_metadata)}")
    if (!is.null(model_result$qini_metadata)) {
      cli::cli_alert_info("test_indices present: {!is.null(model_result$qini_metadata$test_indices)}")
    }
  }
  
  # check if we should use a subset of data based on qini_metadata
  if (!is.null(model_result$qini_metadata) && !is.null(model_result$qini_metadata$test_indices)) {
    test_indices <- model_result$qini_metadata$test_indices
    if (verbose) {
      cli::cli_alert_info("Using QINI test indices from metadata (n={length(test_indices)})")
    }
    
    # when qini_split = TRUE, we need to handle tau_hat differently
    # the stored tau_hat is for the full dataset, but we need predictions for test indices only
    if (!is.null(model_result$qini_metadata$qini_split) && model_result$qini_metadata$qini_split) {
      if (verbose) {
        cli::cli_alert_info("qini_split = TRUE detected, will regenerate tau_hat for test indices")
      }
      
      # check if we have the model to regenerate predictions
      if (!is.null(model_result$model)) {
        if (verbose) {
          cli::cli_alert_info("Regenerating tau_hat predictions for test subset")
        }
        # get covariates for test indices
        if (!is.null(model_result$model$X.orig)) {
          test_covariates <- model_result$model$X.orig[test_indices, , drop = FALSE]
          tau_hat <- predict(model_result$model, newdata = test_covariates)$predictions
          if (verbose) {
            cli::cli_alert_info("Regenerated tau_hat for {length(tau_hat)} test observations")
          }
        } else {
          cli::cli_alert_warning("Cannot regenerate tau_hat: model$X.orig not available")
          # fall back to subsetting
          tau_hat <- tau_hat[test_indices]
        }
      } else {
        if (verbose) {
          cli::cli_alert_info("Model not available, subsetting existing tau_hat")
        }
        tau_hat <- tau_hat[test_indices]
      }
    } else {
      # standard subsetting for non-qini_split cases
      if (length(tau_hat) == length(test_indices)) {
        # tau_hat is already for test set only, no subsetting needed
        if (verbose) {
          cli::cli_alert_info("tau_hat already matches test set size, no subsetting needed")
        }
      } else if (length(tau_hat) > length(test_indices)) {
        # need to subset the data
        if (verbose) {
          cli::cli_alert_info("Subsetting tau_hat to match test indices: {length(tau_hat)} -> {length(test_indices)}")
        }
        tau_hat <- tau_hat[test_indices]
      }
    }
    
    # always subset outcome, treatment, and weights to match test indices
    if (length(outcome_data) > length(test_indices)) {
      outcome_data <- outcome_data[test_indices]
      treatment <- treatment[test_indices]
      if (!is.null(weights)) {
        weights <- weights[test_indices]
      }
    }
  }
  
  # ensure vectors
  tau_hat <- as.vector(tau_hat)
  Y <- as.vector(outcome_data)
  W <- as.vector(treatment)
  
  # get IPW scores
  treatment_factor <- as.factor(W)
  if (verbose) {
    cli::cli_alert_info("Computing IPW scores:")
    cli::cli_alert_info("  - Y length: {length(Y)}, range: [{round(min(Y), 3)}, {round(max(Y), 3)}]")
    cli::cli_alert_info("  - W unique values: {paste(sort(unique(W)), collapse = ', ')}")
    cli::cli_alert_info("  - treatment_factor levels: {paste(levels(treatment_factor), collapse = ', ')}")
  }
  
  IPW_scores <- tryCatch({
    maq::get_ipw_scores(Y, treatment_factor)
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning("Failed to compute IPW scores: {e$message}")
    }
    stop("Cannot proceed without IPW scores: ", e$message)
  })
  
  # generate CATE curve using maq
  cate_qini <- tryCatch({
    # add detailed logging
    if (verbose) {
      cli::cli_alert_info("Attempting to generate CATE curve:")
      cli::cli_alert_info("  - tau_hat length: {length(tau_hat)}")
      cli::cli_alert_info("  - tau_hat range: [{round(min(tau_hat), 3)}, {round(max(tau_hat), 3)}]")
      cli::cli_alert_info("  - IPW_scores dimensions: {paste(dim(IPW_scores), collapse = ' x ')}")
      cli::cli_alert_info("  - weights: {if(is.null(weights)) 'NULL' else paste0('length ', length(weights))}")
      cli::cli_alert_info("  - seed: {if(is.null(seed)) 'NULL' else seed}")
      cli::cli_alert_info("  - treatment_cost: {treatment_cost}")
    }
    
    maq::maq(
      reward = as.matrix(tau_hat),
      cost = matrix(treatment_cost, length(tau_hat), 1),
      DR.scores = IPW_scores,
      R = 200,
      sample.weights = weights,
      seed = seed
    )
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning("Failed to generate CATE curve: {e$message}")
      # add more diagnostic info
      if (grepl("DR.scores", e$message)) {
        cli::cli_alert_info("  Issue appears to be with DR.scores (IPW scores)")
      }
      if (grepl("reward", e$message)) {
        cli::cli_alert_info("  Issue appears to be with reward matrix (tau_hat)")
      }
    }
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
      n_units = length(tau_hat),
      treatment_cost = treatment_cost,
      x_axis = x_axis
    )
    if (verbose) cli::cli_alert_info("Using simple baseline from (0,0) to (1, {round(mean_tau, 3)})")
  } else if (baseline_method == "maq_no_covariates") {
    # use maq with target.with.covariates = FALSE, with automatic fallback
    ate_qini <- tryCatch({
      result <- maq::maq(
        reward = as.matrix(tau_hat),
        cost = matrix(treatment_cost, length(tau_hat), 1),
        DR.scores = IPW_scores,
        target.with.covariates = FALSE,
        R = 200,
        sample.weights = weights,
        seed = seed
      )
      result$baseline_type <- "maq_no_covariates"
      if (verbose) {
        cli::cli_alert_info("Generated baseline using maq with target.with.covariates = FALSE")
        # debug: check if _path exists
        if (is.null(result[["_path"]]) || is.null(result[["_path"]]$gain)) {
          cli::cli_alert_warning("maq baseline result missing _path$gain structure")
        }
      }
      result
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning("Failed to generate baseline with maq (no covariates): {e$message}")
        cli::cli_alert_info("Falling back to simple baseline from (0,0) to (1, {round(mean_tau, 3)})")
      }
      # fallback to simple baseline
      simple_baseline <- margot_qini_simple_baseline(
        mean_tau = mean_tau,
        n_points = 100,
        n_units = length(tau_hat),
        treatment_cost = treatment_cost,
        x_axis = x_axis
      )
      simple_baseline$baseline_type <- "simple_fallback"
      simple_baseline
    })
  } else if (baseline_method == "maq_only") {
    # try standard maq with constant rewards
    ate_qini <- tryCatch({
      result <- maq::maq(
        reward = matrix(rep(mean_tau, length(tau_hat)), ncol = 1),
        cost = matrix(treatment_cost, length(tau_hat), 1),
        DR.scores = IPW_scores,
        R = 200,
        sample.weights = weights,
        seed = seed
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
        cost = matrix(treatment_cost, length(tau_hat), 1),
        DR.scores = IPW_scores,
        target.with.covariates = FALSE,
        R = 200,
        sample.weights = weights,
        seed = seed
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
        n_units = length(tau_hat),
        treatment_cost = treatment_cost,
        x_axis = x_axis
      )
    })
  }
  
  # prepare qini objects
  qini_objs <- list()
  if (!is.null(cate_qini)) qini_objs$cate <- cate_qini
  if (!is.null(ate_qini)) qini_objs$ate <- ate_qini
  
  if (length(qini_objs) == 0) {
    return(list(qini_data = NULL, qini_objects = NULL, baseline_method = baseline_method, x_axis = x_axis))
  }
  
  # extract data for plotting
  max_idx <- max(sapply(qini_objs, function(q) {
    if (!is.null(q[["_path"]]$gain)) length(q[["_path"]]$gain) else 2
  }))
  
  if (max_idx == 0) {
    return(list(qini_data = NULL, qini_objects = NULL, baseline_method = baseline_method))
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
      
      # transform x-axis if using budget
      if (x_axis == "budget") {
        x_values <- proportion * treatment_cost
      } else {
        x_values <- proportion
      }
      
      curve_data <- data.frame(proportion = x_values, gain = gain, curve = curve_name)
    } else {
      # extract actual gain values
      gain <- qini_obj[["_path"]]$gain
      
      if (is.null(gain) || length(gain) == 0) {
        if (verbose) {
          cli::cli_alert_warning("No gain data for {curve_name}")
          # debug: check structure of qini_obj
          cli::cli_alert_info("Structure of {curve_name} object: {paste(names(qini_obj), collapse = ', ')}")
          if ("_path" %in% names(qini_obj)) {
            cli::cli_alert_info("_path contains: {paste(names(qini_obj[['_path']]), collapse = ', ')}")
          }
        }
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
      
      # transform x-axis if using budget
      if (x_axis == "budget") {
        x_values <- proportion * treatment_cost
      } else {
        x_values <- proportion
      }
      # transform x-axis if using budget
      if (x_axis == "budget") {
        x_values <- proportion * treatment_cost
      } else {
        x_values <- proportion
      }
      
      curve_data <- data.frame(proportion = x_values, gain = gain, curve = curve_name)
    }
    
    qini_data <- rbind(qini_data, curve_data)
  }
  
  if (is.null(qini_data) || nrow(qini_data) == 0) {
    return(list(qini_data = NULL, qini_objects = NULL, baseline_method = baseline_method, treatment_cost = treatment_cost, x_axis = x_axis))
  }
  
  return(list(qini_data = qini_data, qini_objects = qini_objs, baseline_method = baseline_method, treatment_cost = treatment_cost, x_axis = x_axis))
}