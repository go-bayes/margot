#' Compute Qini Curves for Causal Forest and Multi-Arm Causal Forest
#'
#' @description
#' Computes Qini curves for both causal forest and multi-arm causal forest using the maq package.
#'
#' @param tau_hat Matrix or vector of estimated treatment effects.
#' @param Y Vector of observed outcomes.
#' @param W Vector of treatment assignments for binary treatment.
#' @param W_multi Factor of treatment assignments for multi-arm treatment.
#'
#' @return A data frame containing Qini curve data for plotting.
#'
#' @importFrom maq get_ipw_scores maq
#' @importFrom purrr map2_dfr
#'
#' @keywords internal
compute_qini_curves <- function(tau_hat, Y, W = NULL, W_multi = NULL) {
  # Ensure tau_hat is a matrix
  tau_hat <- as.matrix(tau_hat)

  # Determine if using multi-arm or binary treatment
  is_multi_arm <- !is.null(W_multi)

  # Ensure treatment is provided and is appropriately formatted
  if (is_multi_arm) {
    treatment <- W_multi
    if (!is.factor(treatment)) {
      stop("For multi-arm cases, W_multi should be a factor")
    }
  } else if (!is.null(W)) {
    treatment <- as.factor(W)
  } else {
    stop("Either W or W_multi must be provided")
  }

  # Compute IPW scores
  IPW_scores <- maq::get_ipw_scores(Y, treatment)

  # Set cost for each treatment arm or for binary treatment
  cost <- rep(1, ncol(tau_hat))

  # Compute Qini curves with and without covariates
  ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
  ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)

  # Create list of Qini objects
  if (is_multi_arm) {
    qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)
    # Dynamically add each arm to the list
    for (i in 1:ncol(tau_hat)) {
      qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
    }
  } else {
    qini_objects <- list(treatment_arm = ma_qini, baseline = ma_qini_baseline)
  }

  # Determine the maximum index to extend all curves to
  max_index <- max(sapply(qini_objects, function(qini_obj) length(qini_obj[["_path"]]$gain)))

  # Extract Qini data for plotting
  qini_data <- map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))

  return(qini_data)
}
