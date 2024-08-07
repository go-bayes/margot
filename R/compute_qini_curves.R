#' Compute Qini Curves for Multi-Arm and Binary Treatments
#'
#' @description
#' Computes Qini curves for multi-arm and binary treatments using the maq package.
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
  # Determine if using multi-arm or binary treatment
  is_multi_arm <- !is.null(W_multi)

  # Ensure tau_hat is a matrix for multi-arm, vector for binary
  if (is_multi_arm) {
    tau_hat <- as.matrix(tau_hat)
    treatment <- W_multi
  } else {
    tau_hat <- as.vector(tau_hat)
    treatment <- as.factor(W)
  }

  # Compute IPW scores
  IPW_scores <- maq::get_ipw_scores(Y, treatment)

  # Set cost for each treatment arm or for binary treatment
  if (is_multi_arm) {
    cost <- rep(1, ncol(tau_hat))
  } else {
    cost <- 1
  }

  # Compute qini curve with covariates
  ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)

  # Compute qini curve without covariates
  ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)

  # Create a list of all qini objects
  qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)

  # Add individual arms for multi-arm treatment
  if (is_multi_arm) {
    for (i in 1:ncol(tau_hat)) {
      qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
    }
  } else {
    # For binary treatment, add the single treatment arm
    qini_objects[["treatment_arm"]] <- ma_qini
  }

  # Determine the maximum index to extend all curves to
  max_index <- max(sapply(qini_objects, function(qini_obj) length(qini_obj[["_path"]]$gain)))

  # Extract qini data for plotting
  qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))

  return(qini_data)
}
