#' Compute Qini Curves for Multi-Arm Treatments
#'
#' @description
#' Computes Qini curves for multi-arm treatments using the maq package.
#'
#' @param tau_hat Matrix of estimated treatment effects.
#' @param Y Vector of observed outcomes.
#' @param W_multi Matrix of treatment assignments.
#'
#' @return A data frame containing Qini curve data for plotting.
#'
#' @importFrom maq get_ipw_scores maq
#' @importFrom purrr map2_dfr
#'
#' @keywords internal
compute_qini_curves <- function(tau_hat, Y, W_multi) {
  # compute ipw scores
  IPW_scores <- maq::get_ipw_scores(Y, W_multi)

  # set cost for each treatment arm to be the same
  cost <- rep(1, ncol(tau_hat))

  # compute qini curve for the multi-arm case with covariates
  ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)

  # compute qini curve for the multi-arm case without covariates
  ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)

  # create a list of all qini objects
  qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)

  # dynamically add each arm to the list
  for (i in 1:ncol(tau_hat)) {
    qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
  }

  # determine the maximum index to extend all curves to
  max_index <- max(sapply(qini_objects, function(qini_obj) length(qini_obj[["_path"]]$gain)))

  # extract qini data for plotting
  qini_data <- map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))

  return(qini_data)
}
