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
  # Input validation
  if (is.null(tau_hat) || is.null(Y) || (is.null(W) && is.null(W_multi))) {
    stop("tau_hat, Y, and either W or W_multi must be provided")
  }

  if (length(Y) != nrow(as.matrix(tau_hat))) {
    stop("Length of Y must match the number of rows in tau_hat")
  }

  # Determine which treatment assignment variable to use
  is_multi_arm <- !is.null(W_multi)
  treatment <- if (is_multi_arm) W_multi else W

  if (length(Y) != length(treatment)) {
    stop("Length of Y must match the length of the treatment assignment variable")
  }

  # Ensure tau_hat is a matrix
  tau_hat <- as.matrix(tau_hat)

  if (!is_multi_arm) {
    treatment <- as.factor(treatment)
  } else if (!is.factor(treatment)) {
    stop("For multi-arm cases, W_multi should be a factor")
  }

  # Compute ipw scores
  tryCatch({
    IPW_scores <- maq::get_ipw_scores(Y, treatment)
  }, error = function(e) {
    stop("Error in computing IPW scores: ", e$message)
  })

  # Set cost
  cost <- rep(1, ncol(tau_hat))

  # Compute qini curves
  compute_maq <- function(tau, c, ipw, with_covariates = TRUE) {
    tryCatch({
      maq::maq(tau, c, ipw, target.with.covariates = with_covariates, R = 200)
    }, error = function(e) {
      warning("Error in computing MAQ: ", e$message)
      NULL
    })
  }

  # Compute qini curves
  ma_qini <- compute_maq(tau_hat, cost, IPW_scores)
  ma_qini_baseline <- compute_maq(tau_hat, cost, IPW_scores, FALSE)

  if (is.null(ma_qini) || is.null(ma_qini_baseline)) {
    stop("Failed to compute main Qini curves")
  }

  # Create a list of qini objects
  if (is_multi_arm) {
    qini_objects <- list(all_treatment_arms = ma_qini, baseline = ma_qini_baseline)
  } else {
    qini_objects <- list(treatment_arm = ma_qini, baseline = ma_qini_baseline)
  }

  # For multi-arm case, add each arm to the list
  if (is_multi_arm) {
    for (i in 1:ncol(tau_hat)) {
      arm_qini <- compute_maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE])
      if (!is.null(arm_qini)) {
        qini_objects[[paste0("arm", i)]] <- arm_qini
      }
    }
  }

  # Determine the maximum index to extend all curves to
  max_index <- max(sapply(qini_objects, function(qini_obj) length(qini_obj[["_path"]]$gain)))

  # Extract qini data for plotting
  tryCatch({
    qini_data <- map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))
  }, error = function(e) {
    stop("Error in extracting Qini data: ", e$message)
  })

  return(qini_data)
}
