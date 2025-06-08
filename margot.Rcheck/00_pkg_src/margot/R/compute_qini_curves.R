#' Compute Qini Curves for Multi-Arm and Binary Treatments (Deprecated)
#'
#' @description
#' DEPRECATED: This function is no longer functional and will be removed in a future version.
#' Please use `compute_qini_curves_binary()` for binary treatments and
#' `compute_qini_curves_multi_arm()` for multi-arm treatments instead.
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
#' @keywords internal deprecated
#'
#' @seealso
#' \code{\link{compute_qini_curves_binary}} for binary treatments
#' \code{\link{compute_qini_curves_multi_arm}} for multi-arm treatments
compute_qini_curves <- function(tau_hat, Y, W = NULL, W_multi = NULL) {
  .Deprecated("compute_qini_curves_binary() or compute_qini_curves_multi_arm()")
  tryCatch({
    # Debug information
    cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
    cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
    cli::cli_alert_info(paste("Y class:", class(Y)))
    cli::cli_alert_info(paste("Y dimensions:", paste(dim(Y), collapse = "x")))
    cli::cli_alert_info(paste("W class:", class(W)))
    cli::cli_alert_info(paste("W length:", length(W)))

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

    # Set cost appropriately
    cost <- if (is_multi_arm) rep(1, ncol(tau_hat)) else 1

    # Debug information for maq inputs
    cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
    cli::cli_alert_info(paste("cost length:", length(cost)))
    cli::cli_alert_info(paste("IPW_scores dimensions:", paste(dim(IPW_scores), collapse = "x")))

    # Compute qini curves
    if (is_multi_arm) {
      # Multi-arm treatment
      ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
      ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
      qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)
      for (i in 1:ncol(tau_hat)) {
        qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
      }
    } else {
      # Binary treatment
      treatment_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
      baseline_qini <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
      qini_objects <- list(treatment = treatment_qini, baseline = baseline_qini)
    }

    # Determine the maximum index
    max_index <- max(sapply(qini_objects, function(qini_obj) {
      if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
        return(0)
      }
      length(qini_obj[["_path"]]$gain)
    }))

    if (max_index == 0) {
      cli::cli_alert_warning("All Qini objects have empty gain. Returning NULL.")
      return(NULL)
    }

    # Extract qini data for plotting
    qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))

    if (nrow(qini_data) == 0) {
      cli::cli_alert_warning("Extracted Qini data is empty. Returning NULL.")
      return(NULL)
    }

    return(qini_data)
  }, error = function(e) {
    cli::cli_alert_warning(paste("Error in compute_qini_curves:", e$message))
    return(NULL)
  })
}

