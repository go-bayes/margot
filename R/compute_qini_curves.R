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
  tryCatch({
    # Debug information
    cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
    cli::cli_alert_info(paste("tau_hat dimensions:", paste(dim(tau_hat$predictions), collapse = "x")))
    cli::cli_alert_info(paste("Y class:", class(Y)))
    cli::cli_alert_info(paste("Y dimensions:", paste(dim(Y), collapse = "x")))
    cli::cli_alert_info(paste("W_multi class:", class(W_multi)))
    cli::cli_alert_info(paste("W_multi length:", length(W_multi)))

    # Determine if using multi-arm or binary treatment
    is_multi_arm <- !is.null(W_multi)
    cli::cli_alert_info(paste("Is multi-arm treatment:", is_multi_arm))

    # Extract predictions from tau_hat if it's a list
    if (is.list(tau_hat) && "predictions" %in% names(tau_hat)) {
      tau_hat <- tau_hat$predictions
    }

    # Ensure tau_hat is a matrix for multi-arm, vector for binary
    if (is_multi_arm) {
      if (length(dim(tau_hat)) == 3) {
        tau_hat <- tau_hat[,,1]  # Take the first slice if it's a 3D array
      }
      tau_hat <- as.matrix(tau_hat)
      treatment <- W_multi
    } else {
      tau_hat <- as.vector(tau_hat)
      treatment <- as.factor(W)
    }
    cli::cli_alert_info(paste("tau_hat dimensions after conversion:", paste(dim(tau_hat), collapse = "x")))

    # Compute IPW scores
    cli::cli_alert_info("Computing IPW scores")
    IPW_scores <- maq::get_ipw_scores(Y, treatment)
    cli::cli_alert_info(paste("IPW_scores dimensions:", paste(dim(IPW_scores), collapse = "x")))

    # Ensure tau_hat has the same number of rows as IPW_scores
    if (nrow(tau_hat) != nrow(IPW_scores)) {
      cli::cli_alert_warning("Mismatch in number of rows between tau_hat and IPW_scores")
      return(NULL)
    }

    # Set cost appropriately
    cost <- rep(1, ncol(tau_hat))
    cli::cli_alert_info(paste("cost length:", length(cost)))

    # Compute qini curves
    cli::cli_alert_info("Computing Qini curves")
    ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
    ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
    qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)

    for (i in 1:ncol(tau_hat)) {
      qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
    }

    # Determine the maximum index
    max_index <- max(sapply(qini_objects, function(qini_obj) {
      if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
        return(0)
      }
      length(qini_obj[["_path"]]$gain)
    }))
    cli::cli_alert_info(paste("Maximum index:", max_index))

    if (max_index == 0) {
      cli::cli_alert_warning("All Qini objects have empty gain. Returning NULL.")
      return(NULL)
    }

    # Extract qini data for plotting
    qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))
    cli::cli_alert_info(paste("Extracted Qini data dimensions:", paste(dim(qini_data), collapse = "x")))

    if (nrow(qini_data) == 0) {
      cli::cli_alert_warning("Extracted Qini data is empty. Returning NULL.")
      return(NULL)
    }

    return(qini_data)
  }, error = function(e) {
    cli::cli_alert_danger(paste("Error in compute_qini_curves:", e$message))
    return(NULL)
  })
}
#old works for binary
# compute_qini_curves <- function(tau_hat, Y, W = NULL, W_multi = NULL) {
#   tryCatch({
#     # Debug information
#     cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
#     cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
#     cli::cli_alert_info(paste("Y class:", class(Y)))
#     cli::cli_alert_info(paste("Y dimensions:", paste(dim(Y), collapse = "x")))
#     cli::cli_alert_info(paste("W class:", class(W)))
#     cli::cli_alert_info(paste("W length:", length(W)))
#
#     # Determine if using multi-arm or binary treatment
#     is_multi_arm <- !is.null(W_multi)
#
#     # Ensure tau_hat is a matrix for multi-arm, vector for binary
#     if (is_multi_arm) {
#       tau_hat <- as.matrix(tau_hat)
#       treatment <- W_multi
#     } else {
#       tau_hat <- as.vector(tau_hat)
#       treatment <- as.factor(W)
#     }
#
#     # Compute IPW scores
#     IPW_scores <- maq::get_ipw_scores(Y, treatment)
#
#     # Set cost appropriately
#     cost <- if (is_multi_arm) rep(1, ncol(tau_hat)) else 1
#
#     # Debug information for maq inputs
#     cli::cli_alert_info(paste("tau_hat length:", length(tau_hat)))
#     cli::cli_alert_info(paste("cost length:", length(cost)))
#     cli::cli_alert_info(paste("IPW_scores dimensions:", paste(dim(IPW_scores), collapse = "x")))
#
#     # Compute qini curves
#     if (is_multi_arm) {
#       # Multi-arm treatment
#       ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
#       ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
#       qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)
#       for (i in 1:ncol(tau_hat)) {
#         qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
#       }
#     } else {
#       # Binary treatment
#       treatment_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
#       baseline_qini <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
#       qini_objects <- list(treatment = treatment_qini, baseline = baseline_qini)
#     }
#
#     # Determine the maximum index
#     max_index <- max(sapply(qini_objects, function(qini_obj) {
#       if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
#         return(0)
#       }
#       length(qini_obj[["_path"]]$gain)
#     }))
#
#     if (max_index == 0) {
#       cli::cli_alert_warning("All Qini objects have empty gain. Returning NULL.")
#       return(NULL)
#     }
#
#     # Extract qini data for plotting
#     qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))
#
#     if (nrow(qini_data) == 0) {
#       cli::cli_alert_warning("Extracted Qini data is empty. Returning NULL.")
#       return(NULL)
#     }
#
#     return(qini_data)
#   }, error = function(e) {
#     cli::cli_alert_warning(paste("Error in compute_qini_curves:", e$message))
#     return(NULL)
#   })
# }
# original
# compute_qini_curves <- function(tau_hat, Y, W = NULL, W_multi = NULL) {
#   # Determine if using multi-arm or binary treatment
#   is_multi_arm <- !is.null(W_multi)
#
#   # Ensure tau_hat is a matrix for multi-arm, vector for binary
#   if (is_multi_arm) {
#     tau_hat <- as.matrix(tau_hat)
#     treatment <- W_multi
#   } else {
#     tau_hat <- as.vector(tau_hat)
#     treatment <- as.factor(W)
#   }
#
#   # Compute IPW scores
#   IPW_scores <- maq::get_ipw_scores(Y, treatment)
#
#   # Set cost appropriately
#   cost <- if (is_multi_arm) rep(1, ncol(tau_hat)) else 1
#
#   # Compute qini curves
#   if (is_multi_arm) {
#     # Multi-arm treatment
#     ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
#     ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
#
#     qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)
#
#     for (i in 1:ncol(tau_hat)) {
#       qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
#     }
#   } else {
#     # Binary treatment
#     treatment_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
#     baseline_qini <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
#
#     qini_objects <- list(treatment = treatment_qini, baseline = baseline_qini)
#   }
#
#   # Determine the maximum index
#   max_index <- max(sapply(qini_objects, function(qini_obj) {
#     if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
#       return(0)
#     }
#     length(qini_obj[["_path"]]$gain)
#   }))
#
#   # Extract qini data for plotting
#   qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))
#
#   return(qini_data)
# }
# working for multi-arm treatments
# compute_qini_curves <- function(tau_hat, Y, W = NULL, W_multi = NULL) {
#   # Determine if using multi-arm or binary treatment
#   is_multi_arm <- !is.null(W_multi)
#
#   # Ensure tau_hat is a matrix for multi-arm, vector for binary
#   if (is_multi_arm) {
#     tau_hat <- as.matrix(tau_hat)
#     treatment <- W_multi
#   } else {
#     tau_hat <- as.vector(tau_hat)
#     treatment <- as.factor(W)
#   }
#
#   # Compute IPW scores
#   IPW_scores <- maq::get_ipw_scores(Y, treatment)
#
#   # Set cost for each treatment arm or for binary treatment
#   if (is_multi_arm) {
#     cost <- rep(1, ncol(tau_hat))
#   } else {
#     cost <- 1
#   }
#
#   # Compute qini curve with covariates
#   ma_qini <- maq::maq(tau_hat, cost, IPW_scores, R = 200)
#
#   # Compute qini curve without covariates
#   ma_qini_baseline <- maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200)
#
#   # Create a list of all qini objects
#   qini_objects <- list(all_arms = ma_qini, baseline = ma_qini_baseline)
#
#   # Add individual arms for multi-arm treatment
#   if (is_multi_arm) {
#     for (i in 1:ncol(tau_hat)) {
#       qini_objects[[paste0("arm", i)]] <- maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200)
#     }
#   } else {
#     # For binary treatment, add the single treatment arm
#     qini_objects[["treatment_arm"]] <- ma_qini
#   }
#
#   # Determine the maximum index to extend all curves to
#   max_index <- max(sapply(qini_objects, function(qini_obj) length(qini_obj[["_path"]]$gain)))
#
#   # Extract qini data for plotting
#   qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index))
#
#   return(qini_data)
# }
