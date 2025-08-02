#' Compute Qini Curves for Multi-Arm Treatments
#'
#' This function computes Qini curves for multi-arm treatment effects using the maq package.
#' It handles various edge cases and provides detailed information about the computation process.
#'
#' @param tau_hat A matrix or 3D array of estimated treatment effects. For multi-arm treatments,
#'   this should be a matrix where each column represents a treatment arm.
#' @param Y A vector or matrix of observed outcomes.
#' @param W_multi A factor vector of treatment assignments for multi-arm treatments.
#' @param W.hat Optional vector specifying known treatment assignment probabilities for each arm.
#' @param cost Optional vector specifying the cost associated with each treatment arm.
#' @param verbose Logical indicating whether to display detailed messages during execution. Default is TRUE.
#'
#' @return A list containing:
#'   \item{qini_data}{A data frame containing Qini curve data for plotting.}
#'   \item{qini_objects}{A list of maq objects for each curve, which can be used to compute average gain.}
#' The qini_data has an attribute "imputed" which is TRUE if any curves were imputed with zeros.
#'
#' @details
#' The function computes Qini curves for all arms combined, a baseline (no covariates),
#' and each individual treatment arm. It handles cases where some or all Qini objects
#' have zero length or are NULL, extending curves with zeros when necessary.
#'
#' @importFrom maq maq get_ipw_scores
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_danger
#' @importFrom purrr map2_dfr
#'
#' @keywords internal
compute_qini_curves_multi_arm <- function(tau_hat, Y, W_multi, W.hat = NULL, cost = NULL, verbose = TRUE) {
  tryCatch(
    {
      # Ensure tau_hat is a matrix
      if (length(dim(tau_hat)) == 3) {
        tau_hat <- tau_hat[, , 1] # Take the first slice if it's a 3D array
      }
      tau_hat <- as.matrix(tau_hat)

      # Compute IPW scores
      if (is.null(W.hat)) {
        # If W.hat is not provided, assume uniform probabilities
        num_arms <- length(levels(W_multi))
        W.hat <- rep(1 / num_arms, num_arms)
        if (verbose) cli::cli_alert_info("Assuming uniform treatment assignment probabilities")
      }
      IPW_scores <- maq::get_ipw_scores(Y, W_multi, W.hat)

      # Ensure tau_hat has the same number of rows as IPW_scores
      if (nrow(tau_hat) != nrow(IPW_scores)) {
        if (verbose) cli::cli_alert_warning("Mismatch in number of rows between tau_hat and IPW_scores")
        return(NULL)
      }

      # Set cost
      if (is.null(cost)) {
        cost <- rep(1, ncol(tau_hat))
        if (verbose) cli::cli_alert_info("Assuming equal costs for all treatments")
      }
      # Ensure cost vector length matches number of treatment arms
      if (length(cost) != ncol(tau_hat)) {
        if (verbose) cli::cli_alert_warning("Length of cost vector does not match number of treatment arms")
        return(NULL)
      }

      # Compute qini curves
      if (verbose) cli::cli_alert_info("Computing Qini curves")
      qini_objects <- list()

      qini_objects[["all_arms"]] <- tryCatch(
        maq::maq(tau_hat, cost, IPW_scores, R = 200),
        error = function(e) {
          if (verbose) cli::cli_alert_warning(paste("Error computing all_arms Qini curve:", e$message))
          return(NULL)
        }
      )

      qini_objects[["baseline"]] <- tryCatch(
        maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200),
        error = function(e) {
          if (verbose) cli::cli_alert_warning(paste("Error computing baseline Qini curve:", e$message))
          return(NULL)
        }
      )

      for (i in 1:ncol(tau_hat)) {
        qini_objects[[paste0("arm", i)]] <- tryCatch(
          maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200),
          error = function(e) {
            if (verbose) cli::cli_alert_warning(paste("Error computing Qini curve for arm", i, ":", e$message))
            return(NULL)
          }
        )
      }

      # Determine the maximum index
      max_index <- max(sapply(qini_objects, function(qini_obj) {
        if (is.null(qini_obj) || is.null(qini_obj[["_path"]]) || is.null(qini_obj[["_path"]]$gain)) {
          return(0)
        }
        length(qini_obj[["_path"]]$gain)
      }))

      if (max_index == 0) {
        if (verbose) cli::cli_alert_warning("All Qini objects have empty gain. Extending with zeros.")
        max_index <- nrow(tau_hat) # Use the number of observations as max_index
        for (name in names(qini_objects)) {
          if (is.null(qini_objects[[name]])) {
            qini_objects[[name]] <- list("_path" = list(gain = rep(0, max_index)))
          } else if (length(qini_objects[[name]][["_path"]]$gain) == 0) {
            qini_objects[[name]][["_path"]]$gain <- rep(0, max_index)
          }
        }
      } else {
        # Handle case where some, but not all, Qini objects have zero length
        for (name in names(qini_objects)) {
          if (is.null(qini_objects[[name]]) || length(qini_objects[[name]][["_path"]]$gain) == 0) {
            if (verbose) cli::cli_alert_warning(paste("Extending Qini curve", name, "with zeros."))
            qini_objects[[name]] <- list("_path" = list(gain = rep(0, max_index)))
          }
        }
      }

      # Extract qini data for plotting
      qini_data <- purrr::map2_dfr(qini_objects, names(qini_objects), ~ extract_qini_data(.x, .y, max_index, verbose))

      if (nrow(qini_data) == 0) {
        if (verbose) cli::cli_alert_warning("Extracted Qini data is empty. Returning NULL.")
        return(NULL)
      }

      # Add a flag to indicate imputed data
      attr(qini_data, "imputed") <- any(sapply(qini_objects, function(obj) {
        is.null(obj) || length(obj[["_path"]]$gain) == 0 || all(obj[["_path"]]$gain == 0)
      }))

      if (attr(qini_data, "imputed")) {
        if (verbose) cli::cli_alert_warning("Some Qini curves were imputed with zeros. Exercise caution when interpreting results.")
      }

      # Return both qini_data and qini_objects
      return(list(qini_data = qini_data, qini_objects = qini_objects))
    },
    error = function(e) {
      if (verbose) cli::cli_alert_danger(paste("Error in compute_qini_curves_multi_arm:", e$message))
      return(NULL)
    }
  )
}





# compute_qini_curves_multi_arm <- function(tau_hat, Y, W_multi) {
#   tryCatch({
#     # Debug information
#     cli::cli_alert_info(paste("tau_hat class:", class(tau_hat)))
#     cli::cli_alert_info(paste("tau_hat dimensions:", paste(dim(tau_hat), collapse = "x")))
#     cli::cli_alert_info(paste("Y class:", class(Y)))
#     cli::cli_alert_info(paste("Y dimensions:", paste(dim(Y), collapse = "x")))
#     cli::cli_alert_info(paste("W_multi class:", class(W_multi)))
#     cli::cli_alert_info(paste("W_multi length:", length(W_multi)))
#
#     # Ensure tau_hat is a matrix
#     if (length(dim(tau_hat)) == 3) {
#       tau_hat <- tau_hat[,,1]  # Take the first slice if it's a 3D array
#     }
#     tau_hat <- as.matrix(tau_hat)
#     cli::cli_alert_info(paste("tau_hat dimensions after conversion:", paste(dim(tau_hat), collapse = "x")))
#
#     # Compute IPW scores
#     cli::cli_alert_info("Computing IPW scores")
#     IPW_scores <- maq::get_ipw_scores(Y, W_multi)
#     cli::cli_alert_info(paste("IPW_scores dimensions:", paste(dim(IPW_scores), collapse = "x")))
#
#     # Ensure tau_hat has the same number of rows as IPW_scores
#     if (nrow(tau_hat) != nrow(IPW_scores)) {
#       cli::cli_alert_warning("Mismatch in number of rows between tau_hat and IPW_scores")
#       return(NULL)
#     }
#
#     # Set cost
#     cost <- rep(1, ncol(tau_hat))
#     cli::cli_alert_info(paste("cost length:", length(cost)))
#
#     # Compute qini curves
#     cli::cli_alert_info("Computing Qini curves")
#     qini_objects <- list()
#
#     qini_objects[["all_arms"]] <- tryCatch(
#       maq::maq(tau_hat, cost, IPW_scores, R = 200),
#       error = function(e) {
#         cli::cli_alert_warning(paste("Error computing all_arms Qini curve:", e$message))
#         return(NULL)
#       }
#     )
#
#     qini_objects[["baseline"]] <- tryCatch(
#       maq::maq(tau_hat, cost, IPW_scores, target.with.covariates = FALSE, R = 200),
#       error = function(e) {
#         cli::cli_alert_warning(paste("Error computing baseline Qini curve:", e$message))
#         return(NULL)
#       }
#     )
#
#     for (i in 1:ncol(tau_hat)) {
#       qini_objects[[paste0("arm", i)]] <- tryCatch(
#         maq::maq(tau_hat[, i, drop = FALSE], cost[i], IPW_scores[, i, drop = FALSE], R = 200),
#         error = function(e) {
#           cli::cli_alert_warning(paste("Error computing Qini curve for arm", i, ":", e$message))
#           return(NULL)
#         }
#       )
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
#       cli::cli_alert_warning("All Qini objects have empty gain. Extending with zeros.")
#       max_index <- nrow(tau_hat)  # Use the number of observations as max_index
#       for (name in names(qini_objects)) {
#         if (is.null(qini_objects[[name]])) {
#           qini_objects[[name]] <- list("_path" = list(gain = rep(0, max_index)))
#         } else if (length(qini_objects[[name]][["_path"]]$gain) == 0) {
#           qini_objects[[name]][["_path"]]$gain <- rep(0, max_index)
#         }
#       }
#     } else {
#       # Handle case where some, but not all, Qini objects have zero length
#       for (name in names(qini_objects)) {
#         if (is.null(qini_objects[[name]]) || length(qini_objects[[name]][["_path"]]$gain) == 0) {
#           cli::cli_alert_warning(paste("Extending Qini curve", name, "with zeros."))
#           qini_objects[[name]] <- list("_path" = list(gain = rep(0, max_index)))
#         }
#       }
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
#     # Add a flag to indicate imputed data
#     attr(qini_data, "imputed") <- any(sapply(qini_objects, function(obj) {
#       is.null(obj) || length(obj[["_path"]]$gain) == 0 || all(obj[["_path"]]$gain == 0)
#     }))
#
#     if (attr(qini_data, "imputed")) {
#       cli::cli_alert_warning("Some Qini curves were imputed with zeros. Exercise caution when interpreting results.")
#     }
#
#     return(qini_data)
#   }, error = function(e) {
#     cli::cli_alert_danger(paste("Error in compute_qini_curves_multi_arm:", e$message))
#     return(NULL)
#   })
# }
