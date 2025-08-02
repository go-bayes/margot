#' Compute RATE (Rank Average Treatment Effect) On-Demand
#'
#' This internal helper function computes RATE metrics (AUTOC or QINI) on-demand
#' from a causal forest and treatment effect estimates. It ensures proper
#' out-of-sample validation when possible.
#'
#' @param forest A causal_forest object from grf
#' @param tau_hat Optional vector of treatment effect estimates. If NULL, will be
#'   computed from the forest using out-of-bag predictions by default.
#' @param target Character; either "AUTOC" or "QINI"
#' @param q Numeric vector of quantiles at which to evaluate the TOC. Default is
#'   seq(0.1, 1, by = 0.1) which matches the GRF default.
#' @param policy Character; either "treat_best" (default) or "withhold_best"
#' @param subset Optional indices for subsetting the evaluation data
#' @param use_oob_predictions Logical; if TRUE and tau_hat is NULL, use out-of-bag
#'   predictions for better validity (default TRUE)
#' @param verbose Logical; print informative messages (default FALSE)
#' @param seed Random seed for reproducibility (default 12345)
#' @param ... Additional arguments passed to grf::rank_average_treatment_effect()
#'
#' @return A rank_average_treatment_effect object from grf
#'
#' @details
#' For valid statistical performance, the prioritization scores (tau_hat) should
#' be constructed independently from the evaluation forest training data. This
#' function attempts to ensure this by:
#' - Using out-of-bag predictions when tau_hat is not provided
#' - Supporting subset indices for proper train/test splitting
#' - Warning when validation might be compromised
#'
#' @keywords internal
#' @importFrom grf rank_average_treatment_effect
#' @importFrom cli cli_alert_info cli_alert_warning
compute_rate_on_demand <- function(forest,
                                   tau_hat = NULL,
                                   target = c("AUTOC", "QINI"),
                                   q = seq(0.1, 1, by = 0.1),
                                   policy = c("treat_best", "withhold_best"),
                                   subset = NULL,
                                   use_oob_predictions = TRUE,
                                   verbose = FALSE,
                                   seed = 12345,
                                   ...) {
  # validate inputs
  if (!inherits(forest, "causal_forest")) {
    stop("forest must be a causal_forest object from grf")
  }

  target <- match.arg(target)
  policy <- match.arg(policy)

  # set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # determine tau_hat if not provided
  if (is.null(tau_hat)) {
    if (use_oob_predictions) {
      # use out-of-bag predictions for better validity
      if (verbose) {
        cli::cli_alert_info("Computing tau_hat using out-of-bag predictions")
      }
      tau_hat <- predict(forest)$predictions
    } else {
      # use in-sample predictions (less valid but sometimes necessary)
      if (verbose) {
        cli::cli_alert_warning("Computing tau_hat using in-sample predictions - results may be optimistic")
      }
      tau_hat <- predict(forest, forest$X.orig)$predictions
    }
  }

  # handle subsetting if provided
  if (!is.null(subset)) {
    if (verbose) {
      cli::cli_alert_info("Using subset of {length(subset)} observations for RATE computation")
    }

    # validate subset indices
    max_idx <- length(tau_hat)
    if (any(subset > max_idx) || any(subset < 1)) {
      stop("subset contains invalid indices")
    }

    tau_hat <- tau_hat[subset]

    # note: grf::rank_average_treatment_effect will use the subset parameter
    # to properly subset the forest's training data
  }

  # handle policy direction
  if (policy == "withhold_best") {
    tau_hat <- -tau_hat
    if (verbose) {
      cli::cli_alert_info("Flipping tau_hat for withhold_best policy")
    }
  }

  # add small epsilon to break ties (as done in margot_rate_batch)
  eps <- 1e-12
  tau_adj <- tau_hat + eps * seq_along(tau_hat)

  # compute RATE
  if (verbose) {
    cli::cli_alert_info("Computing {target} with q grid of length {length(q)}")
  }

  rate_result <- grf::rank_average_treatment_effect(
    forest,
    priorities = tau_adj,
    target = target,
    q = q,
    subset = subset,
    ...
  )

  # add metadata to help with interpretation
  attr(rate_result, "policy") <- policy
  attr(rate_result, "target") <- target
  attr(rate_result, "q") <- q
  attr(rate_result, "n_eval") <- length(tau_hat)
  attr(rate_result, "used_oob") <- is.null(tau_hat) && use_oob_predictions

  return(rate_result)
}
