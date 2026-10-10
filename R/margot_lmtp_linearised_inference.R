# purpose: read one property of the `ife` estimate that `lmtp` returns. inputs:
# the estimate, a property name and the argument name; output: the property.
.margot_ife_value <- function(estimate, property, name) {
  value <- attr(estimate, property, exact = TRUE)
  if (is.null(value) || length(value) == 0L) {
    stop("`", name, "` carries no `", property, "` property; supply an `lmtp` fit ",
      "or the `ife` estimate it returns.", call. = FALSE)
  }
  value
}

# purpose: extract the uncentred pseudo-outcome, weights and identifiers from an
# `lmtp` fit or its `ife` estimate. inputs: the object and its argument name;
# output: a list.
.margot_lmtp_inference_parts <- function(x, name) {
  estimate <- if (is.list(x) && !is.null(x$estimate)) x$estimate else x
  parts <- list(
    estimate = as.numeric(.margot_ife_value(estimate, "x", name)),
    eif = as.numeric(.margot_ife_value(estimate, "eif", name)),
    weights = as.numeric(.margot_ife_value(estimate, "weights", name)),
    id = as.character(.margot_ife_value(estimate, "id", name)),
    critical_value = as.numeric(.margot_ife_value(estimate, "critical_value", name)),
    package_std_error = as.numeric(.margot_ife_value(estimate, "std_error", name))
  )
  n <- length(parts$eif)
  if (length(parts$estimate) != 1L || !is.finite(parts$estimate)) {
    stop("`", name, "` must carry one finite estimate.", call. = FALSE)
  }
  if (n < 2L || any(!is.finite(parts$eif))) {
    stop("`", name, "` must carry at least two finite pseudo-outcome values.", call. = FALSE)
  }
  if (length(parts$weights) != n || any(!is.finite(parts$weights)) ||
      any(parts$weights < 0) || sum(parts$weights) <= 0) {
    stop("`", name, "` must carry one finite, non-negative weight per pseudo-outcome, ",
      "with a positive total.", call. = FALSE)
  }
  if (length(parts$id) != n) {
    stop("`", name, "` must carry one identifier per pseudo-outcome.", call. = FALSE)
  }
  parts
}

#' Linearised inference for weighted LMTP policy means and additive contrasts
#'
#' `margot_lmtp_linearised_inference()` recomputes the standard error of a
#' weighted policy-specific mean, or of an additive contrast between two policy
#' means fitted to the same participants, from the pseudo-outcomes and weights
#' stored in `lmtp` fits. It uses the linearisation of the weighted (Hájek)
#' mean,
#' \deqn{\hat\theta = \sum_i w_i \varphi_i / \sum_i w_i, \qquad
#'   \widehat{\mathrm{se}}(\hat\theta) = \{ s^2(u) / n \}^{1/2}, \qquad
#'   u_i = w_i(\varphi_i - \hat\theta)/\bar w,}
#' where \eqn{\hat\theta} is the estimate stored in the fit (for the SDR
#' estimator, the weighted mean shown), \eqn{\varphi_i} is participant
#' \eqn{i}'s uncentred pseudo-outcome,
#' \eqn{w_i} the baseline weight, \eqn{\bar w} the mean weight and \eqn{s^2}
#' the sample variance. For a contrast, \eqn{\varphi_i} is the difference of
#' the two pseudo-outcomes. With repeated identifiers, the contributions are
#' summed within clusters and the cluster-robust form used by `ife` is applied.
#'
#' `lmtp` 1.5.4 with `ife` 0.2.5 computes \eqn{s^2(w_i\varphi_i)/n}, which
#' centres the weighted product rather than the pseudo-outcome. The two agree
#' when the weights are constant and differ when they vary. The difference can
#' be large for policy means on a scale far from zero and depends on the
#' contrast's size for additive contrasts. Point estimates are unchanged. The
#' function fits no model and does not account for estimation of the weights.
#'
#' @param fit An `lmtp` fit, or the `ife` estimate stored in its `estimate`
#'   element.
#' @param ref Optional reference fit of the same form. When supplied, the
#'   function reports the additive contrast `fit` minus `ref`. Both fits must
#'   carry identical weights and identifiers.
#' @param conf_level Confidence level for the two-sided Wald interval. Defaults
#'   to the level implied by the critical value stored in `fit`.
#'
#' @return A one-row data frame with the estimate, the linearised standard
#'   error, interval limits, a two-sided p-value against zero (contrasts only),
#'   the standard error reported by `lmtp`, the number of participants and
#'   clusters, and the variance method.
#' @export
margot_lmtp_linearised_inference <- function(fit, ref = NULL, conf_level = NULL) {
  a <- .margot_lmtp_inference_parts(fit, "fit")
  contrast <- !is.null(ref)
  if (contrast) {
    b <- .margot_lmtp_inference_parts(ref, "ref")
    if (length(b$eif) != length(a$eif) || !identical(a$weights, b$weights) ||
        !identical(a$id, b$id)) {
      stop("`fit` and `ref` must carry identical weights and identifiers in the same order.",
        call. = FALSE)
    }
    phi <- a$eif - b$eif
    package_estimate <- a$estimate - b$estimate
  } else {
    phi <- a$eif
    package_estimate <- a$estimate
  }
  w <- a$weights
  n <- length(phi)
  # the stored estimate is the centre: for sdr it equals the weighted mean of the
  # pseudo-outcomes; for tmle it is the targeted plug-in.
  estimate <- package_estimate
  contribution <- w * (phi - estimate) / mean(w)
  n_clusters <- length(unique(a$id))
  std_error <- if (n_clusters == n || n_clusters == 1L) {
    sqrt(stats::var(contribution) / n)
  } else {
    cluster_sums <- as.numeric(rowsum(contribution, a$id, reorder = FALSE))
    sqrt(stats::var(cluster_sums * n_clusters / n) / n_clusters)
  }
  critical_value <- if (is.null(conf_level)) {
    a$critical_value
  } else {
    if (!is.numeric(conf_level) || length(conf_level) != 1L ||
        !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
      stop("`conf_level` must be one number in (0, 1).", call. = FALSE)
    }
    stats::qnorm(1 - (1 - conf_level) / 2)
  }
  package_std_error <- if (contrast) {
    weighted <- w * phi
    if (n_clusters == n || n_clusters == 1L) {
      sqrt(stats::var(weighted) / n)
    } else {
      sums <- as.numeric(rowsum(weighted, a$id, reorder = FALSE))
      sqrt(stats::var(sums * n_clusters / n) / n_clusters)
    }
  } else {
    a$package_std_error
  }
  data.frame(
    target = if (contrast) "additive contrast" else "policy mean",
    estimate = estimate,
    std_error = std_error,
    conf_low = estimate - critical_value * std_error,
    conf_high = estimate + critical_value * std_error,
    p_value = if (contrast) 2 * stats::pnorm(-abs(estimate / std_error)) else NA_real_,
    package_std_error = package_std_error,
    n = n,
    n_clusters = n_clusters,
    variance = "linearised weighted (Hajek) mean",
    stringsAsFactors = FALSE
  )
}
