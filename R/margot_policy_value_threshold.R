#' Resolve a benefit threshold from development action scores
#'
#' @description Resolves a fixed signed threshold or the weighted development-sample average treatment effect reference. The result is a hypothetical outcome-scale benefit threshold, not a measured economic cost.
#' @param dr_scores Numeric matrix of finite, original (unweighted) binary action scores, on a common outcome scale where larger scores are preferred. Recognisable control and treatment column names are required unless both column indices are supplied.
#' @param weights Optional finite non-negative development weights, aligned with rows and with positive total weight. Weights define the averaging population and are applied once.
#' @param value_threshold A finite numeric scalar (default zero, preserving the original objective), or \code{"ate"} to use the weighted mean treatment-minus-control action-score contrast in these development rows.
#' @param threshold_multiplier Finite non-negative scalar multiplying the fixed threshold or development ATE; default one. Multipliers are relative references, not guaranteed extreme ranges.
#' @param treatment_column,control_column Optional distinct integer column indices identifying the two actions. Supply both together.
#' @return A \code{margot_policy_value_threshold} list with resolved \code{value}, \code{source}, \code{multiplier}, \code{development_ate}, development row and weight counts, and action-column indices. Preserve this object unchanged when evaluating the associated learned rule on separate observations.
#' @details Subtract the resolved value from the original treated action score before applying the analysis weight. Apply this same objective to every comparator. Original treatment effects retain their signs: an effect below an ATE reference need not be harmful. A zero or negative ATE is a relative reference and cannot automatically represent a positive treatment expense. This helper does not establish causal identification or independence of the supplied scores; callers must keep nuisance fitting, threshold estimation and rule learning within their declared development boundary.
#' @export
margot_policy_value_threshold <- function(dr_scores, weights = NULL,
                                          value_threshold = 0,
                                          threshold_multiplier = 1,
                                          treatment_column = NULL,
                                          control_column = NULL) {
  # resolve the reference from original development scores and their target weights.
  .policy_value_validate_spec(value_threshold, threshold_multiplier)
  dr_scores <- as.matrix(dr_scores)
  if (!is.numeric(dr_scores) || ncol(dr_scores) != 2L || !nrow(dr_scores) ||
      any(!is.finite(dr_scores))) {
    stop("dr_scores must be a non-empty finite numeric matrix with two action columns", call. = FALSE)
  }
  weights <- .policy_value_weights(weights, nrow(dr_scores))
  if (is.null(treatment_column) && is.null(control_column)) {
    columns <- .margot_policy_binary_action_columns(dr_scores, context = "margot_policy_value_threshold()")
    treatment_column <- columns$treatment
    control_column <- columns$control
  } else {
    valid_column <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && x %in% 1:2
    if (!valid_column(treatment_column) || !valid_column(control_column) || treatment_column == control_column) {
      stop("supply distinct treatment_column and control_column indices in 1:2", call. = FALSE)
    }
  }
  difference <- dr_scores[, treatment_column] - dr_scores[, control_column]
  development_ate <- stats::weighted.mean(difference, weights)
  source <- if (is.character(value_threshold)) "ate" else "fixed"
  value <- threshold_multiplier * if (source == "ate") development_ate else value_threshold
  if (!is.finite(value) || !is.finite(development_ate)) {
    stop("resolved threshold and development ATE must be finite", call. = FALSE)
  }
  structure(list(value = unname(value), source = source, multiplier = threshold_multiplier,
                 development_ate = unname(development_ate), n_development = nrow(dr_scores),
                 development_weight_sum = sum(weights), treatment_column = as.integer(treatment_column),
                 control_column = as.integer(control_column)), class = "margot_policy_value_threshold")
}

.policy_value_validate_spec <- function(value_threshold, threshold_multiplier) {
  # validate the threshold specification before any fitting or row selection.
  valid_fixed <- is.numeric(value_threshold) && length(value_threshold) == 1L && is.finite(value_threshold)
  if (!valid_fixed && !identical(value_threshold, "ate")) {
    stop("value_threshold must be a finite numeric scalar or 'ate'", call. = FALSE)
  }
  if (!is.numeric(threshold_multiplier) || length(threshold_multiplier) != 1L ||
      !is.finite(threshold_multiplier) || threshold_multiplier < 0) {
    stop("threshold_multiplier must be a finite non-negative numeric scalar", call. = FALSE)
  }
  invisible(NULL)
}

.policy_value_weights <- function(weights, n) {
  # validate target weights and make an unweighted target explicit.
  if (is.null(weights)) return(rep(1, n))
  if (!is.numeric(weights) || length(weights) != n || any(!is.finite(weights)) ||
      any(weights < 0) || !is.finite(sum(weights)) || sum(weights) <= 0) {
    stop("weights must be finite, non-negative, aligned and have a positive finite sum", call. = FALSE)
  }
  as.numeric(weights)
}

.policy_value_net_scores <- function(dr_scores, threshold, treatment_column = NULL) {
  # subtract the saved reference from the treated raw reward without weighting.
  dr_scores <- as.matrix(dr_scores)
  if (inherits(threshold, "margot_policy_value_threshold")) {
    treatment_column <- treatment_column %||% threshold$treatment_column
    threshold <- threshold$value
  }
  .policy_value_validate_spec(threshold, 1)
  if (!is.numeric(threshold)) stop("threshold must be resolved before score adjustment", call. = FALSE)
  if (is.null(treatment_column)) {
    treatment_column <- .margot_policy_binary_action_columns(dr_scores)$treatment
  }
  if (!is.numeric(treatment_column) || length(treatment_column) != 1L ||
      !is.finite(treatment_column) || !treatment_column %in% seq_len(ncol(dr_scores))) {
    stop("treatment_column must identify one action-score column", call. = FALSE)
  }
  dr_scores[, treatment_column] <- dr_scores[, treatment_column] - threshold
  dr_scores
}

.policy_value_constant_tolerance <- function(dr_scores, weights = NULL) {
  # bound rounding in centred weighted action means on the original score scale.
  weights <- .policy_value_weights(weights, nrow(dr_scores))
  scale <- max(vapply(seq_len(ncol(dr_scores)), function(j) {
    stats::weighted.mean(abs(dr_scores[, j]), weights)
  }, numeric(1)))
  64 * .Machine$double.eps * max(scale, .Machine$double.xmin)
}
