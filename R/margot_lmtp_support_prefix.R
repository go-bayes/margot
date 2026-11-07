#' Assess and propagate one discrete exposure-support prefix
#'
#' Evaluate the current conditional moment and, after a pass, optionally carry
#' realised exposure ratios to the next assignment.
#'
#' @inheritParams margot_lmtp_support_concentration
#' @param observed Named character vector of observed discrete values or region
#'   labels, matching the probability-matrix column names on reached rows.
#' @param excluded Character vector of regions excluded by the policy by
#'   definition. Their policy mass must be zero on reached rows.
#' @param propagate Whether to calculate realised ratios and next weights after
#'   a pass. Set FALSE at the last assigned wave when no propagation is needed.
#' @return A list containing state, reason, concentration, diagnostics,
#'   realised_ratio and next_weight, with the supplied probability laws and
#'   observed categories. Invalid inputs return a reason and empty results.
#'   Failures after concentration retain the assessment and its diagnostics.
#' @details A required zero natural mass fails the conditional-moment check.
#'   During propagation, an observed category with zero natural mass is allowed
#'   only when it is explicitly excluded by the policy. Otherwise propagation
#'   is unassessable. Rows with zero prior weight stay zero without division.
#'   With propagate = FALSE, callers must still report other_zero_mass in
#'   diagnostics, even if the conditional-moment assessment passes.
#'   This function does not choose or shorten a study's assignment schedule.
#' @examples
#' ids <- c("a", "b")
#' g <- matrix(c(.2, .8, .2, .8), 2, byrow = TRUE,
#'   dimnames = list(ids, c("zero", "positive")))
#' q <- g
#' q[, ] <- rep(c(1, 0), each = 2)
#' margot_lmtp_support_prefix(g, q, setNames(c("zero", "positive"), ids),
#'   setNames(c(1, 1), ids), setNames(c(TRUE, TRUE), ids), ids,
#'   minimum = .05, excluded = "positive")
#' @export
margot_lmtp_support_prefix <- function(natural_mass, policy_mass, observed,
                                     preceding_weight, reached, row_id, minimum,
                                     observation_weight = NULL,
                                     excluded = character(), propagate = TRUE) {
  unassessable <- function(reason) {
    list(state = "unassessable", reason = reason, concentration = NULL,
         realised_ratio = NULL, next_weight = NULL)
  }
  if (!is.character(row_id) || !length(row_id) || anyNA(row_id) ||
      any(!nzchar(row_id)) || anyDuplicated(row_id) ||
      !is.matrix(natural_mass) || !is.numeric(natural_mass) ||
      !is.matrix(policy_mass) || !is.numeric(policy_mass) ||
      !identical(dim(natural_mass), dim(policy_mass)) ||
      nrow(natural_mass) != length(row_id) ||
      !identical(rownames(natural_mass), row_id) ||
      !identical(rownames(policy_mass), row_id) ||
      is.null(colnames(natural_mass)) ||
      !identical(colnames(natural_mass), colnames(policy_mass)) ||
      !is.character(observed) || length(observed) != length(row_id) ||
      !identical(names(observed), row_id) ||
      !is.numeric(preceding_weight) || length(preceding_weight) != length(row_id) ||
      !identical(names(preceding_weight), row_id) ||
      !is.logical(reached) || length(reached) != length(row_id) || anyNA(reached) ||
      !identical(names(reached), row_id) ||
      !is.character(excluded) || anyNA(excluded) || anyDuplicated(excluded) ||
      !all(excluded %in% colnames(natural_mass)) ||
      !is.logical(propagate) || length(propagate) != 1L || is.na(propagate)) {
    return(unassessable("Invalid inputs or ordered identities."))
  }
  if (anyNA(observed[reached]) || !all(observed[reached] %in% colnames(natural_mass))) {
    return(unassessable("Reached observations must name a supplied probability category."))
  }
  if (any(!is.finite(policy_mass[reached, excluded, drop = FALSE])) ||
      any(policy_mass[reached, excluded, drop = FALSE] != 0)) {
    return(unassessable("Policy-excluded categories must have zero policy mass."))
  }
  assessment <- margot_lmtp_support_concentration(natural_mass, policy_mass,
    preceding_weight, reached, row_id, minimum, observation_weight)
  if (!assessment$state %in% c("pass", "fail", "boundary_fail")) {
    return(unassessable(paste("Concentration assessment is unassessable.",
      assessment$reason)))
  }
  active <- reached & preceding_weight > 0
  rows <- which(active)
  cell <- cbind(rows, match(observed[active], colnames(natural_mass)))
  is_excluded <- observed[active] %in% excluded
  zero_mass <- natural_mass[cell] == 0
  diagnostics <- list(boundary_records = assessment$boundary_records,
    sole_failures = assessment$sole_failures,
    excluded_zero_mass = sum(zero_mass & is_excluded),
    other_zero_mass = sum(zero_mass & !is_excluded))
  result <- list(state = assessment$state, reason = assessment$reason,
    concentration = assessment, diagnostics = diagnostics,
    realised_ratio = NULL, next_weight = NULL,
    natural_mass = natural_mass, policy_mass = policy_mass, observed = observed)
  if (assessment$state != "pass" || !propagate) return(result)
  if (any(zero_mass & !is_excluded)) {
    result$state <- "unassessable"
    result$reason <- "Observed exposure has zero natural probability."
    return(result)
  }
  ratio <- next_weight <- setNames(rep(NA_real_, length(row_id)), row_id)
  value <- numeric(length(rows))
  value[!is_excluded] <- policy_mass[cell][!is_excluded] / natural_mass[cell][!is_excluded]
  ratio[active] <- value
  if (any(!is.finite(ratio[active]))) {
    result$state <- "unassessable"
    result$reason <- "A required realised exposure ratio is nonfinite."
    return(result)
  }
  next_weight[reached & !active] <- 0
  next_weight[active] <- preceding_weight[active] * ratio[active]
  if (any(!is.finite(next_weight[reached])) ||
      any(next_weight[active] == 0 & ratio[active] > 0)) {
    result$state <- "unassessable"
    result$reason <- "Cumulative exposure weights are nonfinite or underflowed."
    return(result)
  }
  result$realised_ratio <- ratio
  result$next_weight <- next_weight
  result
}
