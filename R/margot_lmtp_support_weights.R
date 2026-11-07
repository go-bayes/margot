#' Normalised inverse-observation weights for support assessment
#'
#' Construct the averaging measure at one exposure position from the
#' conditional probabilities of its preceding observation transitions.
#'
#' @param probability Numeric matrix of conditional observation probabilities,
#'   with named rows equal to row_id and one named column per required
#'   preceding transition. Values outside reached are ignored.
#' @param reached Named logical vector identifying records observed at the
#'   current exposure.
#' @param row_id Unique nonempty character identifiers in input order.
#' @return A list with state, normalised weight, cumulative
#'   log_inverse_observation, observation_effective_n, and reached count n.
#'   Unreached vector entries are NA. Invalid probabilities, identities, an
#'   empty risk set or numerical underflow return state unassessable
#'   with a reason.
#' @details Products are accumulated in log space and rescaled by one common
#'   constant. No probability floor or weight cap is applied. All reached
#'   records enter normalisation, regardless of their exposure ratios.
#' @examples
#' ids <- c("a", "b")
#' p <- matrix(c(1, .25), dimnames = list(ids, "observation"))
#' margot_lmtp_support_weights(p, setNames(c(TRUE, TRUE), ids), ids)
#' @export
margot_lmtp_support_weights <- function(probability, reached, row_id) {
  unavailable <- function(reason) list(state = "unassessable", reason = reason)
  if (!is.character(row_id) || !length(row_id) || anyNA(row_id) ||
      any(!nzchar(row_id)) || anyDuplicated(row_id) ||
      !is.matrix(probability) || !is.numeric(probability) ||
      nrow(probability) != length(row_id) || ncol(probability) < 1L ||
      !identical(rownames(probability), row_id) ||
      is.null(colnames(probability)) || anyNA(colnames(probability)) ||
      any(!nzchar(colnames(probability))) || anyDuplicated(colnames(probability)) ||
      !is.logical(reached) || length(reached) != length(row_id) ||
      !identical(names(reached), row_id) || anyNA(reached)) {
    return(unavailable("Invalid inputs or ordered identities."))
  }
  if (!any(reached)) return(unavailable("Empty exposure risk set."))
  log_inverse <- rep(0, sum(reached))
  for (node in seq_len(ncol(probability))) {
    p <- probability[reached, node]
    if (any(!is.finite(p)) || any(p <= 0 | p > 1)) {
      return(unavailable("Required observation probability is zero or invalid."))
    }
    log_inverse <- log_inverse - log(p)
  }
  # rescaling preserves normalised weights without capping extremes.
  b <- exp(log_inverse - max(log_inverse))
  b <- b / sum(b)
  if (any(!is.finite(b)) || any(b <= 0)) {
    return(unavailable("Normalised IPC weights underflowed or are invalid."))
  }
  weight <- log_weight <- setNames(rep(NA_real_, length(row_id)), row_id)
  weight[reached] <- b
  log_weight[reached] <- log_inverse
  list(state = "assessable", weight = weight, log_inverse_observation = log_weight,
    observation_effective_n = 1 / sum(b^2), n = sum(reached))
}
