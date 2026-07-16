#' Compute IPSI counterfactual transition probabilities
#'
#' Takes a formatted transition matrix (for example, one of the formatted tables
#' produced by `margot_transition_table()` such as
#' `transition_tables$tables_data[[i]]` or the paired `knitr_kable` stored under
#' `transition_tables$tables[[i]]`) and computes the natural probability of
#' moving to the target state along with the counterfactual probabilities
#' implied by incremental propensity score interventions (IPSIs) for a set of
#' proposed deltas.
#'
#' @details
#' The delta here matches `lmtp::ipsi()`, which implements the risk-ratio
#' incremental propensity score intervention: with probability \eqn{1 - 1/\delta}
#' the intervention sets the exposure to the target state, and otherwise leaves
#' the natural value. The counterfactual probability of the target state is
#' therefore \eqn{p' = 1 - (1 - p)/\delta}: the natural probability of *not*
#' reaching the target state is divided by \eqn{\delta}. This is not the
#' odds-multiplier IPSI of Kennedy (2019), where \eqn{\delta} multiplies the
#' odds of exposure; describe registered estimands accordingly.
#'
#' Direction selects the transition of interest. With `direction = "up"` (the
#' default, matching earlier versions) the at-risk cohort is the `"State 0"`
#' row and the event is arrival in `"State 1"` (initiation). With
#' `direction = "down"` the at-risk cohort is the `"State 1"` row and the event
#' is arrival in `"State 0"` — the natural reading for a contingency that
#' raises the probability of the lower state among the upper-state cohort.
#'
#' @param trans_matrix A data frame containing at least a `"From / To"` column,
#'   the two state columns (`"State 0"`, `"State 1"`), and a `"Total"` column.
#'   You can pass either the raw data frame or the `knitr_kable` object
#'   returned inside `margot_transition_table(... )$tables[[i]]` (the function
#'   automatically reads the attached `table_data` attribute).
#' @param deltas Numeric vector of IPSI shift magnitudes (greater than 1),
#'   interpreted as in `lmtp::ipsi()` (risk ratio on the probability of not
#'   reaching the target state). The defaults match the standard IPSI
#'   reporting set `c(2, 5, 10)`.
#' @param direction Either `"up"` (State 0 cohort moving to State 1; the
#'   default) or `"down"` (State 1 cohort moving to State 0).
#'
#' @return A data frame with one row per delta containing the direction, the
#'   natural transition probability `p`, its exact 95% Clopper-Pearson
#'   confidence limits, the counterfactual probability
#'   `p' = 1 - (1 - p) / delta`, and the fold increase `p'/p`. The result
#'   carries a `counts` attribute with the raw event and at-risk totals
#'   (`events`, `at_risk`; the legacy names `initiations` and `non_attenders`
#'   are kept as aliases) and the natural-probability interval used to compute
#'   `p`.
#' @examples
#' trans <- data.frame(
#'   `From / To` = c("State 0", "State 1"),
#'   `State 0` = c(27380, 330),
#'   `State 1` = c(845, 170),
#'   Total = c(28225, 500),
#'   check.names = FALSE
#' )
#' margot_compute_ipsi_probability(trans)
#' margot_compute_ipsi_probability(trans, deltas = c(1.5, 3))
#' margot_compute_ipsi_probability(trans, direction = "down")
#'
#' @export
margot_compute_ipsi_probability <- function(trans_matrix,
                                            deltas = c(2, 5, 10),
                                            direction = c("up", "down")) {
  direction <- match.arg(direction)
  if (inherits(trans_matrix, "knitr_kable")) {
    data_attr <- attr(trans_matrix, "table_data")
    if (is.null(data_attr)) {
      stop("`knitr_kable` inputs must carry a 'table_data' attribute. ",
           "Use margot_transition_table(... )$tables[[i]] or supply a data.frame.")
    }
    trans_matrix <- data_attr
  }

  if (!is.data.frame(trans_matrix)) stop("`trans_matrix` must be a data frame.")

  # the at-risk row is the origin state; the event column is the target state
  from_state <- if (direction == "up") "State 0" else "State 1"
  to_state <- if (direction == "up") "State 1" else "State 0"

  required_cols <- c("From / To", to_state, "Total")
  missing_cols <- setdiff(required_cols, colnames(trans_matrix))
  if (length(missing_cols)) {
    stop("`trans_matrix` is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  rows_from <- trans_matrix[trimws(as.character(trans_matrix$`From / To`)) == from_state, , drop = FALSE]
  if (!nrow(rows_from)) stop("No '", from_state, "' row found in transition matrix.")

  events <- sum(as.numeric(rows_from[[to_state]]), na.rm = TRUE)
  at_risk <- sum(as.numeric(rows_from$Total), na.rm = TRUE)
  if (!is.finite(events) || events < 0) {
    stop("Event counts (", to_state, " column) must be non-negative numbers.")
  }
  if (!is.finite(at_risk) || at_risk <= 0) {
    stop("Total counts for '", from_state, "' must be positive numbers.")
  }
  if (events > at_risk) {
    warning("Event count exceeds Total for '", from_state, "'; check the transition matrix.")
  }

  p <- events / at_risk
  ci <- stats::binom.test(events, at_risk)$conf.int
  if (!is.finite(p) || p < 0) stop("Natural transition probability is not well-defined.")
  if (p > 1) {
    warning("Natural transition probability exceeds 1; resetting to 1 for downstream calculations.")
    p <- 1
  }

  deltas <- as.numeric(deltas)
  if (!length(deltas) || any(!is.finite(deltas))) stop("`deltas` must contain finite numeric values.")
  if (any(deltas <= 1)) stop("All `deltas` must be greater than 1.")

  # lmtp::ipsi() risk-ratio draw: the natural probability of not reaching the
  # target state is divided by delta
  p_prime <- 1 - (1 - p) / deltas
  # guard against numerical drift outside [0, 1]
  p_prime <- pmin(pmax(p_prime, 0), 1)

  fold <- if (p == 0) {
    warning("Natural transition probability is zero; fold increase is undefined (set to NA).")
    rep(NA_real_, length(deltas))
  } else {
    p_prime / p
  }

  result <- data.frame(
    direction = rep(direction, length(deltas)),
    delta = deltas,
    delta_inverse = round(1 / deltas, 3),
    natural_p = rep(p, length(deltas)),
    natural_p_l = rep(ci[1], length(deltas)),
    natural_p_u = rep(ci[2], length(deltas)),
    counterfactual_p = p_prime,
    fold_increase = fold,
    stringsAsFactors = FALSE
  )
  attr(result, "counts") <- list(
    direction = direction,
    events = events,
    at_risk = at_risk,
    # legacy aliases retained for backward compatibility
    initiations = events,
    non_attenders = at_risk,
    natural_p = p,
    natural_p_l = ci[1],
    natural_p_u = ci[2]
  )
  result
}
