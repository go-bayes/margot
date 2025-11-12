#' Compute IPSI counterfactual initiation probabilities
#'
#' Takes a formatted transition matrix (for example, one of the formatted tables
#' produced by `margot_transition_table()` such as
#' `transition_tables$tables_data[[i]]` or the paired `knitr_kable` stored under
#' `transition_tables$tables[[i]]`) and computes the natural initiation
#' probability for baseline non-attenders along with the counterfactual
#' probabilities implied by marginal incremental propensity score interventions
#' (IPSIs) for a set of proposed deltas.
#'
#' @param trans_matrix A data frame containing at least a `"From / To"` column,
#'   a `"State 1"` column, and a `"Total"` column. Must include a row identified
#'   as `"State 0"` under `"From / To"`, representing non-attenders at the prior
#'   wave. You can pass either the raw data frame or the `knitr_kable` object
#'   returned inside `margot_transition_table(... )$tables[[i]]` (the function
#'   automatically reads the attached `table_data` attribute).
#' @param deltas Numeric vector of IPSI shift magnitudes (greater than 1). The
#'   defaults match the standard IPSI reporting set `c(2, 5, 10)`.
#'
#' @return A data frame with one row per delta containing the natural initiation
#'   probability `p`, the counterfactual probability `p' = 1 - (1 - p) / delta`,
#'   and the fold increase `p'/p`. The result carries a `counts` attribute with
#'   the raw initiation and at-risk totals used to compute `p`.
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
#'
#' @export
margot_compute_ipsi_probability <- function(trans_matrix, deltas = c(2, 5, 10)) {
  if (inherits(trans_matrix, "knitr_kable")) {
    data_attr <- attr(trans_matrix, "table_data")
    if (is.null(data_attr)) {
      stop("`knitr_kable` inputs must carry a 'table_data' attribute. ",
           "Use margot_transition_table(... )$tables[[i]] or supply a data.frame.")
    }
    trans_matrix <- data_attr
  }

  if (!is.data.frame(trans_matrix)) stop("`trans_matrix` must be a data frame.")

  required_cols <- c("From / To", "State 1", "Total")
  missing_cols <- setdiff(required_cols, colnames(trans_matrix))
  if (length(missing_cols)) {
    stop("`trans_matrix` is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  rows_state0 <- trans_matrix[trimws(as.character(trans_matrix$`From / To`)) == "State 0", , drop = FALSE]
  if (!nrow(rows_state0)) stop("No 'State 0' row found in transition matrix.")

  initiations <- sum(as.numeric(rows_state0$`State 1`), na.rm = TRUE)
  non_attenders <- sum(as.numeric(rows_state0$Total), na.rm = TRUE)
  if (!is.finite(initiations) || initiations < 0) {
    stop("Initiation counts (State 1 column) must be non-negative numbers.")
  }
  if (!is.finite(non_attenders) || non_attenders <= 0) {
    stop("Total counts for 'State 0' must be positive numbers.")
  }
  if (initiations > non_attenders) {
    warning("Initiation count exceeds Total for 'State 0'; check the transition matrix.")
  }

  p <- initiations / non_attenders
  if (!is.finite(p) || p < 0) stop("Natural initiation probability is not well-defined.")
  if (p > 1) {
    warning("Natural initiation probability exceeds 1; resetting to 1 for downstream calculations.")
    p <- 1
  }

  deltas <- as.numeric(deltas)
  if (!length(deltas) || any(!is.finite(deltas))) stop("`deltas` must contain finite numeric values.")
  if (any(deltas <= 1)) stop("All `deltas` must be greater than 1.")

  p_prime <- 1 - (1 - p) / deltas
  # guard against numerical drift outside [0, 1]
  p_prime <- pmin(pmax(p_prime, 0), 1)

  fold <- if (p == 0) {
    warning("Natural initiation probability is zero; fold increase is undefined (set to NA).")
    rep(NA_real_, length(deltas))
  } else {
    p_prime / p
  }

  result <- data.frame(
    delta = deltas,
    delta_inverse = round(1 / deltas, 3),
    natural_p = round(rep(p, length(deltas)), 4),
    counterfactual_p = round(p_prime, 4),
    fold_increase = round(fold, 1),
    stringsAsFactors = FALSE
  )
  attr(result, "counts") <- list(
    initiations = initiations,
    non_attenders = non_attenders,
    natural_p = p
  )
  result
}
