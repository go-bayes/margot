#' Report the calibration of predicted observation probabilities
#'
#' `margot_lmtp_observation_report()` summarises, for each wave, how well predicted probabilities of remaining observed describe who was in fact observed among the records that reached that wave. Each column of the three matrices is one wave and each row one participant. For wave \eqn{j}, let \eqn{R_j} be the records with `reached[, j]` true, \eqn{y_i} the indicator `observed[i, j]` and \eqn{p_i} the predicted probability `probability[i, j]`. The report gives the number reached, the number that continued (\eqn{\sum_{i \in R_j} y_i}), the Brier score \eqn{|R_j|^{-1} \sum_{i \in R_j} (p_i - y_i)^2} and the log loss \eqn{|R_j|^{-1} \sum_{i \in R_j} -\{y_i \log p_i + (1 - y_i) \log(1 - p_i)\}}, computed as `mean(-ifelse(y == 1, log(p), log1p(-p)))`. Records that did not reach a wave do not enter it.
#'
#' Probabilities are not clipped. A probability of exactly 0 or 1 is valid: when it matches the observed indicator the record contributes 0 to the log loss, and when it contradicts it (a record observed with predicted probability 0, or not observed with predicted probability 1) the record's loss, and so the wave's log loss, is `Inf`. The column `n_infinite_log_loss` counts those records, so an infinite log loss can be traced to a confident wrong prediction rather than mistaken for a numerical failure. A wave that no record reached has zero counts and `NA` scores.
#'
#' The report describes the observation model; it is descriptive and attaches no positivity or estimator-stability judgement. Weight concentration is reported separately by [margot_lmtp_analysis_weight_report()].
#'
#' @param reached Logical matrix, one row per participant and one column per wave, true where a record reached that wave and was at risk of being lost to observation. Missing values are not allowed.
#' @param observed Logical matrix of the same dimensions, true where a record that reached the wave remained observed. It must be non-missing wherever `reached` is true and may not be true where `reached` is false; its values elsewhere are ignored. Convert a 0/1 censoring indicator with `observed == 1`.
#' @param probability Numeric matrix of the same dimensions holding the predicted probability of remaining observed. It must lie between 0 and 1 inclusive and be non-missing wherever `reached` is true; its values elsewhere are ignored.
#' @param wave_labels Optional labels, one per column. Column names of `reached` are used when available, then `Wave 1`, `Wave 2`, and so on.
#' @return A data frame of class `margot_lmtp_observation_report` with one row per wave and columns `wave`, `reached`, `continued`, `brier`, `log_loss` and `n_infinite_log_loss`. It contains no participant-level values.
#' @examples
#' reached <- cbind(rep(TRUE, 6), c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
#' probability <- matrix(c(0.9, 0.8, 0.7, 0.6, 0.9, 0.5, 0.8, 0.7, 0.6, 0.5, 0.9, 0.5), ncol = 2)
#' observed <- cbind(reached[, 2], c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
#' margot_lmtp_observation_report(reached, observed, probability, wave_labels = c("2019", "2020"))
#' @seealso [margot_lmtp_analysis_weight_report()], [margot_lmtp_censoring_report()]
#' @md
#' @export
margot_lmtp_observation_report <- function(reached, observed, probability, wave_labels = NULL) {
  if (!is.matrix(reached) || !is.logical(reached)) {
    stop("`reached` must be a logical matrix.", call. = FALSE)
  }
  if (!is.matrix(observed) || !is.logical(observed)) {
    stop("`observed` must be a logical matrix; convert a 0/1 indicator with `observed == 1`.", call. = FALSE)
  }
  if (!is.matrix(probability) || !is.numeric(probability)) {
    stop("`probability` must be a numeric matrix.", call. = FALSE)
  }
  if (!nrow(reached) || !ncol(reached)) {
    stop("`reached` must have at least one row and one column.", call. = FALSE)
  }
  if (!identical(dim(observed), dim(reached)) || !identical(dim(probability), dim(reached))) {
    stop("`reached`, `observed` and `probability` must have the same dimensions.", call. = FALSE)
  }
  if (anyNA(reached)) stop("`reached` must not contain missing values.", call. = FALSE)
  if (anyNA(observed[reached])) {
    stop("`observed` must not be missing where `reached` is true.", call. = FALSE)
  }
  if (any(observed[!reached] %in% TRUE)) {
    stop("`observed` must not be true where `reached` is false.", call. = FALSE)
  }
  p_reached <- probability[reached]
  if (anyNA(p_reached)) {
    stop("`probability` must not be missing where `reached` is true.", call. = FALSE)
  }
  if (any(p_reached < 0 | p_reached > 1)) {
    stop("`probability` must lie in [0, 1] where `reached` is true.", call. = FALSE)
  }
  waves <- ncol(reached)
  labels <- wave_labels %||% colnames(reached)
  if (is.null(labels)) labels <- paste0("Wave ", seq_len(waves))
  labels <- as.character(labels)
  if (length(labels) != waves || anyNA(labels) || anyDuplicated(labels)) {
    stop("`wave_labels` must uniquely label every column.", call. = FALSE)
  }

  rows <- lapply(seq_len(waves), function(j) {
    at_risk <- reached[, j]
    y <- as.numeric(observed[at_risk, j])
    p <- as.numeric(probability[at_risk, j])
    loss <- -ifelse(y == 1, log(p), log1p(-p))
    data.frame(wave = labels[[j]], reached = sum(at_risk), continued = sum(observed[at_risk, j]),
      brier = if (length(y)) mean((p - y)^2) else NA_real_,
      log_loss = if (length(y)) mean(loss) else NA_real_,
      n_infinite_log_loss = sum(is.infinite(loss)), stringsAsFactors = FALSE)
  })
  report <- do.call(rbind, rows)
  rownames(report) <- NULL
  class(report) <- c("margot_lmtp_observation_report", "data.frame")
  report
}
