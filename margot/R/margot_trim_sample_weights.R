#' Standardise and (optionally) trim sample weights at both ends
#'
#' This function first trims any weights below a specified lower‐quantile threshold
#' and/or above a specified upper‐quantile threshold, then standardises the
#' (possibly trimmed) weights to have a mean of 1.  Missing values are preserved.
#'
#' @param weights_vec Numeric vector of sample weights.  Must be positive or NA.
#' @param lower_quantile Numeric in (0,1); all weights below this quantile will
#'   be raised (“winsorised up”) to the lower quantile value.  If \code{NULL} or \code{<= 0},
#'   lower‐end trimming is skipped.
#' @param upper_quantile Numeric in (0,1); all weights above this quantile will
#'   be lowered (“winsorised down”) to the upper quantile value.  If \code{NULL} or \code{>= 1},
#'   upper‐end trimming is skipped.
#'
#' @return A numeric vector the same length as \code{weights_vec}, with extremes
#'   winsorised and then rescaled to have mean 1.
#'
#' @details
#' Trimming both tails of inverse-probability weights can mitigate the influence
#' of implausibly small or large weights, trading a bit of bias for lower variance.
#'
#' @examples
#' set.seed(42)
#' w <- c(rlnorm(90, 0, 0.5), runif(5, 5, 20), runif(5, 0, 0.01), NA)
#' summary(w)
#'
#' # trim both lower 1% and upper 99%, then standardise
#' w_both <- margot_trim_sample_weights(
#'   w,
#'   lower_quantile = 0.01,
#'   upper_quantile = 0.99
#' )
#' summary(w_both)
#'
#' # only upper trim at 95th percentile
#' w_up95 <- margot_trim_sample_weights(w, lower_quantile = NULL, upper_quantile = 0.95)
#' summary(w_up95)
#'
#' # only lower trim at 5th percentile
#' w_low5 <- margot_trim_sample_weights(w, lower_quantile = 0.05, upper_quantile = NULL)
#' summary(w_low5)
#'
#' # no trimming (both NULL), only standardise
#' w_std <- margot_trim_sample_weights(w, lower_quantile = NULL, upper_quantile = NULL)
#' summary(w_std)
#'
#' @export
margot_trim_sample_weights <- function(weights_vec,
                                       lower_quantile = NULL,
                                       upper_quantile = 0.99) {
  # input validation
  if (!is.numeric(weights_vec)) {
    stop("`weights_vec` must be a numeric vector.")
  }
  if (any(weights_vec <= 0, na.rm = TRUE)) {
    stop("`weights_vec` contains non‐positive values; weights must be positive or NA.")
  }

  trimmed <- weights_vec

  # lower‐end trimming
  if (!is.null(lower_quantile) && is.numeric(lower_quantile)) {
    if (lower_quantile > 0 && lower_quantile < 1) {
      low_thr <- stats::quantile(weights_vec, probs = lower_quantile, na.rm = TRUE, type = 7)
      trimmed <- pmax(trimmed, low_thr)
      n_low <- sum(weights_vec < low_thr, na.rm = TRUE)
      if (n_low > 0) {
        message(sprintf(
          "Raised %d weight(s) below the %.1f%% quantile (threshold = %g).",
          n_low, lower_quantile * 100, low_thr
        ))
      }
    } else if (lower_quantile >= 1) {
      warning("`lower_quantile >= 1` — skipping lower trimming.")
    } else if (lower_quantile <= 0) {
      warning("`lower_quantile <= 0` — skipping lower trimming.")
    }
  }

  # upper‐end trimming
  if (!is.null(upper_quantile) && is.numeric(upper_quantile)) {
    if (upper_quantile > 0 && upper_quantile < 1) {
      up_thr <- stats::quantile(weights_vec, probs = upper_quantile, na.rm = TRUE, type = 7)
      trimmed <- pmin(trimmed, up_thr)
      n_up <- sum(weights_vec > up_thr, na.rm = TRUE)
      if (n_up > 0) {
        message(sprintf(
          "Lowered %d weight(s) above the %.1f%% quantile (threshold = %g).",
          n_up, upper_quantile * 100, up_thr
        ))
      }
    } else if (upper_quantile <= 0) {
      warning("`upper_quantile <= 0` — skipping upper trimming.")
    } else if (upper_quantile >= 1) {
      warning("`upper_quantile >= 1` — skipping upper trimming.")
    }
  }

  # standardisation to mean = 1
  m <- mean(trimmed, na.rm = TRUE)
  if (is.na(m) || m == 0) {
    stop("Mean of (trimmed) weights is zero or NA; cannot standardise.")
  }
  trimmed / m
}
