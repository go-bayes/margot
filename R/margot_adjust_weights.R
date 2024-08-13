#' Adjust Weights for Censoring and Sample Design
#'
#' This function calculates and adjusts weights for censoring, combining them with
#' sample weights and/or gender weights if provided. It also offers options for
#' trimming and normalizing the resulting weights.
#'
#' @param pscore Numeric vector of predicted probabilities from a censoring model.
#'   Values must be between 0 and 1.
#' @param censoring_indicator Logical vector or 0/1 numeric vector indicating
#'   censoring status (TRUE/1 if censored, FALSE/0 if not).
#' @param sample_weights Optional numeric vector of sample weights.
#' @param previous_weights Optional numeric vector of weights from previous time points.
#' @param trim Logical; whether to trim weights (default is TRUE).
#' @param normalize Logical; whether to normalize weights (default is TRUE).
#' @param lower_percentile Numeric; lower percentile for trimming (default is 0.01).
#' @param upper_percentile Numeric; upper percentile for trimming (default is 0.99).
#' @param na.rm Logical; whether to remove NA values (default is TRUE).
#'
#' @return A list containing:
#'   \item{adjusted_weights}{Numeric vector of final adjusted weights}
#'   \item{censoring_weights}{Numeric vector of calculated censoring weights}
#'   \item{combined_weights}{Numeric vector of combined weights before adjustment}
#'   \item{trimmed}{Logical; whether trimming was performed}
#'   \item{normalized}{Logical; whether normalization was performed}
#'   \item{min_weight}{Minimum adjusted weight}
#'   \item{max_weight}{Maximum adjusted weight}
#'   \item{mean_weight}{Mean of adjusted weights}
#'   \item{median_weight}{Median of adjusted weights}
#'   \item{summary_stats}{Summary statistics of adjusted weights}
#'
#' @examples
#' pscore <- runif(100, 0, 1)
#' censoring <- sample(c(0, 1), 100, replace = TRUE)
#' sample_weights <- runif(100, 0.5, 1.5)
#'
#' result <- margot_adjust_weights(pscore, censoring, sample_weights)
#' hist(result$adjusted_weights)
#'
#' @export
margot_adjust_weights <- function(pscore,
                                  censoring_indicator,
                                  sample_weights = NULL,
                                  trim = TRUE,
                                  normalize = TRUE,
                                  lower_percentile = 0.01,
                                  upper_percentile = 0.99,
                                  na.rm = TRUE) {

  # Input validation
  if (!is.numeric(pscore) || any(pscore < 0 | pscore > 1, na.rm = TRUE)) {
    stop("'pscore' must be a numeric vector with values between 0 and 1")
  }

  # Convert censoring_indicator to logical if it's 0/1
  if (is.numeric(censoring_indicator)) {
    if (all(censoring_indicator %in% c(0, 1, NA))) {
      censoring_indicator <- as.logical(censoring_indicator)
    } else {
      stop("If numeric, 'censoring_indicator' must contain only 0, 1, or NA")
    }
  } else if (!is.logical(censoring_indicator)) {
    stop("'censoring_indicator' must be a logical vector or 0/1 numeric vector")
  }

  if (length(pscore) != length(censoring_indicator)) {
    stop("'pscore' and 'censoring_indicator' must have the same length")
  }
  if (!is.null(sample_weights) && !is.numeric(sample_weights)) {
    stop("'sample_weights' must be a numeric vector or NULL")
  }
  if (!is.logical(trim) || !is.logical(normalize) || !is.logical(na.rm)) {
    stop("'trim', 'normalize', and 'na.rm' must be logical")
  }
  if (!is.numeric(lower_percentile) || !is.numeric(upper_percentile) ||
      lower_percentile < 0 || lower_percentile > 1 ||
      upper_percentile < 0 || upper_percentile > 1 ||
      lower_percentile >= upper_percentile) {
    stop("Invalid percentile values")
  }

  # Calculate censoring weights
  censoring_weights <- ifelse(censoring_indicator, 1 / pscore, 1 / (1 - pscore))

  # Combine weights if sample weights are provided
  if (!is.null(sample_weights)) {
    if (length(censoring_weights) != length(sample_weights)) {
      stop("'censoring_weights' and 'sample_weights' must have the same length")
    }
    combined_weights <- censoring_weights * sample_weights
  } else {
    combined_weights <- censoring_weights
  }

  # Remove NA values if specified
  if (na.rm) {
    combined_weights <- combined_weights[!is.na(combined_weights)]
  }

  # Check if there are any non-NA weights left
  if (length(combined_weights) == 0) {
    stop("No non-NA weights remain after processing")
  }

  # Trimming
  if (trim) {
    lower_bound <- quantile(combined_weights, lower_percentile, na.rm = na.rm)
    upper_bound <- quantile(combined_weights, upper_percentile, na.rm = na.rm)
    trimmed_weights <- pmin(pmax(combined_weights, lower_bound), upper_bound)
  } else {
    trimmed_weights <- combined_weights
  }

  # Normalization
  if (normalize) {
    normalized_weights <- trimmed_weights / mean(trimmed_weights, na.rm = na.rm)
  } else {
    normalized_weights <- trimmed_weights
  }

  # Compute summary statistics
  summary_stats <- summary(normalized_weights)

  # Return results
  return(list(
    adjusted_weights = normalized_weights,
    censoring_weights = censoring_weights,
    combined_weights = combined_weights,
    trimmed = trim,
    normalized = normalize,
    min_weight = min(normalized_weights, na.rm = na.rm),
    max_weight = max(normalized_weights, na.rm = na.rm),
    mean_weight = mean(normalized_weights, na.rm = na.rm),
    median_weight = median(normalized_weights, na.rm = na.rm),
    summary_stats = summary_stats
  ))
}
