#' Adjust Weights for Censoring and Sample Design with Progress Reporting
#'
#' This function calculates and adjusts weights for censoring, combining them with
#' sample weights if provided. It also offers options for trimming and normalising
#' the resulting weights. Progress is reported using the cli package.
#'
#' @param pscore Numeric vector of predicted probabilities from a censoring model.
#'   Values must be between 0 and 1.
#' @param censoring_indicator Logical vector or 0/1 numeric vector indicating
#'   censoring status (TRUE/1 if censored, FALSE/0 if not).
#' @param sample_weights Optional numeric vector of sample weights.
#' @param trim Logical; whether to trim weights (default is TRUE).
#' @param normalize Logical; whether to normalise weights (default is TRUE).
#' @param lower_percentile Numeric; lower percentile for trimming (default is 0.01).
#' @param upper_percentile Numeric; upper percentile for trimming (default is 0.99).
#' @param na.rm Logical; whether to remove NA values (default is TRUE).
#'
#' @return A list containing adjusted weights and summary statistics.
#'
#' @import cli
#' @export
margot_adjust_weights <- function(pscore,
                                  censoring_indicator,
                                  sample_weights = NULL,
                                  trim = TRUE,
                                  normalize = TRUE,
                                  lower_percentile = 0.01,
                                  upper_percentile = 0.99,
                                  na.rm = TRUE) {
  cli::cli_h1("Adjusting Weights")

  # input validation
  cli::cli_alert_info("Validating inputs...")
  if (!is.numeric(pscore) || any(pscore < 0 | pscore > 1, na.rm = TRUE)) {
    cli::cli_alert_danger("Error: 'pscore' must be a numeric vector with values between 0 and 1")
    stop("Invalid pscore")
  }

  # convert censoring_indicator to logical if it's 0/1
  if (is.numeric(censoring_indicator)) {
    if (all(censoring_indicator %in% c(0, 1, NA))) {
      censoring_indicator <- as.logical(censoring_indicator)
    } else {
      cli::cli_alert_danger("Error: If numeric, 'censoring_indicator' must contain only 0, 1, or NA")
      stop("Invalid censoring_indicator")
    }
  } else if (!is.logical(censoring_indicator)) {
    cli::cli_alert_danger("Error: 'censoring_indicator' must be a logical vector or 0/1 numeric vector")
    stop("Invalid censoring_indicator")
  }

  if (length(pscore) != length(censoring_indicator)) {
    cli::cli_alert_danger("Error: 'pscore' and 'censoring_indicator' must have the same length")
    stop("Mismatched lengths")
  }

  if (!is.null(sample_weights) && !is.numeric(sample_weights)) {
    cli::cli_alert_danger("Error: 'sample_weights' must be a numeric vector or NULL")
    stop("Invalid sample_weights")
  }

  cli::cli_alert_success("Input validation complete")

  # calculate censoring weights
  cli::cli_alert_info("Calculating censoring weights...")
  # assign zero weight to censored individuals, and inverse of survival probability to uncensored
  censoring_weights <- ifelse(censoring_indicator, 0, 1 / (1 - pscore))
  cli::cli_alert_success("Censoring weights calculated")

  # combine weights if sample weights are provided
  if (!is.null(sample_weights)) {
    cli::cli_alert_info("Combining censoring weights with sample weights...")
    if (length(censoring_weights) != length(sample_weights)) {
      cli::cli_alert_danger("Error: 'censoring_weights' and 'sample_weights' must have the same length")
      stop("Mismatched lengths")
    }
    combined_weights <- censoring_weights * sample_weights
    cli::cli_alert_success("Weights combined")
  } else {
    combined_weights <- censoring_weights
    cli::cli_alert_info("No sample weights provided. Using censoring weights only.")
  }

  # remove NA values if specified
  if (na.rm) {
    cli::cli_alert_info("Removing NA values...")
    combined_weights <- combined_weights[!is.na(combined_weights)]
    cli::cli_alert_success("NA values removed")
  }

  # check if there are any non-NA weights left
  if (length(combined_weights) == 0) {
    cli::cli_alert_danger("Error: No non-NA weights remain after processing")
    stop("No valid weights")
  }

  # trimming
  if (trim) {
    cli::cli_alert_info("Trimming weights...")
    # exclude zero weights when calculating quantiles for trimming
    positive_weights <- combined_weights[combined_weights > 0]
    lower_bound <- quantile(positive_weights, lower_percentile, na.rm = na.rm)
    upper_bound <- quantile(positive_weights, upper_percentile, na.rm = na.rm)
    # trim weights, ensuring censored individuals remain at zero
    trimmed_weights <- combined_weights
    trimmed_weights[combined_weights > 0] <- pmin(pmax(positive_weights, lower_bound), upper_bound)
    cli::cli_alert_success("Weights trimmed")
  } else {
    trimmed_weights <- combined_weights
    cli::cli_alert_info("Skipping weight trimming")
  }

  # normalisation
  if (normalize) {
    cli::cli_alert_info("Normalising weights...")
    # normalise using the mean of non-zero weights
    mean_positive_weight <- mean(trimmed_weights[trimmed_weights > 0], na.rm = na.rm)
    normalized_weights <- trimmed_weights / mean_positive_weight
    cli::cli_alert_success("Weights normalised")
  } else {
    normalized_weights <- trimmed_weights
    cli::cli_alert_info("Skipping weight normalisation")
  }

  # compute summary statistics
  cli::cli_alert_info("Computing summary statistics...")
  summary_stats <- summary(normalized_weights[normalized_weights > 0])
  cli::cli_alert_success("Summary statistics computed")

  # print summary
  cli::cli_h2("Weight Adjustment Summary")
  cli::cli_alert_info(c(
    "Minimum weight: ", format(min(normalized_weights[normalized_weights > 0], na.rm = na.rm), digits = 4)
  ))
  cli::cli_alert_info(c(
    "Maximum weight: ", format(max(normalized_weights[normalized_weights > 0], na.rm = na.rm), digits = 4)
  ))
  cli::cli_alert_info(c(
    "Mean weight: ", format(mean(normalized_weights[normalized_weights > 0], na.rm = na.rm), digits = 4)
  ))
  cli::cli_alert_info(c(
    "Median weight: ", format(median(normalized_weights[normalized_weights > 0], na.rm = na.rm), digits = 4)
  ))

  # return results
  cli::cli_alert_success("Weight adjustment complete")
  return(list(
    adjusted_weights = normalized_weights,
    censoring_weights = censoring_weights,
    combined_weights = combined_weights,
    trimmed = trim,
    normalized = normalize,
    min_weight = min(normalized_weights[normalized_weights > 0], na.rm = na.rm),
    max_weight = max(normalized_weights[normalized_weights > 0], na.rm = na.rm),
    mean_weight = mean(normalized_weights[normalized_weights > 0], na.rm = na.rm),
    median_weight = median(normalized_weights[normalized_weights > 0], na.rm = na.rm),
    summary_stats = summary_stats
  ))
}
#old
# margot_adjust_weights <- function(pscore,
#                                   censoring_indicator,
#                                   sample_weights = NULL,
#                                   trim = TRUE,
#                                   normalize = TRUE,
#                                   lower_percentile = 0.01,
#                                   upper_percentile = 0.99,
#                                   na.rm = TRUE) {
#
#   cli::cli_h1("Adjusting Weights")
#
#   # Input validation
#   cli::cli_alert_info("Validating inputs...")
#   if (!is.numeric(pscore) || any(pscore < 0 | pscore > 1, na.rm = TRUE)) {
#     cli::cli_alert_danger("Error: 'pscore' must be a numeric vector with values between 0 and 1")
#     stop("Invalid pscore")
#   }
#
#   # convert censoring_indicator to logical if it's 0/1
#   if (is.numeric(censoring_indicator)) {
#     if (all(censoring_indicator %in% c(0, 1, NA))) {
#       censoring_indicator <- as.logical(censoring_indicator)
#     } else {
#       cli::cli_alert_danger("Error: If numeric, 'censoring_indicator' must contain only 0, 1, or NA")
#       stop("Invalid censoring_indicator")
#     }
#   } else if (!is.logical(censoring_indicator)) {
#     cli::cli_alert_danger("Error: 'censoring_indicator' must be a logical vector or 0/1 numeric vector")
#     stop("Invalid censoring_indicator")
#   }
#
#   if (length(pscore) != length(censoring_indicator)) {
#     cli::cli_alert_danger("Error: 'pscore' and 'censoring_indicator' must have the same length")
#     stop("Mismatched lengths")
#   }
#
#   if (!is.null(sample_weights) && !is.numeric(sample_weights)) {
#     cli::cli_alert_danger("Error: 'sample_weights' must be a numeric vector or NULL")
#     stop("Invalid sample_weights")
#   }
#
#   cli::cli_alert_success("Input validation complete")
#
#   # calculate censoring weights
#   cli::cli_alert_info("Calculating censoring weights...")
#   censoring_weights <- ifelse(censoring_indicator, 1 / pscore, 1 / (1 - pscore))
#   cli::cli_alert_success("Censoring weights calculated")
#
#   # combine weights if sample weights are provided
#   if (!is.null(sample_weights)) {
#     cli::cli_alert_info("Combining censoring weights with sample weights...")
#     if (length(censoring_weights) != length(sample_weights)) {
#       cli::cli_alert_danger("Error: 'censoring_weights' and 'sample_weights' must have the same length")
#       stop("Mismatched lengths")
#     }
#     combined_weights <- censoring_weights * sample_weights
#     cli::cli_alert_success("Weights combined")
#   } else {
#     combined_weights <- censoring_weights
#     cli::cli_alert_info("No sample weights provided. Using censoring weights only.")
#   }
#
#   # remove NA values if specified
#   if (na.rm) {
#     cli::cli_alert_info("Removing NA values...")
#     combined_weights <- combined_weights[!is.na(combined_weights)]
#     cli::cli_alert_success("NA values removed")
#   }
#
#   # check if there are any non-NA weights left
#   if (length(combined_weights) == 0) {
#     cli::cli_alert_danger("Error: No non-NA weights remain after processing")
#     stop("No valid weights")
#   }
#
#   # trimming
#   if (trim) {
#     cli::cli_alert_info("Trimming weights...")
#     lower_bound <- quantile(combined_weights, lower_percentile, na.rm = na.rm)
#     upper_bound <- quantile(combined_weights, upper_percentile, na.rm = na.rm)
#     trimmed_weights <- pmin(pmax(combined_weights, lower_bound), upper_bound)
#     cli::cli_alert_success("Weights trimmed")
#   } else {
#     trimmed_weights <- combined_weights
#     cli::cli_alert_info("Skipping weight trimming")
#   }
#
#   # normalisation
#   if (normalize) {
#     cli::cli_alert_info("Normalizing weights...")
#     normalized_weights <- trimmed_weights / mean(trimmed_weights, na.rm = na.rm)
#     cli::cli_alert_success("Weights normalized")
#   } else {
#     normalized_weights <- trimmed_weights
#     cli::cli_alert_info("Skipping weight normalization")
#   }
#
#   # compute summary statistics
#   cli::cli_alert_info("Computing summary statistics...")
#   summary_stats <- summary(normalized_weights)
#   cli::cli_alert_success("Summary statistics computed")
#
#   # print summary
#   cli::cli_h2("Weight Adjustment Summary")
#   cli::cli_alert_info(c(
#     "Minimum weight: ", format(min(normalized_weights, na.rm = na.rm), digits = 4)
#   ))
#   cli::cli_alert_info(c(
#     "Maximum weight: ", format(max(normalized_weights, na.rm = na.rm), digits = 4)
#   ))
#   cli::cli_alert_info(c(
#     "Mean weight: ", format(mean(normalized_weights, na.rm = na.rm), digits = 4)
#   ))
#   cli::cli_alert_info(c(
#     "Median weight: ", format(median(normalized_weights, na.rm = na.rm), digits = 4)
#   ))
#
#   # return results
#   cli::cli_alert_success("Weight adjustment complete")
#   return(list(
#     adjusted_weights = normalized_weights,
#     censoring_weights = censoring_weights,
#     combined_weights = combined_weights,
#     trimmed = trim,
#     normalized = normalize,
#     min_weight = min(normalized_weights, na.rm = na.rm),
#     max_weight = max(normalized_weights, na.rm = na.rm),
#     mean_weight = mean(normalized_weights, na.rm = na.rm),
#     median_weight = median(normalized_weights, na.rm = na.rm),
#     summary_stats = summary_stats
#   ))
# }
