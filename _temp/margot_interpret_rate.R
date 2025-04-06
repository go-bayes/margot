#' Interpret RATE estimates in markdown
#'
#' This function provides a concise interpretation of Rank-Weighted Average Treatment Effect (RATE)
#' estimates from \code{margot_rate()}. The RATE ranks individuals by predicted treatment effects
#' and computes average effects for selected percentiles.
#'
#' @param rate_df A data frame from \code{margot_rate()} with columns:
#'   \code{RATE Estimate}, \code{2.5\%}, and \code{97.5\%}. First column contains outcome names.
#' @param flipped_outcomes Character vector of outcome names that were inverted during pre-processing.
#' @param target Character string specifying RATE type: \code{AUTOC} (default) or \code{QINI}.
#'
#' @return A markdown string interpreting statistically reliable RATE estimates.
#'
#' @examples
#' \dontrun{
#' interpretation <- margot_interpret_rate(rate_table, flipped_outcomes = c("t2_neuroticism_z"))
#' cat(interpretation)
#' }
#'
#' @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  # validate target parameter
  if (!target %in% c("AUTOC", "QINI")) {
    stop("target must be either 'AUTOC' or 'QINI'.")
  }

  # check for required columns
  required_cols <- c("RATE Estimate", "2.5%", "97.5%")
  if (!all(required_cols %in% colnames(rate_df))) {
    stop("rate_df must contain columns 'RATE Estimate', '2.5%', and '97.5%'.")
  }

  # outcome names are assumed to be in the first column
  outcome_names <- rate_df[[1]]

  # ensure flipped_outcomes is always a character vector
  if (!is.null(flipped_outcomes)) {
    if (!is.vector(flipped_outcomes)) {
      flipped_outcomes <- c(as.character(flipped_outcomes))
    } else {
      flipped_outcomes <- as.character(flipped_outcomes)
    }
  }

  # determine significance: reliable if the 95% CI doesn't include zero
  is_significant <- (rate_df[,"2.5%"] > 0) | (rate_df[,"97.5%"] < 0)
  sig_idx <- which(is_significant)

  # create the main heading
  interpretation_parts <- list("### Evidence for Heterogeneous Treatment Effects")

  # brief explanation of RATE based on selected target
  if (target == "AUTOC") {
    target_explanation <- paste0(
      "The Rank-Weighted Average Treatment Effect (RATE) identifies subgroups of individuals ",
      "with different responses to treatment. We used the AUTOC targeting method, which is ",
      "appropriate when heterogeneity is concentrated in a smaller subset of the population."
    )
  } else { # QINI
    target_explanation <- paste0(
      "The Rank-Weighted Average Treatment Effect (RATE) identifies subgroups of individuals ",
      "with different responses to treatment. We used the QINI targeting method, which is ",
      "appropriate when heterogeneity is broadly distributed across the population."
    )
  }
  interpretation_parts[[length(interpretation_parts) + 1]] <- target_explanation

  # check if any outcomes are significant
  if (length(sig_idx) > 0) {
    # loop over significant outcomes
    for (i in sig_idx) {
      outcome <- outcome_names[i]
      rate_est <- rate_df[i, "RATE Estimate"]
      ci_lower <- rate_df[i, "2.5%"]
      ci_upper <- rate_df[i, "97.5%"]

      # check if outcome was flipped
      is_flipped <- FALSE
      if (!is.null(flipped_outcomes)) {
        # force both sides to be character for comparison and strip any asterisks
        outcome_str <- gsub("\\*", "", as.character(outcome))
        flipped_clean <- gsub("\\*", "", flipped_outcomes)
        is_flipped <- outcome_str %in% flipped_clean
        # for debugging:
        # cat("checking if", outcome_str, "is in", paste(flipped_clean, collapse=", "), ":", is_flipped, "\n")
      }

      # determine if effect is positive based on RATE estimate and flipped status
      beneficial <- if (is_flipped) rate_est < 0 else rate_est > 0

      header <- paste0("#### ", outcome)
      estimate_text <- sprintf("Estimated RATE: %.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper)

      interpretation_text <- "This result indicates reliable treatment effect heterogeneity"
      if (beneficial) {
        if (is_flipped) {
          interpretation_text <- paste0(interpretation_text, " with positive effects for targeted subgroups (after inverting the outcome).")
        } else {
          interpretation_text <- paste0(interpretation_text, " with positive effects for targeted subgroups.")
        }
      } else {
        if (is_flipped) {
          interpretation_text <- paste0(interpretation_text, " with negative effects for targeted subgroups (after inverting the outcome).")
        } else {
          interpretation_text <- paste0(interpretation_text, " with negative effects for targeted subgroups.")
        }
      }

      outcome_interpretation <- paste(header, estimate_text, interpretation_text, sep = "\n\n")
      interpretation_parts[[length(interpretation_parts) + 1]] <- outcome_interpretation
    }
  } else {
    # no significant outcomes
    no_sig_text <- "No statistically significant evidence of treatment effect heterogeneity was detected for any outcome (all 95% confidence intervals crossed zero)."
    interpretation_parts[[length(interpretation_parts) + 1]] <- no_sig_text
    return(paste(interpretation_parts, collapse = "\n\n"))
  }

  # add summary for non-significant outcomes (only if there were some significant ones)
  if (sum(is_significant) < length(outcome_names)) {
    non_sig_text <- "For the remaining outcomes, no statistically significant evidence of treatment effect heterogeneity was detected (95% confidence intervals crossed zero)."
    interpretation_parts[[length(interpretation_parts) + 1]] <- non_sig_text
  }

  interpretation_text <- paste(interpretation_parts, collapse = "\n\n")
  return(interpretation_text)
}
