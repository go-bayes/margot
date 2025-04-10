#' Interpret Rank-Weighted Average Treatment Effect (RATE) estimates
#'
#' This function provides a concise interpretation of Rank-Weighted Average Treatment Effect (RATE)
#' estimates from \code{margot_rate()}. The RATE evaluates a prioritisation policy based on the
#' estimated conditional treatment effect, \eqn{\tau(x)}, which reflects the expected gain conditional
#' on measured covariates. For AUTOC the logarithmic weighting is most effective when heterogeneity
#' is concentrated in a small subset of individuals (typically ≤ 10%), whereas for QINI the linear
#' weighting is most effective when heterogeneity is broadly distributed (typically > 50%).
#'
#' @param rate_df A data frame from \code{margot_rate()} with columns:
#'   \code{RATE Estimate}, \code{2.5\%}, and \code{97.5\%}. The first column contains outcome names.
#'   Alternatively, a list containing both AUTOC and QINI results.
#' @param flipped_outcomes Character vector of outcome names that were inverted during pre-processing.
#' @param target Character string specifying RATE type: \code{AUTOC} (default) or \code{QINI}.
#'   Ignored if \code{rate_df} is a list containing both methods.
#'
#' @return A markdown string interpreting the RATE estimates.
#'
#' @examples
#' \donttest{
#' # single method:
#' interpretation <- margot_interpret_rate(rate_table, flipped_outcomes = c("t2_neuroticism_z"))
#' cat(interpretation)
#'
#' # both methods:
#' interpretation <- margot_interpret_rate(rate_table_all, flipped_outcomes = flipped_names)
#' cat(interpretation)
#' }
#'
#' @importFrom stats qnorm
#' @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  # if rate_df is a list of both methods, first check that both results are available;
  # if not, state that the results remain inconclusive.
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      (("rate_result" %in% names(rate_df) && "rate_qini" %in% names(rate_df)) ||
       ("rate_autoc" %in% names(rate_df) && "rate_qini" %in% names(rate_df)))) {

    if (!("rate_qini" %in% names(rate_df)) ||
        !(("rate_autoc" %in% names(rate_df)) || ("rate_result" %in% names(rate_df)))) {
      return("Results remain inconclusive for all evaluated models.")
    }

    # get the appropriate AUTOC data frame if available
    autoc_df <- if ("rate_autoc" %in% names(rate_df)) rate_df$rate_autoc else rate_df$rate_result
    qini_df <- rate_df$rate_qini

    # call the separate comparison function
    return(margot_interpret_rate_comparison(autoc_df, qini_df, flipped_outcomes))
  }

  # validate target
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

  # ensure flipped_outcomes is a character vector
  if (!is.null(flipped_outcomes)) {
    flipped_outcomes <- as.character(flipped_outcomes)
  }

  # determine significance by checking whether the 95% CI does not cross zero
  # note: a positive RATE estimate with CI entirely above zero and a negative RATE estimate with CI entirely below zero
  is_significant <- (rate_df[,"2.5%"] > 0) | (rate_df[,"97.5%"] < 0)
  pos_sig_idx <- which(rate_df[,"2.5%"] > 0)
  neg_sig_idx <- which(rate_df[,"97.5%"] < 0)

  interpretation_parts <- list(
    paste0("### Evidence for Treatment Prioritisation based on $\\tau(x)$ (", target, " Method)")
  )

  # explanation of the selected targeting method
  if (target == "AUTOC") {
    target_explanation <- paste0(
      "The RATE evaluates a prioritisation policy based on the estimated conditional treatment effect, $\\tau(x)$, ",
      "using logarithmic weighting. This approach is most effective when heterogeneity is concentrated in a ",
      "small subset of individuals (typically ≤ 10%)."
    )
  } else {
    target_explanation <- paste0(
      "The RATE evaluates a prioritisation policy based on the estimated conditional treatment effect, $\\tau(x)$, ",
      "using linear weighting. This approach is most effective when heterogeneity is broadly distributed across the ",
      "population (typically > 50%)."
    )
  }
  interpretation_parts[[length(interpretation_parts) + 1]] <- target_explanation

  # handle outcomes with positive RATE estimates
  if (length(pos_sig_idx) > 0) {
    outcomes <- outcome_names[pos_sig_idx]
    estimates <- sapply(pos_sig_idx, function(i) {
      # extract outcome details
      outcome <- outcome_names[i]
      rate_est <- rate_df[i, "RATE Estimate"]
      ci_lower <- rate_df[i, "2.5%"]
      ci_upper <- rate_df[i, "97.5%"]

      # check for flipped outcome
      is_flipped <- FALSE
      if (!is.null(flipped_outcomes)) {
        outcome_str <- gsub("\\*", "", as.character(outcome))
        flipped_clean <- gsub("\\*", "", flipped_outcomes)
        is_flipped <- outcome_str %in% flipped_clean
      }

      paste0(outcome, ": ", sprintf("%.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper),
             if (is_flipped) " (after inverting the outcome)" else "")
    })
    pos_section <- paste0(
      "If heterogeneity is concentrated in a small subset of individuals",
      if (target == "QINI") " or broadly distributed" else "",
      ", the following outcome(s) exhibit positive RATE estimates: ",
      paste(outcomes, collapse = ", "), ". ",
      "These estimates (", paste(estimates, collapse = "; "), ") suggest that a treatment prioritisation policy based on ",
      "predicted individual gains may result in outcomes that differ from those achieved by targeting using the ATE."
    )
    interpretation_parts[[length(interpretation_parts) + 1]] <- pos_section
  }

  # handle outcomes with negative RATE estimates
  if (length(neg_sig_idx) > 0) {
    outcomes <- outcome_names[neg_sig_idx]
    estimates <- sapply(neg_sig_idx, function(i) {
      outcome <- outcome_names[i]
      rate_est <- rate_df[i, "RATE Estimate"]
      ci_lower <- rate_df[i, "2.5%"]
      ci_upper <- rate_df[i, "97.5%"]

      is_flipped <- FALSE
      if (!is.null(flipped_outcomes)) {
        outcome_str <- gsub("\\*", "", as.character(outcome))
        flipped_clean <- gsub("\\*", "", flipped_outcomes)
        is_flipped <- outcome_str %in% flipped_clean
      }

      paste0(outcome, ": ", sprintf("%.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper),
             if (is_flipped) " (after inverting the outcome)" else "")
    })
    neg_section <- paste0(
      "For outcome(s) where the RATE estimates are negative (",
      paste(outcomes, collapse = ", "), "), the analysis provides strong evidence that ",
      "targeting treatment based on $\\tau(x)$ would yield worse outcomes compared to a policy based solely on the ATE. ",
      "Specific estimates (", paste(estimates, collapse = "; "), ") indicate that extra caution is warranted when considering a CATE-based approach."
    )
    interpretation_parts[[length(interpretation_parts) + 1]] <- neg_section
  }

  # handle cases where no significant outcomes were detected
  if (length(pos_sig_idx) == 0 && length(neg_sig_idx) == 0) {
    interpretation_parts[[length(interpretation_parts) + 1]] <-
      "No statistically significant evidence of treatment effect heterogeneity was detected for any outcome, indicating that the results remain inconclusive. Proceed with caution."
  } else if (sum(is_significant) < length(outcome_names)) {
    non_sig_idx <- which(!is_significant)
    non_sig_outcomes <- outcome_names[non_sig_idx]
    interpretation_parts[[length(interpretation_parts) + 1]] <- paste0(
      "For outcome(s) ", paste(non_sig_outcomes, collapse = ", "),
      ", the confidence intervals cross zero, rendering the evidence inconclusive."
    )
  }

  interpretation_text <- paste(interpretation_parts, collapse = "\n\n")
  return(interpretation_text)
}
