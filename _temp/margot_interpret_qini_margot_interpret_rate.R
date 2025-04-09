#' This function provides a concise interpretation of Rank-Weighted Average Treatment Effect (RATE)
#' estimates from \code{margot_rate()}. The RATE ranks individuals by predicted treatment effects
#' and computes average effects for selected percentiles.
#'
#' @param rate_df A data frame from \code{margot_rate()} with columns:
#'   \code{RATE Estimate}, \code{2.5\%}, and \code{97.5\%}. First column contains outcome names.
#'   Alternatively, a list containing both AUTOC and QINI results.
#' @param flipped_outcomes Character vector of outcome names that were inverted during pre-processing.
#' @param target Character string specifying RATE type: \code{AUTOC} (default) or \code{QINI}.
#'   Ignored if rate_df is a list containing both methods.
#'
#' @return A markdown string interpreting statistically reliable RATE estimates.
#'
#' @examples
#' \dontrun{
#' # For a single method:
#' interpretation <- margot_interpret_rate(rate_table, flipped_outcomes = c("t2_neuroticism_z"))
#' cat(interpretation)
#'
#' # For both methods:
#' interpretation <- margot_interpret_rate(rate_table_all, flipped_outcomes = flipped_names)
#' cat(interpretation)
#' }
#'
#' @importFrom stats qnorm
#' @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  # Check if rate_df is a list containing both methods
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      (("rate_result" %in% names(rate_df) && "rate_qini" %in% names(rate_df)) ||
       ("rate_autoc" %in% names(rate_df) && "rate_qini" %in% names(rate_df)))) {

    # Get the correct AUTOC data frame (either rate_result or rate_autoc)
    autoc_df <- if ("rate_autoc" %in% names(rate_df)) rate_df$rate_autoc else rate_df$rate_result
    qini_df <- rate_df$rate_qini

    # Call the comparison function instead
    return(margot_interpret_rate_comparison(autoc_df, qini_df, flipped_outcomes))
  }

  # Original function for single method continues here
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

  # determine significance: significant if the 95% CI doesn't include zero
  is_significant <- (rate_df[,"2.5%"] > 0) | (rate_df[,"97.5%"] < 0)
  sig_idx <- which(is_significant)

  # separate positive and negative significant effects
  pos_sig_idx <- which(rate_df[,"2.5%"] > 0)
  neg_sig_idx <- which(rate_df[,"97.5%"] < 0)

  # create the main heading with the method specified
  interpretation_parts <- list(paste0("### Evidence for Heterogeneous Treatment Effects (", target, " Method)"))

  # brief explanation of RATE based on selected target
  if (target == "AUTOC") {
    target_explanation <- paste0(
      "The rank-weighted average treatment effect (RATE) identifies subgroups of individuals ",
      "with different responses to treatment. We used the AUTOC (Area Under the TOC curve) targeting method, ",
      "which uses logarithmic weighting and is most effective when heterogeneity is concentrated in a smaller ",
      "subset of the population (typically ≤ 10%). AUTOC provides better statistical power when only a ",
      "small fraction of individuals experiences non-zero heterogeneous treatment effects."
    )
  } else { # QINI
    target_explanation <- paste0(
      "The rank-weighted average treatment effect (RATE) identifies subgroups of individuals ",
      "with different responses to treatment. We used the QINI targeting method, which uses linear weighting ",
      "and is most effective when heterogeneity is broadly distributed across a substantial portion of the population ",
      "(typically > 50%). The Qini coefficient measures cumulative benefits as we increase the treatment fraction ",
      "and provides better statistical power when treatment effects are diffuse across the population."
    )
  }
  interpretation_parts[[length(interpretation_parts) + 1]] <- target_explanation

  # handle positive outcomes as a group
  if (length(pos_sig_idx) > 0) {
    pos_outcome_names <- outcome_names[pos_sig_idx]

    # create a subsection for positive outcomes
    pos_header <- paste0("#### Evidence for Prioritising using CATE (", target, ")")

    # list the outcomes with positive effects
    pos_outcomes_list <- paste(pos_outcome_names, collapse = ", ")

    pos_interpretation <- paste0(
      "For the following outcome(s): ", pos_outcomes_list, ", the analysis shows reliably positive RATE estimates ",
      "(95% confidence intervals entirely above zero). This indicates that targeting treatment ",
      "based on predicted effects would lead to better outcomes than using the average treatment effect (ATE). ",
      "For these outcomes, targeting the CATE is expected to lead to reliably better outcomes and ",
      "is recommended."
    )

    # Add estimates for each positive outcome
    pos_estimates <- paste0("Specific Estimates (", target, "):")
    for (i in pos_sig_idx) {
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

      flip_note <- if (is_flipped) " (after inverting the outcome)" else ""
      pos_estimates <- paste0(
        pos_estimates, "\n- ", outcome, ": ",
        sprintf("%.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper), flip_note
      )
    }

    pos_section <- paste(pos_header, pos_interpretation, pos_estimates, sep = "\n\n")
    interpretation_parts[[length(interpretation_parts) + 1]] <- pos_section
  }

  # handle reliably negative outcomes as a group with a caution
  if (length(neg_sig_idx) > 0) {
    neg_outcome_names <- outcome_names[neg_sig_idx]

    # create a subsection for negative outcomes
    neg_header <- paste0("#### Caution Against Prioritising using CATE (", target, ")")

    # list the outcomes with negative effects
    neg_outcomes_list <- paste(neg_outcome_names, collapse = ", ")

    neg_interpretation <- paste0(
      "For the following outcome(s): ", neg_outcomes_list, ", the analysis shows reliably negative RATE estimates ",
      "(95% confidence intervals entirely below zero). This is an important caution that targeting treatment ",
      "based on predicted effects would lead to worse outcomes than using the average treatment effect (ATE). ",
      "For these outcomes, targeting the CATE is expected to lead to reliably worse outcomes and ",
      "is not recommended."
    )

    # Add estimates for each negative outcome
    neg_estimates <- paste0("Specific Estimates (", target, "):")
    for (i in neg_sig_idx) {
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

      flip_note <- if (is_flipped) " (after inverting the outcome)" else ""
      neg_estimates <- paste0(
        neg_estimates, "\n- ", outcome, ": ",
        sprintf("%.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper), flip_note
      )
    }

    neg_section <- paste(neg_header, neg_interpretation, neg_estimates, sep = "\n\n")
    interpretation_parts[[length(interpretation_parts) + 1]] <- neg_section
  }

  # check if any outcomes are significant (either positive or negative)
  if (length(sig_idx) == 0) {
    # no significant outcomes at all
    no_sig_text <- "No statistically significant evidence of treatment effect heterogeneity was detected for any outcome (all 95% confidence intervals crossed zero)."
    interpretation_parts[[length(interpretation_parts) + 1]] <- no_sig_text
  } else if (sum(is_significant) < length(outcome_names)) {
    # some outcomes are not significant
    non_sig_idx <- which(!is_significant)
    non_sig_outcomes <- outcome_names[non_sig_idx]

    if (length(non_sig_outcomes) > 0) {
      non_sig_text <- paste0(
        "For the remaining outcome(s): ", paste(non_sig_outcomes, collapse = ", "),
        ", no statistically significant evidence of treatment effect heterogeneity was detected ",
        "(95% confidence intervals crossed zero)."
      )
      interpretation_parts[[length(interpretation_parts) + 1]] <- non_sig_text
    }
  }

  interpretation_text <- paste(interpretation_parts, collapse = "\n\n")
  return(interpretation_text)
}
