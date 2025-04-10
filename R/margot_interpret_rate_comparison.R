#' Compare and interpret RATE estimates from both AUTOC and QINI methods
#'
#' This function compares and synthesises results from AUTOC and QINI analyses,
#' providing an integrated interpretation of treatment effect heterogeneity. It
#' evaluates prioritisation policies based on the estimated conditional treatment effect,
#' and examines how these estimates differ from targeting by the average
#' treatment effect (ATE). Negative RATE estimates provide strong evidence that CATE-based
#' targeting would lead to worse outcomes, while positive estimates remain suggestive.
#'
#' @param autoc_df Data frame with AUTOC RATE results.
#' @param qini_df Data frame with QINI RATE results.
#' @param flipped_outcomes Character vector of outcome names that were inverted during pre-processing.
#'
#' @return A list containing the integrated comparison text as well as the individual
#' method interpretations.
#'
#' @examples
#' \dontrun{
#' comparison <- margot_interpret_rate_comparison(
#'   rate_table_all$rate_result,  # AUTOC results
#'   rate_table_all$rate_qini,    # QINI results
#'   flipped_outcomes = flipped_names
#' )
#' cat(comparison$comparison)
#' }
#'
#' @export
margot_interpret_rate_comparison <- function(autoc_df, qini_df, flipped_outcomes = NULL) {
  # validate input data frames
  required_cols <- c("RATE Estimate", "2.5%", "97.5%")
  if (!all(required_cols %in% colnames(autoc_df)) || !all(required_cols %in% colnames(qini_df))) {
    stop("Both data frames must contain columns 'RATE Estimate', '2.5%', and '97.5%'.")
  }

  # extract outcome names from first column of each data frame
  autoc_outcomes <- autoc_df[[1]]
  qini_outcomes <- qini_df[[1]]

  # ensure flipped_outcomes is a character vector
  if (!is.null(flipped_outcomes)) {
    flipped_outcomes <- as.character(flipped_outcomes)
  }

  # extract statistically significant outcomes from each method using helper function
  autoc_pos <- extract_significant_outcomes(autoc_df, positive = TRUE)
  autoc_neg <- extract_significant_outcomes(autoc_df, positive = FALSE)
  qini_pos <- extract_significant_outcomes(qini_df, positive = TRUE)
  qini_neg <- extract_significant_outcomes(qini_df, positive = FALSE)

  # determine overlaps between methods
  positive_both <- intersect(autoc_pos, qini_pos)
  negative_both <- intersect(autoc_neg, qini_neg)
  positive_autoc_only <- setdiff(autoc_pos, qini_pos)
  positive_qini_only <- setdiff(qini_pos, autoc_pos)
  negative_autoc_only <- setdiff(autoc_neg, qini_neg)
  negative_qini_only <- setdiff(qini_neg, autoc_neg)

  # start constructing the integrated comparison text
  comparison_parts <- list()
  comparison_parts[[1]] <- "### Comparison of AUTOC and QINI Analyses"

  # explanation of the two methods in integrated text
  comparison_parts[[2]] <- paste0(
    "We analysed treatment effect heterogeneity using two RATE methods. AUTOC employs logarithmic weighting, which is most effective when heterogeneity is concentrated in a small subset of the population (typically ≤10%), while QINI employs linear weighting and is most effective when heterogeneity is broadly distributed (typically >50%). ",
    "Comparing the two methods allows us to evaluate a prioritisation policy based on $\\tau(x)$ relative to a policy based on the ATE. ",
    "For outcomes where the RATE estimates are negative, there is strong evidence that targeting by CATE would yield inferior outcomes; reliably positive estimates suggest that targeting by CATE could be beneficial."
  )

  # integrated interpretation for outcomes with reliable positive estimates
  if (length(positive_both) > 0) {
    pos_text <- paste0(
      "When both methods yield statistically positive RATE estimates for outcome(s): ",
      paste(positive_both, collapse = ", "),
      ", this suggests that a CATE-based targeting policy may achieve better outcomes compared to an ATE-based policy. "
    )
    # add comparison of estimate magnitudes using helper to create a markdown table
    comp_table <- create_comparison_table(autoc_df, qini_df, positive_both, flipped_outcomes)
    pos_text <- paste0(pos_text, " The estimates are compared below.\n\n", comp_table)
    comparison_parts[[length(comparison_parts) + 1]] <- pos_text
  }

  # integrated interpretation for outcomes with reliable negative estimates
  if (length(negative_both) > 0) {
    neg_text <- paste0(
      "When both methods yield statistically negative RATE estimates for outcome(s): ",
      paste(negative_both, collapse = ", "),
      ", the evidence strongly suggests that prioritising treatment based on $\\tau(x)$ would lead to worse outcomes than targeting by the ATE. ",
      "Extra caution is warranted in these cases."
    )
    comp_table <- create_comparison_table(autoc_df, qini_df, negative_both, flipped_outcomes)
    neg_text <- paste0(neg_text, " The corresponding estimates are shown below.\n\n", comp_table)
    comparison_parts[[length(comparison_parts) + 1]] <- neg_text
  }

  # method-specific findings as separate commentary
  if (length(positive_autoc_only) > 0 || length(negative_autoc_only) > 0 ||
      length(positive_qini_only) > 0 || length(negative_qini_only) > 0) {

    ms_text <- "In addition, the individual methods yield method-specific results. In the AUTOC analysis, "
    if (length(positive_autoc_only) > 0) {
      ms_text <- paste0(
        ms_text,
        "the following outcome(s) show statistically positive RATE estimates: ",
        paste(positive_autoc_only, collapse = ", "), ". "
      )
    }
    if (length(negative_autoc_only) > 0) {
      ms_text <- paste0(
        ms_text,
        "Also, the following outcome(s) show statistically negative RATE estimates: ",
        paste(negative_autoc_only, collapse = ", "), ". "
      )
    }
    ms_text <- paste0(ms_text, "In the QINI analysis, ")
    if (length(positive_qini_only) > 0) {
      ms_text <- paste0(
        ms_text,
        "the following outcome(s) yield statistically positive estimates: ",
        paste(positive_qini_only, collapse = ", "), ". "
      )
    }
    if (length(negative_qini_only) > 0) {
      ms_text <- paste0(
        ms_text,
        "and the following outcome(s) yield statistically negative estimates: ",
        paste(negative_qini_only, collapse = ", "), "."
      )
    }
    comparison_parts[[length(comparison_parts) + 1]] <- ms_text
  }

  # --- New conditional policy recommendations block ---
  # Determine overall policy recommendation based on reliable evidence
  if (length(positive_both) == 0 && length(negative_both) == 0) {
    policy_text <- "The RATE analysis did not yield any statistically reliable evidence for treatment effect heterogeneity. This indicates that there is insufficient evidence to support a change from an ATE-based targeting strategy."
  } else if (length(negative_both) == 0) {
    policy_text <- paste0(
      "For outcome(s) ", paste(positive_both, collapse = ", "),
      ", the analysis yields statistically reliable positive RATE estimates. This suggests that a CATE-based targeting approach would likely improve outcomes compared to an ATE-based strategy."
    )
  } else if (length(positive_both) == 0) {
    policy_text <- paste0(
      "For outcome(s) ", paste(negative_both, collapse = ", "),
      ", the analysis yields statistically reliable negative RATE estimates, providing clear evidence that a CATE-based targeting approach would lead to inferior outcomes compared to an ATE-based policy."
    )
  } else {
    policy_text <- "For outcomes with 95% confidence intervals entirely above zero, the evidence supports adopting a CATE-based targeting strategy. By contrast, for outcomes with 95% confidence intervals entirely below zero, the evidence strongly advises using an ATE-based strategy."
  }

  comparison_parts[[length(comparison_parts) + 1]] <- paste0(
    "#### Policy Recommendations\n\n",
    policy_text
  )
  # --- End of policy recommendations block ---

  # obtain individual interpretations for each method
  autoc_interpretation <- margot_interpret_rate(autoc_df, flipped_outcomes, target = "AUTOC")
  qini_interpretation <- margot_interpret_rate(qini_df, flipped_outcomes, target = "QINI")

  # combine all parts into a single text output
  comparison_text <- paste(comparison_parts, collapse = "\n\n")
  return(list(
    comparison = comparison_text,
    autoc_results = autoc_interpretation,
    qini_results = qini_interpretation
  ))
}  # <- Ensure you close your function here

# --- Helper Functions (defined separately) ---

# helper function to extract statistically significant outcomes
extract_significant_outcomes <- function(rate_df, positive = TRUE) {
  outcome_names <- rate_df[[1]]
  if (positive) {
    # outcomes with lower bound of the 95% CI greater than zero
    sig_idx <- which(rate_df[,"2.5%"] > 0)
  } else {
    # outcomes with upper bound of the 95% CI less than zero
    sig_idx <- which(rate_df[,"97.5%"] < 0)
  }
  if (length(sig_idx) > 0) {
    return(outcome_names[sig_idx])
  } else {
    return(character(0))
  }
}

# helper function to create a markdown table comparing estimates from both methods
create_comparison_table <- function(autoc_df, qini_df, outcomes, flipped_outcomes = NULL) {
  # prepare data for the table
  table_data <- data.frame(
    Outcome = character(0),
    AUTOC_Estimate = character(0),
    QINI_Estimate = character(0),
    Flipped = character(0),
    stringsAsFactors = FALSE
  )

  for (outcome in outcomes) {
    # locate outcome row in AUTOC data
    autoc_idx <- which(autoc_df[[1]] == outcome)
    autoc_est <- autoc_df[autoc_idx, "RATE Estimate"]
    autoc_ci_lower <- autoc_df[autoc_idx, "2.5%"]
    autoc_ci_upper <- autoc_df[autoc_idx, "97.5%"]

    # locate outcome row in QINI data
    qini_idx <- which(qini_df[[1]] == outcome)
    qini_est <- qini_df[qini_idx, "RATE Estimate"]
    qini_ci_lower <- qini_df[qini_idx, "2.5%"]
    qini_ci_upper <- qini_df[qini_idx, "97.5%"]

    # determine if outcome was flipped
    is_flipped <- FALSE
    if (!is.null(flipped_outcomes)) {
      outcome_clean <- gsub("\\*", "", as.character(outcome))
      flipped_clean <- gsub("\\*", "", flipped_outcomes)
      is_flipped <- outcome_clean %in% flipped_clean
    }

    # format the estimates with confidence intervals
    autoc_formatted <- sprintf("%.3f (95%% CI: %.3f, %.3f)", autoc_est, autoc_ci_lower, autoc_ci_upper)
    qini_formatted <- sprintf("%.3f (95%% CI: %.3f, %.3f)", qini_est, qini_ci_lower, qini_ci_upper)

    # append to the table data
    table_data <- rbind(table_data, data.frame(
      Outcome = outcome,
      AUTOC_Estimate = autoc_formatted,
      QINI_Estimate = qini_formatted,
      Flipped = if(is_flipped) "Yes" else "No",
      stringsAsFactors = FALSE
    ))
  }

  # create a markdown table as a string
  table_header <- "| Outcome | AUTOC Estimate | QINI Estimate | Flipped? |"
  table_sep <- "|:--------|:---------------|:--------------|:---------|"
  table_rows <- apply(table_data, 1, function(row) {
    sprintf("| %s | %s | %s | %s |", row["Outcome"], row["AUTOC_Estimate"], row["QINI_Estimate"], row["Flipped"])
  })

  table_md <- c(table_header, table_sep, table_rows)
  return(paste(table_md, collapse = "\n"))
}
# margot_interpret_rate_comparison <- function(autoc_df, qini_df, flipped_outcomes = NULL) {
#   # validate input data frames
#   required_cols <- c("RATE Estimate", "2.5%", "97.5%")
#   if (!all(required_cols %in% colnames(autoc_df)) || !all(required_cols %in% colnames(qini_df))) {
#     stop("Both data frames must contain columns 'RATE Estimate', '2.5%', and '97.5%'.")
#   }
#
#   # extract outcome names from first column of each data frame
#   autoc_outcomes <- autoc_df[[1]]
#   qini_outcomes <- qini_df[[1]]
#
#   # ensure flipped_outcomes is a character vector
#   if (!is.null(flipped_outcomes)) {
#     flipped_outcomes <- as.character(flipped_outcomes)
#   }
#
#   # extract statistically significant outcomes from each method using helper function
#   autoc_pos <- extract_significant_outcomes(autoc_df, positive = TRUE)
#   autoc_neg <- extract_significant_outcomes(autoc_df, positive = FALSE)
#   qini_pos <- extract_significant_outcomes(qini_df, positive = TRUE)
#   qini_neg <- extract_significant_outcomes(qini_df, positive = FALSE)
#
#   # determine overlaps between methods
#   positive_both <- intersect(autoc_pos, qini_pos)
#   negative_both <- intersect(autoc_neg, qini_neg)
#   positive_autoc_only <- setdiff(autoc_pos, qini_pos)
#   positive_qini_only <- setdiff(qini_pos, autoc_pos)
#   negative_autoc_only <- setdiff(autoc_neg, qini_neg)
#   negative_qini_only <- setdiff(qini_neg, autoc_neg)
#
#   # start constructing the integrated comparison text
#   comparison_parts <- list()
#   comparison_parts[[1]] <- "### Comparison of AUTOC and QINI Analyses"
#
#   # explanation of the two methods in integrated text
#   comparison_parts[[2]] <- paste0(
#     "We analysed treatment effect heterogeneity using two RATE methods. AUTOC employs logarithmic weighting, which is most effective when heterogeneity is concentrated in a small subset of the population (typically ≤10%), while QINI employs linear weighting and is most effective when heterogeneity is broadly distributed (typically >50%). ",
#     "Comparing the two methods allows us to evaluate a prioritisation policy based on $\\tau(x)$ relative to a policy based on the ATE. ",
#     "For outcomes where the RATE estimates are negative, there is strong evidence that targeting by CATE would yield inferior outcomes; positive estimates are suggestive but remain inconclusive."
#   )
#
#   # integrated interpretation when both methods yield statistically positive estimates
#   if (length(positive_both) > 0) {
#     pos_text <- paste0(
#       "When both methods yield statistically positive RATE estimates for outcome(s): ",
#       paste(positive_both, collapse = ", "),
#       ", this suggests that a CATE-based targeting policy may achieve better outcomes compared to an ATE-based policy. ",
#       "However, these findings remain suggestive rather than definitive."
#     )
#     # add comparison of estimate magnitudes using helper to create a markdown table
#     comp_table <- create_comparison_table(autoc_df, qini_df, positive_both, flipped_outcomes)
#     pos_text <- paste0(pos_text, " The estimates are compared below.\n\n", comp_table)
#     comparison_parts[[length(comparison_parts) + 1]] <- pos_text
#   }
#
#   # integrated interpretation when both methods yield statistically negative estimates
#   if (length(negative_both) > 0) {
#     neg_text <- paste0(
#       "When both methods yield statistically negative RATE estimates for outcome(s): ",
#       paste(negative_both, collapse = ", "),
#       ", the evidence strongly suggests that prioritising treatment based on $\\tau(x)$ would lead to worse outcomes than targeting by the ATE. ",
#       "Extra caution is warranted in these cases."
#     )
#     comp_table <- create_comparison_table(autoc_df, qini_df, negative_both, flipped_outcomes)
#     neg_text <- paste0(neg_text, " The corresponding estimates are shown below.\n\n", comp_table)
#     comparison_parts[[length(comparison_parts) + 1]] <- neg_text
#   }
#
#   # method-specific findings as separate commentary
#   if (length(positive_autoc_only) > 0 || length(negative_autoc_only) > 0 ||
#       length(positive_qini_only) > 0 || length(negative_qini_only) > 0) {
#
#     ms_text <- "In addition, the individual methods yield method-specific results. In the AUTOC analysis, "
#     if (length(positive_autoc_only) > 0) {
#       ms_text <- paste0(
#         ms_text,
#         "the following outcome(s) show statistically positive RATE estimates: ",
#         paste(positive_autoc_only, collapse = ", "), ". "
#       )
#     }
#     if (length(negative_autoc_only) > 0) {
#       ms_text <- paste0(
#         ms_text,
#         "Also, the following outcome(s) show statistically negative RATE estimates: ",
#         paste(negative_autoc_only, collapse = ", "), ". "
#       )
#     }
#     ms_text <- paste0(ms_text, "In the QINI analysis, ")
#     if (length(positive_qini_only) > 0) {
#       ms_text <- paste0(
#         ms_text,
#         "the following outcome(s) yield statistically positive estimates: ",
#         paste(positive_qini_only, collapse = ", "), ". "
#       )
#     }
#     if (length(negative_qini_only) > 0) {
#       ms_text <- paste0(
#         ms_text,
#         "and the following outcome(s) yield statistically negative estimates: ",
#         paste(negative_qini_only, collapse = ", "), "."
#       )
#     }
#     comparison_parts[[length(comparison_parts) + 1]] <- ms_text
#   }
#
#   # concise policy recommendations in integrated text
#   # Determine overall policy recommendation based on reliable evidence
#   if (length(positive_both) == 0 && length(negative_both) == 0) {
#     policy_text <- "The RATE analysis did not yield any statistically reliable evidence for treatment effect heterogeneity. This indicates that there is insufficient evidence to support a change from an ATE-based targeting strategy."
#   } else if (length(negative_both) == 0) {
#     policy_text <- paste0(
#       "For outcome(s) ", paste(positive_both, collapse = ", "),
#       ", the analysis yields statistically reliable positive RATE estimates. This suggests that a CATE-based targeting approach would likely improve outcomes compared to an ATE-based strategy."
#     )
#   } else if (length(positive_both) == 0) {
#     policy_text <- paste0(
#       "For outcome(s) ", paste(negative_both, collapse = ", "),
#       ", the analysis yields statistically reliable negative RATE estimates, providing clear evidence that a CATE-based targeting approach would lead to inferior outcomes compared to an ATE-based policy."
#     )
#   } else {
#     policy_text <- "For outcomes with 95% confidence intervals entirely above zero, the evidence supports adopting a CATE-based targeting strategy. By contrast, for outcomes with 95% confidence intervals entirely below zero, the evidence strongly advises using an ATE-based strategy."
#   }
#
#   comparison_parts[[length(comparison_parts) + 1]] <- paste0(
#     "#### Policy Recommendations\n\n",
#     policy_text
#   )
#
# # helper function to extract statistically significant outcomes
# extract_significant_outcomes <- function(rate_df, positive = TRUE) {
#   outcome_names <- rate_df[[1]]
#   if (positive) {
#     # outcomes with lower bound of the 95% CI greater than zero
#     sig_idx <- which(rate_df[,"2.5%"] > 0)
#   } else {
#     # outcomes with upper bound of the 95% CI less than zero
#     sig_idx <- which(rate_df[,"97.5%"] < 0)
#   }
#   if (length(sig_idx) > 0) {
#     return(outcome_names[sig_idx])
#   } else {
#     return(character(0))
#   }
# }
#
# # helper function to create a markdown table comparing estimates from both methods
# create_comparison_table <- function(autoc_df, qini_df, outcomes, flipped_outcomes = NULL) {
#   # prepare data for the table
#   table_data <- data.frame(
#     Outcome = character(0),
#     AUTOC_Estimate = character(0),
#     QINI_Estimate = character(0),
#     Flipped = character(0),
#     stringsAsFactors = FALSE
#   )
#
#   for (outcome in outcomes) {
#     # locate outcome row in AUTOC data
#     autoc_idx <- which(autoc_df[[1]] == outcome)
#     autoc_est <- autoc_df[autoc_idx, "RATE Estimate"]
#     autoc_ci_lower <- autoc_df[autoc_idx, "2.5%"]
#     autoc_ci_upper <- autoc_df[autoc_idx, "97.5%"]
#
#     # locate outcome row in QINI data
#     qini_idx <- which(qini_df[[1]] == outcome)
#     qini_est <- qini_df[qini_idx, "RATE Estimate"]
#     qini_ci_lower <- qini_df[qini_idx, "2.5%"]
#     qini_ci_upper <- qini_df[qini_idx, "97.5%"]
#
#     # determine if outcome was flipped
#     is_flipped <- FALSE
#     if (!is.null(flipped_outcomes)) {
#       outcome_clean <- gsub("\\*", "", as.character(outcome))
#       flipped_clean <- gsub("\\*", "", flipped_outcomes)
#       is_flipped <- outcome_clean %in% flipped_clean
#     }
#
#     # format the estimates with confidence intervals
#     autoc_formatted <- sprintf("%.3f (95%% CI: %.3f, %.3f)", autoc_est, autoc_ci_lower, autoc_ci_upper)
#     qini_formatted <- sprintf("%.3f (95%% CI: %.3f, %.3f)", qini_est, qini_ci_lower, qini_ci_upper)
#
#     # append to the table data
#     table_data <- rbind(table_data, data.frame(
#       Outcome = outcome,
#       AUTOC_Estimate = autoc_formatted,
#       QINI_Estimate = qini_formatted,
#       Flipped = if(is_flipped) "Yes" else "No",
#       stringsAsFactors = FALSE
#     ))
#   }
#
#   # create a markdown table as a string
#   table_header <- "| Outcome | AUTOC Estimate | QINI Estimate | Flipped? |"
#   table_sep <- "|:--------|:---------------|:--------------|:---------|"
#   table_rows <- apply(table_data, 1, function(row) {
#     sprintf("| %s | %s | %s | %s |", row["Outcome"], row["AUTOC_Estimate"], row["QINI_Estimate"], row["Flipped"])
#   })
#
#   table_md <- c(table_header, table_sep, table_rows)
#   return(paste(table_md, collapse = "\n"))
# }
