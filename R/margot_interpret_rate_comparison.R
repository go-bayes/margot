#' Compare and interpret RATE estimates from both AUTOC and QINI methods
#'
#' This function compares and synthesizes results from both AUTOC and QINI analyses,
#' highlighting outcomes that show consistent effects across both methods, as well as
#' method-specific findings. It provides a comprehensive interpretation of treatment
#' effect heterogeneity patterns.
#'
#' @param autoc_df Data frame with AUTOC RATE results
#' @param qini_df Data frame with QINI RATE results
#' @param flipped_outcomes Character vector of outcome names that were inverted during pre-processing.
#'
#' @return A list containing the comparison text and individual method interpretations
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

  # ensure flipped_outcomes is always a character vector
  if (!is.null(flipped_outcomes)) {
    if (!is.vector(flipped_outcomes)) {
      flipped_outcomes <- c(as.character(flipped_outcomes))
    } else {
      flipped_outcomes <- as.character(flipped_outcomes)
    }
  }

  # extract significant outcomes from each method
  autoc_pos <- extract_significant_outcomes(autoc_df, positive = TRUE)
  autoc_neg <- extract_significant_outcomes(autoc_df, positive = FALSE)
  qini_pos <- extract_significant_outcomes(qini_df, positive = TRUE)
  qini_neg <- extract_significant_outcomes(qini_df, positive = FALSE)

  # find overlaps
  positive_both <- intersect(autoc_pos, qini_pos)
  negative_both <- intersect(autoc_neg, qini_neg)
  positive_autoc_only <- setdiff(autoc_pos, qini_pos)
  positive_qini_only <- setdiff(qini_pos, autoc_pos)
  negative_autoc_only <- setdiff(autoc_neg, qini_neg)
  negative_qini_only <- setdiff(qini_neg, autoc_neg)

  # generate comparison summary
  comparison_parts <- list()
  comparison_parts[[1]] <- "### Comparison of AUTOC and QINI Treatment Effect Analyses"

  # brief explanation of the two methods and when each is more appropriate
  comparison_parts[[2]] <- paste0(
    "We analyzed treatment effect heterogeneity using two different RATE methods:\n\n",
    "1. **AUTOC** (logarithmic weighting): Most effective when heterogeneity is concentrated in a smaller subset (≤10%) of the population\n",
    "2. **QINI** (linear weighting): Most effective when heterogeneity is broadly distributed (>50%) across the population\n\n",
    "Comparing results from both methods provides nuanced insight into evidence for prioritising treatments using conditional average treatment effects (CATE)."
  )

  # consistent positive findings (highest confidence)
  if (length(positive_both) > 0) {
    comparison_parts[[length(comparison_parts) + 1]] <- paste0(
      "#### Evidence for Prioritising Treatments using CATE (both AUTOC and QINI)\n\n",
      "Both AUTOC and QINI analyses identified reliably positive treatment effects for the following outcome(s), ",
      "providing good evidence that targeting treatment using CATE will be beneficial:\n\n- ",
      paste(positive_both, collapse = "\n- ")
    )

    # add comparison of estimate magnitudes
    comp_table <- create_comparison_table(autoc_df, qini_df, positive_both, flipped_outcomes)
    comparison_parts[[length(comparison_parts) + 1]] <- paste0(
      "**Comparison of Estimates:**\n\n",
      comp_table
    )
  }

  # consistent negative findings (highest concern)
  if (length(negative_both) > 0) {
    comparison_parts[[length(comparison_parts) + 1]] <- paste0(
      "#### Indications for Caution From Both AUTOC and QINI Methods\n\n",
      "Both AUTOC and QINI analyses identified reliably negative treatment effects for the following outcome(s), ",
      "indicating that targeting treatment using CATE will potentially harm outcomes:\n\n- ",
      paste(negative_both, collapse = "\n- ")
    )

    # add comparison of estimate magnitudes
    comp_table <- create_comparison_table(autoc_df, qini_df, negative_both, flipped_outcomes)
    comparison_parts[[length(comparison_parts) + 1]] <- paste0(
      "**Comparison of Estimates:**\n\n",
      comp_table
    )
  }

  # method-specific findings
  if (length(positive_autoc_only) > 0 || length(negative_autoc_only) > 0 ||
      length(positive_qini_only) > 0 || length(negative_qini_only) > 0) {

    comparison_parts[[length(comparison_parts) + 1]] <- "#### Method-Specific Findings"

    # AUTOC-specific findings
    autoc_specific_text <- "**AUTOC-Specific Results:**\n\n"

    if (length(positive_autoc_only) > 0) {
      autoc_specific_text <- paste0(
        autoc_specific_text,
        "AUTOC identifies positive effects (suggesting concentrated heterogeneity) for: ",
        paste(positive_autoc_only, collapse = ", "), ". "
      )
    }

    if (length(negative_autoc_only) > 0) {
      autoc_specific_text <- paste0(
        autoc_specific_text,
        "AUTOC identifies negative effects (suggesting concentrated negative heterogeneity) for: ",
        paste(negative_autoc_only, collapse = ", "), "."
      )
    }

    if (length(positive_autoc_only) > 0 || length(negative_autoc_only) > 0) {
      comparison_parts[[length(comparison_parts) + 1]] <- autoc_specific_text
    }

    # QINI-specific findings
    qini_specific_text <- "**QINI-Specific Results:**\n\n"

    if (length(positive_qini_only) > 0) {
      qini_specific_text <- paste0(
        qini_specific_text,
        "QINI identifies positive effects (suggesting broadly distributed heterogeneity) for: ",
        paste(positive_qini_only, collapse = ", "), ". "
      )
    }

    if (length(negative_qini_only) > 0) {
      qini_specific_text <- paste0(
        qini_specific_text,
        "QINI identifies negative effects (suggesting broadly distributed negative heterogeneity) for: ",
        paste(negative_qini_only, collapse = ", "), "."
      )
    }

    if (length(positive_qini_only) > 0 || length(negative_qini_only) > 0) {
      comparison_parts[[length(comparison_parts) + 1]] <- qini_specific_text
    }
  }

  # recommendation section
  comparison_parts[[length(comparison_parts) + 1]] <- "#### Implementation Recommendations"

  # add recommendation based on findings
  if (length(positive_both) > 0) {
    comparison_parts[[length(comparison_parts) + 1]] <- paste0(
      "For outcomes with consistent positive findings (", paste(positive_both, collapse = ", "), "), ",
      "we recommend implementing CATE-based targeting as it will likely improve outcomes regardless of ",
      "whether heterogeneity is concentrated or diffuse."
    )
  }

  if (length(negative_both) > 0) {
    comparison_parts[[length(comparison_parts) + 1]] <- paste0(
      "For outcomes with consistent negative findings (", paste(negative_both, collapse = ", "), "), ",
      "we recommend using the average treatment effect (ATE) rather than CATE-based targeting to avoid potential harm."
    )
  }

  # Add recommendations for method-specific findings
  if (length(positive_autoc_only) > 0 || length(negative_autoc_only) > 0 || length(positive_both) > 0 || length(negative_both) > 0) {
    autoc_rec <- "**If heterogeneity is concentrated in a small subset of individuals:**\n\n"

    # Combine method-specific positives with overlapping positives
    autoc_all_pos <- c(positive_autoc_only, positive_both)
    if (length(autoc_all_pos) > 0) {
      autoc_rec <- paste0(autoc_rec,
                          "We recommend CATE-based targeting for outcomes identified as positive by AUTOC: ",
                          paste(autoc_all_pos, collapse = ", "), ". ")
    }

    # Combine method-specific negatives with overlapping negatives
    autoc_all_neg <- c(negative_autoc_only, negative_both)
    if (length(autoc_all_neg) > 0) {
      autoc_rec <- paste0(autoc_rec,
                          "We caution against CATE-based targeting for outcomes identified as negative by AUTOC: ",
                          paste(autoc_all_neg, collapse = ", "), ".")
    }

    comparison_parts[[length(comparison_parts) + 1]] <- autoc_rec
  }

  if (length(positive_qini_only) > 0 || length(negative_qini_only) > 0 || length(positive_both) > 0 || length(negative_both) > 0) {
    qini_rec <- "**If heterogeneity is broadly distributed across the population:**\n\n"

    # Combine method-specific positives with overlapping positives
    qini_all_pos <- c(positive_qini_only, positive_both)
    if (length(qini_all_pos) > 0) {
      qini_rec <- paste0(qini_rec,
                         "We recommend CATE-based targeting for outcomes identified as positive by QINI: ",
                         paste(qini_all_pos, collapse = ", "), ". ")
    }

    # Combine method-specific negatives with overlapping negatives
    qini_all_neg <- c(negative_qini_only, negative_both)
    if (length(qini_all_neg) > 0) {
      qini_rec <- paste0(qini_rec,
                         "We caution against CATE-based targeting for outcomes identified as negative by QINI: ",
                         paste(qini_all_neg, collapse = ", "), ".")
    }

    comparison_parts[[length(comparison_parts) + 1]] <- qini_rec
  }

  # Don't include individual interpretations in the main output
  # Instead, return them separately so they can be printed if needed
  autoc_interpretation <- margot_interpret_rate(autoc_df, flipped_outcomes, target = "AUTOC")
  qini_interpretation <- margot_interpret_rate(qini_df, flipped_outcomes, target = "QINI")

  # combine all parts for the main comparison
  comparison_text <- paste(comparison_parts, collapse = "\n\n")

  # return a list containing the main comparison and individual interpretations
  return(list(
    comparison = comparison_text,
    autoc_results = autoc_interpretation,
    qini_results = qini_interpretation
  ))
}

# Helper function to extract significant outcomes
extract_significant_outcomes <- function(rate_df, positive = TRUE) {
  outcome_names <- rate_df[[1]]

  if (positive) {
    # extract positively significant outcomes (CI lower bound > 0)
    sig_idx <- which(rate_df[,"2.5%"] > 0)
  } else {
    # extract negatively significant outcomes (CI upper bound < 0)
    sig_idx <- which(rate_df[,"97.5%"] < 0)
  }

  if (length(sig_idx) > 0) {
    return(outcome_names[sig_idx])
  } else {
    return(character(0))
  }
}

# Helper function to create a comparison table of estimates
create_comparison_table <- function(autoc_df, qini_df, outcomes, flipped_outcomes = NULL) {
  # prepare data for table
  table_data <- data.frame(
    Outcome = character(0),
    AUTOC_Estimate = character(0),
    QINI_Estimate = character(0),
    Flipped = character(0),
    stringsAsFactors = FALSE
  )

  for (outcome in outcomes) {
    # find outcome in AUTOC data
    autoc_idx <- which(autoc_df[[1]] == outcome)
    autoc_est <- autoc_df[autoc_idx, "RATE Estimate"]
    autoc_ci_lower <- autoc_df[autoc_idx, "2.5%"]
    autoc_ci_upper <- autoc_df[autoc_idx, "97.5%"]

    # find outcome in QINI data
    qini_idx <- which(qini_df[[1]] == outcome)
    qini_est <- qini_df[qini_idx, "RATE Estimate"]
    qini_ci_lower <- qini_df[qini_idx, "2.5%"]
    qini_ci_upper <- qini_df[qini_idx, "97.5%"]

    # check if outcome was flipped
    is_flipped <- FALSE
    if (!is.null(flipped_outcomes)) {
      outcome_str <- gsub("\\*", "", as.character(outcome))
      flipped_clean <- gsub("\\*", "", flipped_outcomes)
      is_flipped <- outcome_str %in% flipped_clean
    }

    # format estimates
    autoc_formatted <- sprintf("%.3f (95%% CI: %.3f, %.3f)", autoc_est, autoc_ci_lower, autoc_ci_upper)
    qini_formatted <- sprintf("%.3f (95%% CI: %.3f, %.3f)", qini_est, qini_ci_lower, qini_ci_upper)

    # add to table data
    table_data <- rbind(table_data, data.frame(
      Outcome = outcome,
      AUTOC_Estimate = autoc_formatted,
      QINI_Estimate = qini_formatted,
      Flipped = if(is_flipped) "Yes" else "No",
      stringsAsFactors = FALSE
    ))
  }

  # create markdown table
  table_header <- "| Outcome | AUTOC Estimate | QINI Estimate | Flipped? |"
  table_sep <- "|:--------|:---------------|:--------------|:---------|"
  table_rows <- apply(table_data, 1, function(row) {
    sprintf("| %s | %s | %s | %s |", row["Outcome"], row["AUTOC_Estimate"], row["QINI_Estimate"], row["Flipped"])
  })

  table_md <- c(table_header, table_sep, table_rows)
  return(paste(table_md, collapse = "\n"))
}
