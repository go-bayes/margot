#’ Interpret rank-weighted average treatment effect (RATE) estimates
#’
#’ This function provides a concise interpretation of RATE estimates from causal forests.
#’
#’ @param rate_df a data frame or list from margot_rate()
#’ @param flipped_outcomes character vector of outcomes inverted during preprocessing
#’ @param target “AUTOC” or “Qini” (ignored if rate_df is a list)
#’ @return a markdown string interpreting the RATE estimates
#’ @importFrom stats qnorm
#’ @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc","rate_qini") %in% names(rate_df))) {
    return(margot_interpret_rate_comparison(
      rate_df$rate_autoc, rate_df$rate_qini, flipped_outcomes
    ))
  }
  if (!target %in% c("AUTOC","Qini")) stop("target must be 'AUTOC' or 'Qini'.")
  required <- c("RATE Estimate","2.5%","97.5%")
  if (!all(required %in% colnames(rate_df))) stop("rate_df must contain 'RATE Estimate', '2.5%', and '97.5%'.")

  outcomes <- rate_df[[1]]
  ci_low  <- rate_df[,"2.5%"]
  ci_high <- rate_df[,"97.5%"]
  pos_idx <- which(ci_low  > 0)
  neg_idx <- which(ci_high < 0)

  parts <- list(
    paste0(
      "### Targeting operating characteristic (TOC) by rank average treatment effect (RATE): ",
      target
    )
  )
  desc <- if (target == "AUTOC") {
    "AUTOC uses logarithmic weighting to focus treatment on top responders."
  } else {
    "Qini uses linear weighting to balance effect size and prevalence for aggregate gain."
  }
  parts <- c(parts, desc)

  if (length(pos_idx) > 0) {
    pos_out <- outcomes[pos_idx]
    pos_est <- sapply(pos_idx, function(i) sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      outcomes[i], rate_df[i,"RATE Estimate"], ci_low[i], ci_high[i]
    ))
    parts <- c(parts,
               paste0(
                 "Positive RATE estimates indicate that CATE-based targeting outperforms an ATE-based strategy for: ",
                 paste(pos_out, collapse = ", "), "."
               ),
               paste0(
                 "These estimates (", paste(pos_est, collapse = "; "), ") provide robust evidence of treatment effect heterogeneity."
               )
    )
  }
  if (length(neg_idx) > 0) {
    neg_out <- outcomes[neg_idx]
    neg_est <- sapply(neg_idx, function(i) sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      outcomes[i], rate_df[i,"RATE Estimate"], ci_low[i], ci_high[i]
    ))
    parts <- c(parts,
               paste0(
                 "Negative RATE estimates indicate that CATE-based targeting underperforms an ATE-based strategy for: ",
                 paste(neg_out, collapse = ", "), "."
               ),
               paste0(
                 "These estimates (", paste(neg_est, collapse = "; "), ") caution against CATE-based prioritisation for these outcomes."
               )
    )
  }
  nonsig <- setdiff(seq_along(outcomes), union(pos_idx, neg_idx))
  if (length(pos_idx) == 0 && length(neg_idx) == 0) {
    parts <- c(parts,
               "No outcome shows a confidence interval excluding zero; evidence for heterogeneity is inconclusive."
    )
  } else if (length(nonsig) > 0) {
    parts <- c(parts,
               paste0(
                 "For outcomes whose 95% CI includes zero (",
                 paste(outcomes[nonsig], collapse = ", "), "), evidence remains inconclusive."
               )
    )
  }
  paste(parts, collapse = "\\n\\n")
}

#’ Compare and interpret RATE estimates from both AUTOC and Qini
#’
#’ This function compares and synthesises results from two RATE methods.
#’
#’ @param autoc_df data frame of AUTOC RATE results
#’ @param qini_df data frame of Qini RATE results
#’ @param flipped_outcomes character vector of inverted outcomes
#’ @return list with elements comparison (markdown), autoc_results, qini_results
#’ @keywords internal
margot_interpret_rate_comparison <- function(autoc_df, qini_df, flipped_outcomes = NULL) {
  required <- c("RATE Estimate","2.5%","97.5%")
  if (!all(required %in% colnames(autoc_df)) ||
      !all(required %in% colnames(qini_df))) {
    stop("Both inputs must have 'RATE Estimate', '2.5%', and '97.5%'.")
  }

  outcomes           <- autoc_df[[1]]
  autoc_pos          <- outcomes[autoc_df[,"2.5%"]   > 0]
  autoc_neg          <- outcomes[autoc_df[,"97.5%"]  < 0]
  qini_pos           <- outcomes[qini_df[,"2.5%"]    > 0]
  qini_neg           <- outcomes[qini_df[,"97.5%"]   < 0]

  # sets of significant outcomes
  autoc_significant  <- unique(c(autoc_pos, autoc_neg))
  qini_significant   <- unique(c(qini_pos, qini_neg))
  both_significant   <- intersect(autoc_significant, qini_significant)
  any_significant    <- union(autoc_significant, qini_significant)

  both_pos           <- intersect(autoc_pos, qini_pos)
  both_neg           <- intersect(autoc_neg, qini_neg)
  autoc_only_pos     <- setdiff(autoc_pos, qini_pos)
  qini_only_pos      <- setdiff(qini_pos, autoc_pos)
  autoc_only_neg     <- setdiff(autoc_neg, qini_neg)
  qini_only_neg      <- setdiff(qini_neg, autoc_neg)

  parts <- list(
    "### Comparison of targeting operating characteristic (TOC) by rank average treatment effect (RATE): AUTOC vs Qini"
  )
  parts <- c(parts,
             "We applied two TOC by RATE methods to the same causal-forest $\tau(x)$ estimates:",
             "- TOC AUTOC intensifies focus on top responders via logarithmic weighting.",
             "- TOC Qini balances effect size and prevalence via linear weighting."
  )
  if (length(both_pos) > 0) {
    parts <- c(parts,
               paste0("Both methods yield positive RATE estimates for: ", paste(both_pos, collapse = ", "), "."),
               "This concordance indicates robust evidence of treatment effect heterogeneity."
    )
  }
  if (length(both_neg) > 0) {
    parts <- c(parts,
               paste0("Both methods yield negative RATE estimates for: ", paste(both_neg, collapse = ", "), "."),
               "This concordance cautions against CATE-based prioritisation for these outcomes."
    )
  }
  # disagreement guidance
  if (length(autoc_only_pos) + length(qini_only_pos) > 0) {
    discord_pos <- c()
    if (length(autoc_only_pos)>0) discord_pos <- c(discord_pos,
                                                   paste0("only AUTOC yields a positive RATE for ", paste(autoc_only_pos, collapse = ", ")))
    if (length(qini_only_pos)>0) discord_pos <- c(discord_pos,
                                                  paste0("only Qini yields a positive RATE for ", paste(qini_only_pos, collapse = ", ")))
    parts <- c(parts,
               paste0(
                 "When TOC Qini and TOC AUTOC disagree on positive RATE (", paste(discord_pos, collapse = "; "), "), ",
                 "choose Qini to maximise overall benefit or AUTOC to focus on top responders."
               )
    )
  }
  # negative-only guidance
  if (length(autoc_only_neg) + length(qini_only_neg) > 0) {
    discord_neg <- c()
    if (length(autoc_only_neg)>0) discord_neg <- c(discord_neg,
                                                   paste0("only AUTOC yields a negative RATE for ", paste(autoc_only_neg, collapse = ", ")))
    if (length(qini_only_neg)>0) discord_neg <- c(discord_neg,
                                                  paste0("only Qini yields a negative RATE for ", paste(qini_only_neg, collapse = ", ")))
    parts <- c(parts,
               paste0(
                 "For outcomes where one method yields a negative RATE (", paste(discord_neg, collapse = "; "), "), ",
                 "CATE-based prioritisation is not recommended and an ATE-based approach should be used."
               )
    )
  }
  # no significant outcomes
  if (length(any_significant)==0) {
    parts <- c(parts,
               "Neither TOC AUTOC nor TOC Qini yields a statistically significant RATE for any outcome; evidence is inconclusive."
    )
  }
  return(list(
    comparison         = paste(parts, collapse = "

"),
    autoc_results      = margot_interpret_rate(autoc_df, flipped_outcomes, target = "AUTOC"),
    qini_results       = margot_interpret_rate(qini_df,  flipped_outcomes, target = "Qini"),
    autoc_significant  = autoc_significant,
    qini_significant   = qini_significant,
    both_significant   = both_significant,
    any_significant    = any_significant
  ))
}
comparison    = paste(parts, collapse = "\\n\\n"),
autoc_results = margot_interpret_rate(autoc_df, flipped_outcomes, target = "AUTOC"),
qini_results  = margot_interpret_rate(qini_df,  flipped_outcomes, target = "Qini")
))
}
