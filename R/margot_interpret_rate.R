#' @title Interpret rank-weighted average treatment effect (RATE) estimates
#' @description Provide a concise interpretation of RATE estimates from causal forests.
#' @param rate_df A data frame (or list) as returned by \code{margot_rate()}.
#' @param flipped_outcomes Character vector of outcomes inverted during preprocessing.
#' @param target Either \code{"AUTOC"} or \code{"Qini"} (ignored if \code{rate_df} is a list).
#' @return A markdown string interpreting the RATE estimates.
#' @importFrom stats qnorm
#' @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  # if both methods present, delegate to comparison function
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc","rate_qini") %in% names(rate_df))) {
    return(margot_interpret_rate_comparison(
      rate_df$rate_autoc, rate_df$rate_qini, flipped_outcomes
    ))
  }

  # validate inputs
  if (!target %in% c("AUTOC","Qini")) {
    stop("target must be 'AUTOC' or 'Qini'.")
  }
  required <- c("RATE Estimate","2.5%","97.5%")
  if (!all(required %in% colnames(rate_df))) {
    stop("rate_df must contain 'RATE Estimate', '2.5%', and '97.5%'.")
  }

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

  # method description
  desc <- if (target == "AUTOC") {
    "TOC AUTOC uses logarithmic weighting to focus treatment on top responders."
  } else {
    "TOC Qini uses linear weighting to balance effect size and prevalence for aggregate gain."
  }
  parts <- c(parts, desc)

  # positive outcomes
  if (length(pos_idx) > 0) {
    pos_out <- outcomes[pos_idx]
    pos_est <- sapply(pos_idx, function(i) {
      sprintf("%s: %.3f (95%% CI %.3f, %.3f)",
              outcomes[i], rate_df[i,"RATE Estimate"], ci_low[i], ci_high[i])
    })
    parts <- c(parts,
               paste0(
                 "Positive RATE estimates indicate that CATE-based targeting outperforms an ATE-based strategy for the following outcomes: ",
                 paste(pos_out, collapse = ", "), "."
               ),
               paste0(
                 "These estimates (", paste(pos_est, collapse = "; "),
                 ") provide robust evidence of treatment effect heterogeneity."
               )
    )
  }

  # negative outcomes
  if (length(neg_idx) > 0) {
    neg_out <- outcomes[neg_idx]
    neg_est <- sapply(neg_idx, function(i) {
      sprintf("%s: %.3f (95%% CI %.3f, %.3f)",
              outcomes[i], rate_df[i,"RATE Estimate"], ci_low[i], ci_high[i])
    })
    parts <- c(parts,
               paste0(
                 "Negative rate estimates indicate that CATE-based targeting underperforms an ATE-based strategy for the following outcomes: ",
                 paste(neg_out, collapse = ", "), "."
               ),
               paste0(
                 "These estimates (", paste(neg_est, collapse = "; "),
                 ") caution against using CATE-based prioritisation for these outcomes."
               )
    )
  }

  # inconclusive outcomes
  nonsig <- setdiff(seq_along(outcomes), union(pos_idx, neg_idx))
  if (length(pos_idx) == 0 && length(neg_idx) == 0) {
    parts <- c(parts,
               "No outcome shows a confidence interval excluding zero. The evidence for heterogeneity is inconclusive."
    )
  } else if (length(nonsig) > 0) {
    parts <- c(parts,
               paste0(
                 "For outcomes whose 95% confidence interval includes zero (",
                 paste(outcomes[nonsig], collapse = ", "),
                 "), the evidence remains inconclusive."
               )
    )
  }

  paste(parts, collapse = "\n\n")
}


#' @title Compare and interpret RATE estimates (AUTOC vs Qini)
#' @description Internal helper to synthesise two RATE outputs.
#' @param autoc_df A data frame of AUTOC RATE results.
#' @param qini_df A data frame of Qini RATE results.
#' @param flipped_outcomes Character vector of inverted outcomes.
#' @return A list with elements \code{comparison}, \code{autoc_results}, and \code{qini_results}.
#' @keywords internal
margot_interpret_rate_comparison <- function(autoc_df, qini_df, flipped_outcomes = NULL) {
  required <- c("RATE Estimate", "2.5%", "97.5%")
  if (!all(required %in% colnames(autoc_df)) ||
      !all(required %in% colnames(qini_df))) {
    stop("Both inputs must have 'RATE Estimate', '2.5%', and '97.5%'.")
  }

  outcomes      <- autoc_df[[1]]
  sig_auto_pos  <- outcomes[autoc_df[, "2.5%"]  > 0]
  sig_auto_neg  <- outcomes[autoc_df[, "97.5%"] < 0]
  sig_qini_pos  <- outcomes[qini_df[, "2.5%"]   > 0]
  sig_qini_neg  <- outcomes[qini_df[, "97.5%"]  < 0]

  both_pos     <- intersect(sig_auto_pos, sig_qini_pos)
  both_neg     <- intersect(sig_auto_neg, sig_qini_neg)
  auto_only    <- setdiff(sig_auto_pos, sig_qini_pos)
  qini_only    <- setdiff(sig_qini_pos, sig_auto_pos)
  auto_neg_only<- setdiff(sig_auto_neg, sig_qini_neg)
  qini_neg_only<- setdiff(sig_qini_neg, sig_auto_neg)

  parts <- list(
    "### Comparison of targeting operating characteristic (TOC) by rank average treatment effect (RATE): AUTOC vs Qini"
  )

  parts <- c(parts,
             "We evaluated two prioritisation strategies to the same causal-forest $\\tau(x)$ estimates.",
             "- TOC AUTOC intensifies the focus on top responders via logarithmic weighting.",
             "- TOC Qini balances effect size and prevalence via linear weighting."
  )

  # concordant positive
  if (length(both_pos) > 0) {
    parts <- c(parts,
               paste0(
                 "Both methods yield positive RATE estimates for: ",
                 paste(both_pos, collapse = ", "), "."
               ),
               "This concordance indicates robust evidence of treatment effect heterogeneity."
    )
  }

  # concordant negative
  if (length(both_neg) > 0) {
    parts <- c(parts,
               paste0(
                 "Both methods yield negative RATE estimates for: ",
                 paste(both_neg, collapse = ", "), "."
               ),
               "This concordance cautions against CATE-based prioritisation for these outcomes."
    )
  }

  # disagreement guidance or inconclusive
  if (length(auto_only) + length(qini_only) +
      length(auto_neg_only) + length(qini_neg_only) > 0) {
    discord <- c()
    if (length(auto_only) > 0)       discord <- c(discord, paste(auto_only, collapse = ", "))
    if (length(qini_only) > 0)       discord <- c(discord, paste(qini_only, collapse = ", "))
    if (length(auto_neg_only) > 0)   discord <- c(discord, paste(auto_neg_only, collapse = ", "))
    if (length(qini_neg_only) > 0)   discord <- c(discord, paste(qini_neg_only, collapse = ", "))
    parts <- c(parts,
               paste0(
                 "When TOC Qini and TOC AUTOC disagree (for example, only TOC AUTOC yields a positive RATE for ",
                 paste(discord, collapse = " and "),
                 "), choose TOC Qini to maximise overall benefit or choose TOC AUTOC to focus on top responders."
               )
    )
  } else if (length(both_pos) == 0 && length(both_neg) == 0) {
    parts <- c(parts,
               "Neither TOC AUTOC nor TOC Qini yields statistically significant RATE estimates for any outcome; the evidence is inconclusive."
    )
  }

  return(list(
    comparison    = paste(parts, collapse = "

"),
    autoc_results = margot_interpret_rate(autoc_df, flipped_outcomes, target = "AUTOC"),
    qini_results  = margot_interpret_rate(qini_df,  flipped_outcomes, target = "Qini")
  ))
}
