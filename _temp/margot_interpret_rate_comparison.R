#’ interpret rank-weighted average treatment effect (rate) estimates
#’
#’ this function provides a concise interpretation of rate estimates from causal forests.
#’
#’ @param rate_df a data frame or list from margot_rate()
#’ @param flipped_outcomes character vector of outcomes inverted during preprocessing
#’ @param target “AUTOC” or “QINI” (ignored if rate_df is a list)
#’ @return a markdown string interpreting the rate estimates
#’ @importFrom stats qnorm
#’ @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  # if both methods present, delegate to comparison function
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc","rate_qini") %in% names(rate_df))) {
    return(margot_interpret_rate_comparison(
      rate_df$rate_autoc, rate_df$rate_qini, flipped_outcomes
    ))
  }

  # validate inputs
  if (!target %in% c("AUTOC","QINI")) stop("target must be 'AUTOC' or 'QINI'")
  required <- c("RATE Estimate","2.5%","97.5%")
  if (!all(required %in% colnames(rate_df))) stop("missing required columns")

  outcomes <- rate_df[[1]]
  ci_low  <- rate_df[,"2.5%"]
  ci_high <- rate_df[,"97.5%"]

  # identify significant positive and negative
  pos_idx <- which(ci_low  > 0)
  neg_idx <- which(ci_high < 0)

  parts <- list(
    paste0("### prioritisation via ", target, " on causal-forest $\\tau(x)$ estimates")
  )

  # method description
  desc <- if (target=="AUTOC") {
    "AUTOC uses logarithmic weighting to focus treatment on the top responders."
  } else {
    "QINI uses linear weighting to balance effect size and prevalence for aggregate gain."
  }
  parts <- c(parts, desc)

  # positive outcomes
  if (length(pos_idx) > 0) {
    pos_out <- outcomes[pos_idx]
    pos_est <- sapply(pos_idx, function(i)
      sprintf("%s: %.3f (95%% CI %.3f, %.3f)",
              outcomes[i], rate_df[i,"RATE Estimate"], ci_low[i], ci_high[i])
    )
    parts <- c(parts,
               paste0(
                 "positive rate estimates for ", target, " indicate that c-ate targeting ",
                 "outperforms ate for: ", paste(pos_out, collapse=", "), ". ",
                 "(", paste(pos_est, collapse="; "), ")."
               )
    )
  }

  # negative outcomes
  if (length(neg_idx) > 0) {
    neg_out <- outcomes[neg_idx]
    neg_est <- sapply(neg_idx, function(i)
      sprintf("%s: %.3f (95%% CI %.3f, %.3f)",
              outcomes[i], rate_df[i,"RATE Estimate"], ci_low[i], ci_high[i])
    )
    parts <- c(parts,
               paste0(
                 "negative rate estimates for ", target, " indicate that c-ate targeting ",
                 "would underperform ate for: ", paste(neg_out, collapse=", "), ". ",
                 "(", paste(neg_est, collapse="; "), ")."
               )
    )
  }

  # inconclusive outcomes
  nonsig <- setdiff(seq_along(outcomes), union(pos_idx, neg_idx))
  if (length(pos_idx)==0 && length(neg_idx)==0) {
    parts <- c(parts,
               "no outcome shows a confidence interval excluding zero; heterogeneity evidence is inconclusive."
    )
  } else if (length(nonsig)>0) {
    parts <- c(parts,
               paste0(
                 "for ", paste(outcomes[nonsig], collapse=", "),
                 ", the 95% ci includes zero; evidence remains inconclusive."
               )
    )
  }

  paste(parts, collapse="\n\n")
}

#’ compare and interpret rate estimates from both autoc and qini
#’
#’ @param autoc_df data frame of autoc results
#’ @param qini_df data frame of qini results
#’ @param flipped_outcomes character vector of inverted outcomes
#’ @return list with elements comparison (markdown), autoc_results, qini_results
#’ @export
margot_interpret_rate_comparison <- function(autoc_df, qini_df, flipped_outcomes = NULL) {
  # validate inputs
  req <- c("RATE Estimate","2.5%","97.5%")
  if (!all(req %in% colnames(autoc_df)) || !all(req %in% colnames(qini_df))) {
    stop("both inputs must have RATE Estimate, 2.5%, 97.5%")
  }

  # extract outcome names and significance
  out <- autoc_df[[1]]
  sig_autoc_pos <- out[autoc_df[,"2.5%"] > 0]
  sig_autoc_neg <- out[autoc_df[,"97.5%"] < 0]
  sig_qini_pos  <- out[qini_df[,"2.5%"]    > 0]
  sig_qini_neg  <- out[qini_df[,"97.5%"]   < 0]

  # identify overlaps and method-specific
  both_pos <- intersect(sig_autoc_pos, sig_qini_pos)
  both_neg <- intersect(sig_autoc_neg, sig_qini_neg)
  auto_only_pos <- setdiff(sig_autoc_pos, sig_qini_pos)
  qini_only_pos <- setdiff(sig_qini_pos, sig_autoc_pos)
  auto_only_neg <- setdiff(sig_autoc_neg, sig_qini_neg)
  qini_only_neg <- setdiff(sig_qini_neg, sig_autoc_neg)

  parts <- list("### comparison of AUTOC vs QINI prioritisation")

  # methods summary
  parts <- c(parts,
             "we apply two strategies to the same causal-forest $\\tau(x)$ estimates: ",
             "- AUTOC intensifies focus on top responders (log weighting)\n",
             "- QINI balances effect size and prevalence (linear weighting)"
  )

  # concordant positive
  if (length(both_pos)>0) {
    parts <- c(parts,
               paste0(
                 "both methods yield positive estimates for: ", paste(both_pos, collapse=", "),
                 ". robust heterogeneity signal. see table below."
               ),
               # build small markdown table
               {
                 rows <- sapply(both_pos, function(o){
                   a <- autoc_df[autoc_df[[1]]==o,"RATE Estimate"]; al <- autoc_df[autoc_df[[1]]==o,"2.5%"]; ah <- autoc_df[autoc_df[[1]]==o,"97.5%"]
                   q <- qini_df[qini_df[[1]]==o,"RATE Estimate"]; ql <- qini_df[qini_df[[1]]==o,"2.5%"]; qh <- qini_df[qini_df[[1]]==o,"97.5%"]
                   sprintf("| %s | %.3f (%.3f,%.3f) | %.3f (%.3f,%.3f) |",
                           o, a, al, ah, q, ql, qh)
                 })
                 c("| Outcome | AUTOC (95% CI) | QINI (95% CI) |",
                   "|:--------|:---------------|:-------------|",
                   rows)
               }
    )
  }

  # concordant negative
  if (length(both_neg)>0) {
    parts <- c(parts,
               paste0(
                 "both methods yield negative estimates for: ", paste(both_neg, collapse=", "),
                 ". targeting by c-ate would underperform ate."
               )
    )
  }

  # discordant
  if (length(auto_only_pos)+length(qini_only_pos)+length(auto_only_neg)+length(qini_only_neg) > 0) {
    discord <- c()
    if (length(auto_only_pos)>0)
      discord <- c(discord,
                   paste0("AUTOC only positive: ", paste(auto_only_pos, collapse=", "))
      )
    if (length(qini_only_pos)>0)
      discord <- c(discord,
                   paste0("QINI only positive: ", paste(qini_only_pos, collapse=", "))
      )
    if (length(auto_only_neg)>0)
      discord <- c(discord,
                   paste0("AUTOC only negative: ", paste(auto_only_neg, collapse=", "))
      )
    if (length(qini_only_neg)>0)
      discord <- c(discord,
                   paste0("QINI only negative: ", paste(qini_only_neg, collapse=", "))
      )
    parts <- c(parts,
               "discordant findings indicate sensitivity to weighting scheme:",
               paste(discord, collapse="; "), "."
    )
  }

  # nuanced recommendation
  parts <- c(parts,
             "#### policy note",
             "overlap indicates robust heterogeneity but does not mandate one scheme over the other. ",
             "choose AUTOC to intensify benefit for top responders or QINI to maximise aggregate gain."
  )

  comparison_text <- paste(parts, collapse="\n\n")

  list(
    comparison    = comparison_text,
    autoc_results = margot_interpret_rate(autoc_df, flipped_outcomes, target="AUTOC"),
    qini_results  = margot_interpret_rate(qini_df,  flipped_outcomes, target="QINI")
  )
}
