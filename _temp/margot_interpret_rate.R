#' Interpret RATE estimates
#'
#' @title Interpret RATE estimates
#' @description
#' Given a data‐frame (or list) of RATE results, assemble a compact
#' markdown summary of which outcomes have positive, negative, or
#' inconclusive heterogeneity.
#'
#' @param rate_df     a data.frame from margot_rate() or a list with components rate_autoc and rate_qini
#' @param flipped_outcomes  character vector of outcomes inverted during preprocessing
#' @param target      one of "AUTOC" or "Qini" (ignored if rate_df is a list)
#' @return a markdown string (for data.frames) or a comparison list (for lists)
#' @importFrom stats qnorm
#' @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  # if both methods present, delegate to comparison
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc","rate_qini") %in% names(rate_df))) {
    return(margot_interpret_rate_comparison(
      rate_df$rate_autoc, rate_df$rate_qini, flipped_outcomes
    ))
  }
  # otherwise treat as single-method data.frame
  required <- c("RATE Estimate","2.5%","97.5%","outcome")
  if (!all(required %in% names(rate_df))) {
    stop("rate_df must contain RATE Estimate, 2.5%, 97.5%, and outcome columns")
  }
  # headings
  header <- paste0(
    "### Targeting operating characteristic (TOC) by rank average treatment effect (RATE): ",
    target
  )
  desc <- if (target == "AUTOC") {
    "AUTOC uses logarithmic weighting to focus treatment on top responders."
  } else {
    "Qini uses linear weighting to balance effect size and prevalence for aggregate gain."
  }
  low  <- rate_df$`2.5%`
  high <- rate_df$`97.5%`
  pos  <- which(low  > 0)
  neg  <- which(high < 0)
  all  <- seq_len(nrow(rate_df))
  incon <- setdiff(all, union(pos, neg))

  parts <- list(header, desc)
  if (length(pos) > 0) {
    labs <- rate_df$outcome[pos]
    ests <- sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      labs, rate_df$`RATE Estimate`[pos], low[pos], high[pos]
    )
    parts <- c(parts,
               paste0("Positive RATE estimates for: ", paste(labs, collapse = ", "), "."),
               paste0("Estimates (", paste(ests, collapse = "; "), ") show robust heterogeneity.")
    )
  }
  if (length(neg) > 0) {
    labs <- rate_df$outcome[neg]
    ests <- sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      labs, rate_df$`RATE Estimate`[neg], low[neg], high[neg]
    )
    parts <- c(parts,
               paste0("Negative RATE estimates for: ", paste(labs, collapse = ", "), "."),
               paste0("Estimates (", paste(ests, collapse = "; "), ") caution against CATE prioritisation.")
    )
  }
  if (length(incon) > 0) {
    parts <- c(parts,
               paste0(
                 "For outcomes with 95% CI crossing zero (",
                 paste(rate_df$outcome[incon], collapse = ", "),
                 "), evidence is inconclusive."
               )
    )
  }

  # return the assembled text
  paste(parts, collapse = "\n\n")
}

#’ Compare and interpret RATE estimates from both AUTOC and Qini, extracting model names
#'
#’ @keywords internal
#' Compare and interpret RATE estimates from both AUTOC and Qini, extracting model names
#'
#' @keywords internal
margot_interpret_rate_comparison <- function(autoc_df, qini_df, flipped_outcomes = NULL) {
  #' ensure required columns
  req <- c("model", "outcome", "RATE Estimate", "2.5%", "97.5%")
  if (!all(req %in% names(autoc_df)) || !all(req %in% names(qini_df))) {
    stop("data frames must include columns: ", paste(req, collapse = ", "))
  }

  #' positive model significance: only include models with ci entirely above zero
  pos_auto <- autoc_df$model[autoc_df$`2.5%` > 0]
  pos_qini <- qini_df$model[qini_df$`2.5%` > 0]

  #' only include positive models for targeting
  autoc_models  <- unique(pos_auto)
  qini_models   <- unique(pos_qini)
  both_models   <- intersect(autoc_models, qini_models)
  either_models <- union(autoc_models, qini_models)

  #' map raw names to labels
  label_map <- setNames(autoc_df$outcome, autoc_df$model)

  #' build comparison text
  parts <- list(
    "### Comparison of targeting operating characteristic (TOC) by rank average treatment effect (RATE): AUTOC vs Qini",
    "We applied two TOC by RATE methods to the same causal-forest $\tau(x)$ estimates:",
    "- TOC AUTOC intensifies focus on top responders via logarithmic weighting.",
    "- TOC Qini balances effect size and prevalence via linear weighting."
  )

  #' concordant positives
  if (length(both_models) > 0) {
    both_labels <- label_map[both_models]
    parts <- c(parts,
               paste0("Both methods yield positive RATE estimates for: ", paste(both_labels, collapse = ", "), "."),
               "This concordance indicates robust evidence of treatment effect heterogeneity.")
  }

  #' disagreement on positives
  pos_only_a <- setdiff(autoc_models, qini_models)
  pos_only_q <- setdiff(qini_models, autoc_models)
  if (length(pos_only_a) + length(pos_only_q) > 0) {
    desc <- character()
    if (length(pos_only_a) > 0) {
      desc <- c(desc, paste0("only AUTOC yields a positive RATE for ", paste(label_map[pos_only_a], collapse = ", ")))
    }
    if (length(pos_only_q) > 0) {
      desc <- c(desc, paste0("only Qini yields a positive RATE for ", paste(label_map[pos_only_q], collapse = ", ")))
    }
    parts <- c(parts,
               paste0(
                 "When Qini and AUTOC disagree on positive RATE (", paste(desc, collapse = "; "), "), ",
                 "choose Qini to maximise overall benefit or AUTOC to focus on top responders."
               )
    )
  }

  #' inconclusive
  if (length(either_models) == 0) {
    parts <- c(parts,
               "Neither TOC AUTOC nor TOC Qini yields a statistically significant RATE for any outcome; evidence is inconclusive."
    )
  }

  comparison_text <- paste(parts, collapse = "\n\n")

  list(
    comparison          = comparison_text,
    autoc_results       = margot_interpret_rate(autoc_df,  flipped_outcomes, "AUTOC"),
    qini_results        = margot_interpret_rate(qini_df,   flipped_outcomes, "Qini"),
    autoc_model_names   = autoc_models,
    qini_model_names    = qini_models,
    both_model_names    = both_models,
    either_model_names  = either_models
  )
}
