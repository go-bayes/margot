# ────────────────────────────────────────────────────────────────────────────
# 1. Interpret a single AUTOC or QINI table
# ────────────────────────────────────────────────────────────────────────────
#' Interpret RATE estimates
#'
#' Produce a concise Markdown summary of which outcomes show positive,
#' negative, or inconclusive heterogeneous treatment effects under a given
#' **policy** (“treat_best” or “withhold_best”).
#'
#' @param rate_df A data frame returned by `margot_rate()` *or* the list that
#'   `margot_rate()` returns (in which case the call is dispatched to the
#'   comparison method).
#' @param flipped_outcomes Character vector of outcomes that were inverted
#'   during preprocessing (kept for API symmetry; unused here).
#' @param target Character. `"AUTOC"` or `"QINI"` (ignored when `rate_df` is the
#'   list of tables).
#' @return Markdown string (single table) or the comparison list (two tables).
#' @keywords internal
margot_interpret_rate <- function(rate_df,
                                  flipped_outcomes = NULL,
                                  target = "AUTOC") {

  ## ── dispatch to comparison helper ───────────────────────────────────────
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
    return(
      margot_interpret_rate_comparison(
        autoc_df        = rate_df$rate_autoc,
        qini_df         = rate_df$rate_qini,
        flipped_outcomes = flipped_outcomes
      )
    )
  }

  ## ── basic checks ────────────────────────────────────────────────────────
  stopifnot(target %in% c("AUTOC", "QINI"))
  req <- c("RATE Estimate", "2.5%", "97.5%")
  if (!all(req %in% names(rate_df)))
    stop("rate_df must contain the columns: ", paste(req, collapse = ", "))

  ## ── isolate one policy (if present) ─────────────────────────────────────
  if ("policy" %in% names(rate_df)) {
    pol <- unique(rate_df$policy)
    if (length(pol) > 1)
      stop("rate_df mixes different policies; filter before calling.")
    rate_df <- dplyr::filter(rate_df, policy == pol)
    policy_string <- ifelse(pol == "treat_best",
                            "treat best responders",
                            "withhold best responders")
  } else {
    policy_string <- "treat best responders"   # legacy default
  }

  ## ── identify signals ────────────────────────────────────────────────────
  out   <- rate_df[[1]]
  lo    <- rate_df$`2.5%`
  hi    <- rate_df$`97.5%`
  pos   <- which(lo > 0)
  neg   <- which(hi < 0)
  incon <- setdiff(seq_along(out), c(pos, neg))

  ## ── construct narrative ────────────────────────────────────────────────
  bullets <- list(
    paste0("### Evidence for heterogeneous treatment effects (policy = ",
           policy_string, ") via ", target),
    if (target == "AUTOC")
      "AUTOC uses logarithmic weighting, emphasising the top responders."
    else
      "QINI uses linear weighting, balancing effect size and prevalence."
  )

  if (length(pos)) {
    bullets <- c(
      bullets,
      paste0("**Positive RATE** – CATE targeting outperforms ATE for: ",
             paste(out[pos], collapse = ", "), ".")
    )
  }
  if (length(neg)) {
    bullets <- c(
      bullets,
      paste0("**Negative RATE** – CATE targeting would *underperform* ATE for: ",
             paste(out[neg], collapse = ", "), ".")
    )
  }
  if (length(incon)) {
    bullets <- c(
      bullets,
      paste0(
        "Evidence remains inconclusive for: ",
        paste(out[incon], collapse = ", "),
        " (95 % CI crosses zero)."
      )
    )
  }
  paste(bullets, collapse = "\n\n")
}

# ────────────────────────────────────────────────────────────────────────────
# 2. Compare AUTOC vs QINI for the **same** policy
# ────────────────────────────────────────────────────────────────────────────
#' Compare and interpret RATE estimates from AUTOC and QINI
#'
#' @param autoc_df Data frame produced by `margot_rate()` for AUTOC.
#' @param qini_df  Data frame produced by `margot_rate()` for QINI.
#' @param flipped_outcomes Character vector of inverted outcomes (unused here).
#' @return A list: markdown comparison, plus the two individual summaries.
#' @keywords internal
margot_interpret_rate_comparison <- function(autoc_df,
                                             qini_df,
                                             flipped_outcomes = NULL) {

  req <- c("RATE Estimate", "2.5%", "97.5%")
  if (!all(req %in% names(autoc_df)) || !all(req %in% names(qini_df)))
    stop("Both data frames must have RATE Estimate, 2.5%, 97.5% columns.")

  ## ── harmonise by policy ────────────────────────────────────────────────
  if ("policy" %in% names(autoc_df)) {
    pol <- unique(autoc_df$policy)
    if (length(pol) != 1 ||
        !all(pol %in% qini_df$policy))
      stop("AUTOC and QINI tables refer to different policies.")
    autoc_df <- dplyr::filter(autoc_df, policy == pol)
    qini_df  <- dplyr::filter(qini_df,  policy == pol)
    pol_str  <- ifelse(pol == "treat_best",
                       "treat best responders",
                       "withhold best responders")
  } else {
    pol_str  <- "treat best responders"
  }

  ## ── sets of significant outcomes ───────────────────────────────────────
  pos_auto <- autoc_df[[1]][autoc_df$`2.5%` >  0]
  pos_qini <- qini_df [[1]][qini_df $`2.5%` >  0]
  neg_auto <- autoc_df[[1]][autoc_df$`97.5%` < 0]
  neg_qini <- qini_df [[1]][qini_df $`97.5%` < 0]

  both_pos <- intersect(pos_auto, pos_qini)
  both_neg <- intersect(neg_auto, neg_qini)

  ## ── narrative comparison ───────────────────────────────────────────────
  txt <- c(
    paste0("### AUTOC vs QINI (policy = ", pol_str, ")"),
    "* AUTOC — logarithmic weighting (top-tail focus)",
    "* QINI  — linear weighting (aggregate gain)"
  )

  if (length(both_pos)) {
    txt <- c(txt,
             paste0("Both methods **agree on positive RATE** for: ",
                    paste(both_pos, collapse = ", "), "."))
  }
  if (length(both_neg)) {
    txt <- c(txt,
             paste0("Both methods **agree on negative RATE** for: ",
                    paste(both_neg, collapse = ", "), "."))
  }

  discord <- setdiff(
    union(pos_auto, neg_auto, pos_qini, neg_qini),
    union(both_pos,  both_neg)
  )
  if (length(discord)) {
    txt <- c(txt,
             "Some outcomes are sensitive to the weighting scheme:",
             paste(discord, collapse = ", "), ".")
  }

  list(
    comparison    = paste(txt, collapse = "\n\n"),
    autoc_results = margot_interpret_rate(autoc_df, flipped_outcomes, target = "AUTOC"),
    qini_results  = margot_interpret_rate(qini_df,  flipped_outcomes, target = "QINI")
  )
}
# OLD
#' margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
#'   # Check if rate_df is a list containing both methods
#'   if (is.list(rate_df) && !is.data.frame(rate_df) &&
#'       (("rate_result" %in% names(rate_df) && "rate_qini" %in% names(rate_df)) ||
#'        ("rate_autoc" %in% names(rate_df) && "rate_qini" %in% names(rate_df)))) {
#'
#'     # Get the correct AUTOC data frame (either rate_result or rate_autoc)
#'     autoc_df <- if ("rate_autoc" %in% names(rate_df)) rate_df$rate_autoc else rate_df$rate_result
#'     qini_df <- rate_df$rate_qini
#'
#'     # Call the comparison function instead
#'     return(margot_interpret_rate_comparison(autoc_df, qini_df, flipped_outcomes))
#'   }
#'
#'   # Original function for single method continues here
#'   # validate target parameter
#'   if (!target %in% c("AUTOC", "QINI")) {
#'     stop("target must be either 'AUTOC' or 'QINI'.")
#'   }
#'
#'   # check for required columns
#'   required_cols <- c("RATE Estimate", "2.5%", "97.5%")
#'   if (!all(required_cols %in% colnames(rate_df))) {
#'     stop("rate_df must contain columns 'RATE Estimate', '2.5%', and '97.5%'.")
#'   }
#'
#'   # outcome names are assumed to be in the first column
#'   outcome_names <- rate_df[[1]]
#'
#'   # ensure flipped_outcomes is always a character vector
#'   if (!is.null(flipped_outcomes)) {
#'     if (!is.vector(flipped_outcomes)) {
#'       flipped_outcomes <- c(as.character(flipped_outcomes))
#'     } else {
#'       flipped_outcomes <- as.character(flipped_outcomes)
#'     }
#'   }
#'
#'   # determine significance: significant if the 95% CI doesn't include zero
#'   is_significant <- (rate_df[,"2.5%"] > 0) | (rate_df[,"97.5%"] < 0)
#'   sig_idx <- which(is_significant)
#'
#'   # separate positive and negative significant effects
#'   pos_sig_idx <- which(rate_df[,"2.5%"] > 0)
#'   neg_sig_idx <- which(rate_df[,"97.5%"] < 0)
#'
#'   # create the main heading with the method specified
#'   interpretation_parts <- list(paste0("### Evidence for Heterogeneous Treatment Effects (", target, " Method)"))
#'
#'   # brief explanation of RATE based on selected target
#'   if (target == "AUTOC") {
#'     target_explanation <- paste0(
#'       "The rank-weighted average treatment effect (RATE) identifies subgroups of individuals ",
#'       "with different responses to treatment. We used the AUTOC (Area Under the TOC curve) targeting method, ",
#'       "which uses logarithmic weighting and is most effective when heterogeneity is concentrated in a smaller ",
#'       "subset of the population (typically ≤ 10%). AUTOC provides better statistical power when only a ",
#'       "small fraction of individuals experiences non-zero heterogeneous treatment effects."
#'     )
#'   } else { # QINI
#'     target_explanation <- paste0(
#'       "The rank-weighted average treatment effect (RATE) identifies subgroups of individuals ",
#'       "with different responses to treatment. We used the QINI targeting method, which uses linear weighting ",
#'       "and is most effective when heterogeneity is broadly distributed across a substantial portion of the population ",
#'       "(typically > 50%). The Qini coefficient measures cumulative benefits as we increase the treatment fraction ",
#'       "and provides better statistical power when treatment effects are diffuse across the population."
#'     )
#'   }
#'   interpretation_parts[[length(interpretation_parts) + 1]] <- target_explanation
#'
#'   # handle positive outcomes as a group
#'   if (length(pos_sig_idx) > 0) {
#'     pos_outcome_names <- outcome_names[pos_sig_idx]
#'
#'     # create a subsection for positive outcomes
#'     pos_header <- paste0("#### Evidence for Prioritising using CATE (", target, ")")
#'
#'     # list the outcomes with positive effects
#'     pos_outcomes_list <- paste(pos_outcome_names, collapse = ", ")
#'
#'     pos_interpretation <- paste0(
#'       "For the following outcome(s): ", pos_outcomes_list, ", the analysis shows reliably positive RATE estimates ",
#'       "(95% confidence intervals entirely above zero). This indicates that targeting treatment ",
#'       "based on predicted effects would lead to better outcomes than using the average treatment effect (ATE). ",
#'       "For these outcomes, targeting the CATE is expected to lead to reliably better outcomes and ",
#'       "is recommended."
#'     )
#'
#'     # Add estimates for each positive outcome
#'     pos_estimates <- paste0("Specific Estimates (", target, "):")
#'     for (i in pos_sig_idx) {
#'       outcome <- outcome_names[i]
#'       rate_est <- rate_df[i, "RATE Estimate"]
#'       ci_lower <- rate_df[i, "2.5%"]
#'       ci_upper <- rate_df[i, "97.5%"]
#'
#'       is_flipped <- FALSE
#'       if (!is.null(flipped_outcomes)) {
#'         outcome_str <- gsub("\\*", "", as.character(outcome))
#'         flipped_clean <- gsub("\\*", "", flipped_outcomes)
#'         is_flipped <- outcome_str %in% flipped_clean
#'       }
#'
#'       flip_note <- if (is_flipped) " (after inverting the outcome)" else ""
#'       pos_estimates <- paste0(
#'         pos_estimates, "\n- ", outcome, ": ",
#'         sprintf("%.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper), flip_note
#'       )
#'     }
#'
#'     pos_section <- paste(pos_header, pos_interpretation, pos_estimates, sep = "\n\n")
#'     interpretation_parts[[length(interpretation_parts) + 1]] <- pos_section
#'   }
#'
#'   # handle reliably negative outcomes as a group with a caution
#'   if (length(neg_sig_idx) > 0) {
#'     neg_outcome_names <- outcome_names[neg_sig_idx]
#'
#'     # create a subsection for negative outcomes
#'     neg_header <- paste0("#### Caution Against Prioritising using CATE (", target, ")")
#'
#'     # list the outcomes with negative effects
#'     neg_outcomes_list <- paste(neg_outcome_names, collapse = ", ")
#'
#'     neg_interpretation <- paste0(
#'       "For the following outcome(s): ", neg_outcomes_list, ", the analysis shows reliably negative RATE estimates ",
#'       "(95% confidence intervals entirely below zero). This is an important caution that targeting treatment ",
#'       "based on predicted effects would lead to worse outcomes than using the average treatment effect (ATE). ",
#'       "For these outcomes, targeting the CATE is expected to lead to reliably worse outcomes and ",
#'       "is not recommended."
#'     )
#'
#'     # Add estimates for each negative outcome
#'     neg_estimates <- paste0("Specific Estimates (", target, "):")
#'     for (i in neg_sig_idx) {
#'       outcome <- outcome_names[i]
#'       rate_est <- rate_df[i, "RATE Estimate"]
#'       ci_lower <- rate_df[i, "2.5%"]
#'       ci_upper <- rate_df[i, "97.5%"]
#'
#'       is_flipped <- FALSE
#'       if (!is.null(flipped_outcomes)) {
#'         outcome_str <- gsub("\\*", "", as.character(outcome))
#'         flipped_clean <- gsub("\\*", "", flipped_outcomes)
#'         is_flipped <- outcome_str %in% flipped_clean
#'       }
#'
#'       flip_note <- if (is_flipped) " (after inverting the outcome)" else ""
#'       neg_estimates <- paste0(
#'         neg_estimates, "\n- ", outcome, ": ",
#'         sprintf("%.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper), flip_note
#'       )
#'     }
#'
#'     neg_section <- paste(neg_header, neg_interpretation, neg_estimates, sep = "\n\n")
#'     interpretation_parts[[length(interpretation_parts) + 1]] <- neg_section
#'   }
#'
#'   # check if any outcomes are significant (either positive or negative)
#'   if (length(sig_idx) == 0) {
#'     # no significant outcomes at all
#'     no_sig_text <- "No statistically significant evidence of treatment effect heterogeneity was detected for any outcome (all 95% confidence intervals crossed zero)."
#'     interpretation_parts[[length(interpretation_parts) + 1]] <- no_sig_text
#'   } else if (sum(is_significant) < length(outcome_names)) {
#'     # some outcomes are not significant
#'     non_sig_idx <- which(!is_significant)
#'     non_sig_outcomes <- outcome_names[non_sig_idx]
#'
#'     if (length(non_sig_outcomes) > 0) {
#'       non_sig_text <- paste0(
#'         "For the remaining outcome(s): ", paste(non_sig_outcomes, collapse = ", "),
#'         ", no statistically significant evidence of treatment effect heterogeneity was detected ",
#'         "(95% confidence intervals crossed zero)."
#'       )
#'       interpretation_parts[[length(interpretation_parts) + 1]] <- non_sig_text
#'     }
#'   }
#'
#'   interpretation_text <- paste(interpretation_parts, collapse = "\n\n")
#'   return(interpretation_text)
#' }
