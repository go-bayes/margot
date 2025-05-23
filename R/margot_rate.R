#' Batch-compute RATEs for each outcome in a margot_causal_forest result
#'
#' This replaces the legacy internal RATE code and is the only function that
#' actually talks to grf. It flips the CATE vector on the fly when
#' policy is "withhold_best".
#'
#' @param model_results List returned by margot_causal_forest(), containing
#'   results and full_models.
#' @param policy Character; either "treat_best" (default) or "withhold_best".
#' @param target Character; weighting scheme: "AUTOC" (default) or "QINI".
#' @param level Numeric; Wald confidence level (default 0.95).
#' @param round_digits Integer; decimal places to keep (default 3).
#' @param model_prefix Character; common prefix on model names (default "model_").
#' @param verbose Logical; print progress with cli (default TRUE).
#' @param seed Integer; base seed for reproducible RATE computations (default 12345).
#' @importFrom grf rank_average_treatment_effect
#' @importFrom stats qnorm
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_info cli_alert_warning
#' @export
margot_rate_batch <- function(model_results,
                              policy = c("treat_best", "withhold_best"),
                              target = c("AUTOC", "QINI"),
                              level = 0.95,
                              round_digits = 3,
                              model_prefix = "model_",
                              verbose = TRUE,
                              seed = 12345) {
  stopifnot(is.list(model_results),
            all(c("results", "full_models") %in% names(model_results)))

  policy <- match.arg(policy)
  target <- match.arg(target)
  z      <- stats::qnorm(1 - (1 - level) / 2)

  say <- function(fun, msg) if (verbose) fun(msg)

  res_list <- lapply(names(model_results$results), function(mn) {
    withr::with_seed(seed + as.integer(factor(mn)), {
      say(cli::cli_alert_info,
          sprintf("↻ computing %s (%s, %s)", mn, target, policy))

      forest  <- model_results$full_models[[mn]]
      tau_hat <- model_results$results[[mn]]$tau_hat

      if (is.null(forest) || !inherits(forest, "causal_forest")) {
        say(cli::cli_alert_warning, sprintf("skip %s: no causal_forest", mn))
        return(NULL)
      }
      if (!is.numeric(tau_hat)) {
        say(cli::cli_alert_warning, sprintf("skip %s: tau_hat missing", mn))
        return(NULL)
      }

      if (policy == "withhold_best") tau_hat <- -tau_hat

      eps     <- 1e-12
      tau_adj <- tau_hat + eps * seq_along(tau_hat)

      ra <- grf::rank_average_treatment_effect(
        forest,
        tau_adj,
        target = target
      )

      est <- round(ra$estimate, round_digits)
      se  <- round(ra$std.err,   round_digits)
      ci  <- round(est + c(-1, 1) * z * se, round_digits)

      say(cli::cli_alert_info,
          sprintf("✔ %s RATE = %.3f (SE %.3f)", mn, est, se))

      tibble::tibble(
        model           = mn,
        outcome         = sub(paste0("^", model_prefix), "", mn),
        policy          = policy,
        target          = target,
        `RATE Estimate` = est,
        `Std Error`     = se,
        `2.5%`          = ci[1],
        `97.5%`         = ci[2]
      )
    })
  })

  dplyr::bind_rows(res_list)
}

#' Assemble RATE tables (AUTOC and QINI)
#'
#' Convenience wrapper around margot_rate_batch(). Returns two data frames,
#' both sorted by descending RATE Estimate, with reliable results highlighted in bold.
#'
#' @param models List from margot_causal_forest().
#' @param model_names Optional character vector specifying which models to process.
#'   Default NULL (all models).
#' @param policy Character; "treat_best" (default) or "withhold_best".
#' @param round_digits Integer; decimal places (default 3).
#' @param highlight_significant Logical; bold outcomes whose 95 percent CI excludes 0 (default TRUE).
#'   Note: Only positive significant effects are bolded, negative effects are not bolded.
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param remove_tx_prefix Logical; remove treatment prefix from variable names (default TRUE).
#' @param remove_z_suffix Logical; remove z-score suffix from variable names (default TRUE).
#' @param use_title_case Logical; convert variable names to title case (default TRUE).
#' @param remove_underscores Logical; replace underscores with spaces (default TRUE).
#' @param adjust Character; method for adjusting p-values, passed to stats::p.adjust().
#'   Options include "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
#'   Default is NULL (no adjustment).
#' @param alpha Numeric; significance threshold (default 0.05).
#' @param apply_adjustment Logical; if TRUE, compute p-values and apply adjustment method.
#'   If FALSE, just document the adjustment method without recomputing. Default is TRUE when
#'   adjust parameter is provided, FALSE otherwise.
#' @param seed Integer; base seed for reproducible RATE computations (default 12345).
#' @return A list with elements:
#' * rate_autoc: AUTOC RATE table
#' * rate_qini: QINI RATE table
#' @export
margot_rate <- function(models,
                        model_names = NULL,
                        policy = c("treat_best", "withhold_best"),
                        round_digits = 3,
                        highlight_significant = TRUE,
                        label_mapping = NULL,
                        remove_tx_prefix = TRUE,
                        remove_z_suffix = TRUE,
                        use_title_case = TRUE,
                        remove_underscores = TRUE,
                        adjust = NULL,
                        alpha = 0.05,
                        apply_adjustment = !is.null(adjust),
                        seed = 12345) {
  policy <- match.arg(policy)

  # determine which models to process
  all_models <- names(models$results)
  if (!is.null(model_names)) {
    missing <- setdiff(model_names, all_models)
    if (length(missing) > 0) {
      warning("Models not found: ", paste(missing, collapse = ", "))
    }
    selected_models <- intersect(model_names, all_models)

    # subset the models list to only include selected models
    subset_models <- list(
      results = models$results[selected_models],
      full_models = models$full_models[selected_models]
    )
    # preserve any other top-level elements from the original models list
    for (name in setdiff(names(models), c("results", "full_models"))) {
      subset_models[[name]] <- models[[name]]
    }

    models <- subset_models
  }

  make_tbl <- function(target) {
    tab <- margot_rate_batch(
      model_results = models,
      policy        = policy,
      target        = target,
      round_digits  = round_digits,
      seed          = seed
    ) %>%
      dplyr::mutate(
        outcome = vapply(
          model,
          transform_var_name,
          FUN.VALUE = character(1),
          label_mapping,
          remove_tx_prefix,
          remove_z_suffix,
          use_title_case,
          remove_underscores
        )
      )

    # If adjustment is requested and should be applied
    if (!is.null(adjust) && apply_adjustment) {
      # Calculate p-values based on z-scores (two-tailed test)
      tab <- tab %>%
        dplyr::mutate(
          z_score = `RATE Estimate` / `Std Error`,
          p_value = 2 * stats::pnorm(-abs(z_score)),
          adj_p_value = stats::p.adjust(p_value, method = adjust),
          is_significant = adj_p_value < alpha
        )

      # If highlighting is enabled, only bold positive significant results
      if (highlight_significant) {
        pos_sig <- tab$is_significant & tab$`RATE Estimate` > 0
        tab$outcome[pos_sig] <- paste0("**", tab$outcome[pos_sig], "**")
      }
    } else {
      # Standard CI-based significance, but only bold positive significant results
      if (highlight_significant) {
        pos_sig <- tab$`2.5%` > 0  # Only positive significant results
        tab$outcome[pos_sig] <- paste0("**", tab$outcome[pos_sig], "**")
      }
    }

    # Add adjustment information as attributes for interpretation functions
    if (!is.null(adjust)) {
      attr(tab, "adjust") <- adjust
      attr(tab, "alpha") <- alpha
      attr(tab, "apply_adjustment") <- apply_adjustment
      attr(tab, "total_hypotheses") <- nrow(tab)
    }

    tab %>% dplyr::arrange(dplyr::desc(`RATE Estimate`))
  }

  list(
    rate_autoc = make_tbl("AUTOC"),
    rate_qini  = make_tbl("QINI")
  )
}

#' Interpret RATE estimates
#'
#' Produce a compact Markdown summary describing which outcomes show positive,
#' negative, or inconclusive heterogeneous treatment effects.
#'
#' @param rate_df A data frame from margot_rate() or a list containing
#'   rate_autoc and rate_qini.
#' @param flipped_outcomes Character vector of outcomes inverted during
#'   preprocessing.
#' @param target Character; either "AUTOC" or "QINI" (ignored when rate_df
#'   is a list).
#' @param adjust_positives_only Logical; if TRUE, apply multiple testing correction
#'   only to positive RATEs in comparison output. Default FALSE.
#' @return If rate_df is a data frame, a Markdown string. If rate_df is a list,
#'   returns a list produced by margot_interpret_rate_comparison().
#' @export
margot_interpret_rate <- function(rate_df,
                                  flipped_outcomes = NULL,
                                  target = "AUTOC",
                                  adjust_positives_only = FALSE) {
  # comparison case: two-element list
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
    return(
      margot_interpret_rate_comparison(
        rate_df$rate_autoc,
        rate_df$rate_qini,
        flipped_outcomes,
        adjust_positives_only = adjust_positives_only
      )
    )
  }

  required <- c("RATE Estimate", "2.5%", "97.5%", "outcome")
  if (!all(required %in% names(rate_df))) {
    stop("`rate_df` must contain columns: ", paste(required, collapse = ", "))
  }

  # filter to first policy if present
  if ("policy" %in% names(rate_df)) {
    rate_df <- dplyr::filter(rate_df, policy == rate_df$policy[1])
  }

  # get adjustment attributes if any
  adjust <- attr(rate_df, "adjust")
  alpha <- attr(rate_df, "alpha")
  apply_adjustment <- attr(rate_df, "apply_adjustment")
  total_hypotheses <- attr(rate_df, "total_hypotheses")

  # determine pos/neg/inconclusive as before
  if (!is.null(adjust) && apply_adjustment && "is_significant" %in% names(rate_df)) {
    pos   <- which(rate_df$is_significant & rate_df$`RATE Estimate` > 0)
    neg   <- which(rate_df$is_significant & rate_df$`RATE Estimate` < 0)
    incon <- setdiff(seq_len(nrow(rate_df)), union(pos, neg))
  } else {
    low   <- rate_df$`2.5%`
    high  <- rate_df$`97.5%`
    pos   <- which(low > 0)
    neg   <- which(high < 0)
    incon <- setdiff(seq_len(nrow(rate_df)), union(pos, neg))
  }

  # header and base description
  header <- sprintf(
    "### Evidence for heterogeneous treatment effects (policy = %s) using %s",
    if (rate_df$policy[1] == "treat_best")
      "treat best responders" else "withhold from best responders",
    target
  )
  desc <- if (target == "AUTOC") {
    "AUTOC uses logarithmic weighting to focus treatment on top responders."
  } else {
    "QINI uses linear weighting to balance effect size and prevalence."
  }

  parts <- list(header, desc)

  # add flipped outcomes explanation if provided
  if (!is.null(flipped_outcomes) && length(flipped_outcomes) > 0) {
    flipped_text <- sprintf(
      "Note: The following outcomes were inverted during preprocessing because higher values of the exposure correspond to worse outcomes: %s.",
      paste(flipped_outcomes, collapse = ", ")
    )
    parts <- c(parts, flipped_text)
  }

  # positive effects
  if (length(pos) > 0) {
    labs <- rate_df$outcome[pos]
    ests <- sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      labs, rate_df$`RATE Estimate`[pos], rate_df$`2.5%`[pos], rate_df$`97.5%`[pos]
    )
    parts <- c(parts,
               sprintf("Positive RATE estimates for: %s.", paste(labs, collapse = ", ")),
               sprintf("Estimates (%s) show robust heterogeneity.", paste(ests, collapse = "; "))
    )
  }

  # negative effects
  if (length(neg) > 0) {
    labs <- rate_df$outcome[neg]
    ests <- sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      labs, rate_df$`RATE Estimate`[neg], rate_df$`2.5%`[neg], rate_df$`97.5%`[neg]
    )
    parts <- c(parts,
               sprintf("Negative RATE estimates for: %s.", paste(labs, collapse = ", ")),
               sprintf("Estimates (%s) caution against CATE prioritisation.",
                       paste(ests, collapse = "; "))
    )
  }

  # inconclusive
  if (length(incon) > 0) {
    significance_text <- if (!is.null(adjust) && apply_adjustment) {
      if (adjust %in% c("BH", "BY", "fdr")) {
        sprintf("adjusted p-values not meeting the FDR threshold of q = %.2f", alpha)
      } else {
        sprintf("adjusted values above threshold %.3f", alpha / total_hypotheses)
      }
    } else {
      "95% CI crossing zero"
    }
    parts <- c(parts,
               sprintf(
                 "For outcomes with %s (%s), evidence is inconclusive.",
                 significance_text,
                 paste(rate_df$outcome[incon], collapse = ", ")
               )
    )
  }

  paste(parts, collapse = "\n\n")
}

#old
# margot_interpret_rate <- function(rate_df,
#                                   flipped_outcomes = NULL,
#                                   target = "AUTOC",
#                                   adjust_positives_only = FALSE) {
#   # comparison case: two-element list
#   if (is.list(rate_df) && !is.data.frame(rate_df) &&
#       all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
#     return(
#       margot_interpret_rate_comparison(
#         rate_df$rate_autoc,
#         rate_df$rate_qini,
#         flipped_outcomes,
#         adjust_positives_only = adjust_positives_only
#       )
#     )
#   }
#
#   required <- c("RATE Estimate", "2.5%", "97.5%", "outcome")
#   if (!all(required %in% names(rate_df))) {
#     stop("`rate_df` must contain columns: ", paste(required, collapse = ", "))
#   }
#
#   # filter to first policy if present
#   if ("policy" %in% names(rate_df)) {
#     rate_df <- dplyr::filter(rate_df, policy == rate_df$policy[1])
#   }
#
#   # Get adjustment attributes if any
#   adjust <- attr(rate_df, "adjust")
#   alpha <- attr(rate_df, "alpha")
#   apply_adjustment <- attr(rate_df, "apply_adjustment")
#   total_hypotheses <- attr(rate_df, "total_hypotheses")
#
#   # Determine pos/neg/inconclusive as before
#   if (!is.null(adjust) && apply_adjustment && "is_significant" %in% names(rate_df)) {
#     pos   <- which(rate_df$is_significant & rate_df$`RATE Estimate` > 0)
#     neg   <- which(rate_df$is_significant & rate_df$`RATE Estimate` < 0)
#     incon <- setdiff(seq_len(nrow(rate_df)), union(pos, neg))
#   } else {
#     low   <- rate_df$`2.5%`
#     high  <- rate_df$`97.5%`
#     pos   <- which(low > 0)
#     neg   <- which(high < 0)
#     incon <- setdiff(seq_len(nrow(rate_df)), union(pos, neg))
#   }
#
#   # header and base description
#   header <- sprintf(
#     "### Evidence for heterogeneous treatment effects (policy = %s) using %s",
#     if (rate_df$policy[1] == "treat_best")
#       "treat best responders" else "withhold from best responders",
#     target
#   )
#   desc <- if (target == "AUTOC") {
#     "AUTOC uses logarithmic weighting to focus treatment on top responders."
#   } else {
#     "QINI uses linear weighting to balance effect size and prevalence."
#   }
#
#   parts <- list(header, desc)
#
#   # positive effects
#   if (length(pos) > 0) {
#     labs <- rate_df$outcome[pos]
#     ests <- sprintf(
#       "%s: %.3f (95%% CI %.3f, %.3f)",
#       labs, rate_df$`RATE Estimate`[pos], rate_df$`2.5%`[pos], rate_df$`97.5%`[pos]
#     )
#     parts <- c(parts,
#                sprintf("Positive RATE estimates for: %s.", paste(labs, collapse = ", ")),
#                sprintf("Estimates (%s) show robust heterogeneity.", paste(ests, collapse = "; "))
#     )
#   }
#
#   # negative effects
#   if (length(neg) > 0) {
#     labs <- rate_df$outcome[neg]
#     ests <- sprintf(
#       "%s: %.3f (95%% CI %.3f, %.3f)",
#       labs, rate_df$`RATE Estimate`[neg], rate_df$`2.5%`[neg], rate_df$`97.5%`[neg]
#     )
#     parts <- c(parts,
#                sprintf("Negative RATE estimates for: %s.", paste(labs, collapse = ", ")),
#                sprintf("Estimates (%s) caution against CATE prioritisation.",
#                        paste(ests, collapse = "; "))
#     )
#   }
#
#   # inconclusive
#   if (length(incon) > 0) {
#     significance_text <- if (!is.null(adjust) && apply_adjustment) {
#       if (adjust %in% c("BH", "BY", "fdr")) {
#         sprintf("adjusted p-values not meeting the FDR threshold of q = %.2f", alpha)
#       } else {
#         sprintf("adjusted values above threshold %.3f", alpha / total_hypotheses)
#       }
#     } else {
#       "95% CI crossing zero"
#     }
#     parts <- c(parts,
#                sprintf(
#                  "For outcomes with %s (%s), evidence is inconclusive.",
#                  significance_text,
#                  paste(rate_df$outcome[incon], collapse = ", ")
#                )
#     )
#   }
#
#   paste(parts, collapse = "\n\n")
# }


#' Compare and interpret RATE estimates from AUTOC and QINI
#'
#' @param autoc_df Data frame of AUTOC results from margot_rate().
#' @param qini_df Data frame of QINI results from margot_rate().
#' @param flipped_outcomes Character vector of outcomes inverted during preprocessing.
#' @param adjust_positives_only Logical; if TRUE, apply multiple testing correction only to positive RATE estimates (negative outcomes use unadjusted CIs). Default FALSE.
#' @return A list with elements:
#' * comparison: Markdown comparison text
#' * autoc_results: Output of margot_interpret_rate() for AUTOC
#' * qini_results: Output of margot_interpret_rate() for QINI
#' * autoc_model_names: Models with positive AUTOC
#' * qini_model_names: Models with positive QINI
#' * both_model_names: Models positive in both
#' * either_model_names: Models positive in either
#' * not_excluded_autoc_model_names: AUTOC models not reliably negative
#' * not_excluded_qini_model_names: QINI models not reliably negative
#' * not_excluded_both: Models not excluded by both AUTOC and QINI
#' * not_excluded_either: Models not excluded by either AUTOC or QINI
#' @export
margot_interpret_rate_comparison <- function(autoc_df,
                                             qini_df,
                                             flipped_outcomes = NULL,
                                             adjust_positives_only = FALSE) {
  req <- c("model", "outcome", "RATE Estimate", "2.5%", "97.5%")
  if (!all(req %in% names(autoc_df)) || !all(req %in% names(qini_df))) {
    stop("data frames must include columns: ", paste(req, collapse = ", "))
  }

  # Get adjustment attributes
  adjust <- attr(autoc_df, "adjust")
  alpha  <- attr(autoc_df, "alpha")
  apply_adjustment <- attr(autoc_df, "apply_adjustment")
  total_hypotheses <- attr(autoc_df, "total_hypotheses")

  # Determine positive and negative models
  if (!is.null(adjust) && apply_adjustment && "is_significant" %in% names(autoc_df)) {
    # positives by adjusted p-value
    pos_autoc <- autoc_df$model[autoc_df$is_significant & autoc_df$`RATE Estimate` > 0]
    pos_qini  <- qini_df$model[qini_df$is_significant & qini_df$`RATE Estimate` > 0]
    if (adjust_positives_only) {
      # negatives by unadjusted CI crossing only
      neg_autoc <- autoc_df$model[autoc_df$`97.5%` < 0]
      neg_qini  <- qini_df$model[qini_df$`97.5%` < 0]
    } else {
      # negatives also by adjusted p-value
      neg_autoc <- autoc_df$model[autoc_df$is_significant & autoc_df$`RATE Estimate` < 0]
      neg_qini  <- qini_df$model[qini_df$is_significant & qini_df$`RATE Estimate` < 0]
    }
  } else {
    # conventional CI approach
    pos_autoc <- autoc_df$model[autoc_df$`2.5%` > 0]
    pos_qini  <- qini_df$model[qini_df$`2.5%` > 0]
    neg_autoc <- autoc_df$model[autoc_df$`97.5%` < 0]
    neg_qini  <- qini_df$model[qini_df$`97.5%` < 0]
  }

  autoc_models <- unique(pos_autoc)
  qini_models <- unique(pos_qini)
  both_models <- intersect(autoc_models, qini_models)
  either_models <- union(autoc_models, pos_qini)

  # Prepare exclusion lists
  all_autoc <- unique(autoc_df$model)
  all_qini  <- unique(qini_df$model)
  not_excl_autoc  <- setdiff(all_autoc, neg_autoc)
  not_excl_qini   <- setdiff(all_qini,  neg_qini)
  not_excl_both   <- intersect(not_excl_autoc, not_excl_qini)
  not_excl_either <- union(not_excl_autoc, not_excl_qini)

  label_map <- setNames(autoc_df$outcome, autoc_df$model)

  # Build comparison narrative
  parts <- list(
    "### Comparison of targeting operating characteristic (TOC) by rank average treatment effect (RATE): AUTOC vs QINI",
    "We applied two TOC by RATE methods to the same causal-forest $\\tau(x)$ estimates:",
    "- **AUTOC** intensifies focus on top responders via logarithmic weighting.",
    "- **QINI** balances effect size and prevalence via linear weighting."
  )

  # Pre-specification note
  if (is.null(adjust) || !apply_adjustment) {
    parts <- c(parts,
               sprintf("All %d outcomes were prespecified. Interpretations based on 95%% CIs without multiple testing correction.",
                       length(all_autoc))
    )
  }

  # Adjustment details
  if (!is.null(adjust) && adjust != "none") {
    is_fdr <- adjust %in% c("BH","BY","fdr")
    if (is_fdr) {
      parts <- c(parts,
                 sprintf("Exploratory RATE analysis; controlled FDR at q=%.2f over %d outcomes.",
                         alpha, total_hypotheses)
      )
    } else {
      parts <- c(parts,
                 sprintf("Adjusted for multiple testing (%s) over %d hypotheses; threshold = %.3f.",
                         adjust, total_hypotheses, alpha/total_hypotheses)
      )
    }
  }

  # Concordance statements
  if (length(both_models)>0) {
    parts <- c(parts,
               sprintf("Both methods yield positive RATE estimates for: %s.",
                       paste(label_map[both_models], collapse=", ")),
               "This concordance indicates robust heterogeneity evidence."
    )
  }
  pos_only_a <- setdiff(autoc_models, qini_models)
  pos_only_q <- setdiff(qini_models, autoc_models)
  if (length(pos_only_a)|length(pos_only_q)) {
    desc <- c()
    if (length(pos_only_a)) desc <- c(desc,
                                      sprintf("only AUTOC yields positive RATE for %s", paste(label_map[pos_only_a], collapse=", ")))
    if (length(pos_only_q)) desc <- c(desc,
                                      sprintf("only QINI yields positive RATE for %s", paste(label_map[pos_only_q], collapse=", ")))
    parts <- c(parts,
               sprintf("When methods disagree (%s), choose **QINI** for overall benefit or **AUTOC** to focus top responders.",
                       paste(desc, collapse="; "))
    )
  }
  if (length(either_models)==0) {
    parts <- c(parts,
               "Neither AUTOC nor QINI yields positive RATE; evidence is inconclusive."
    )
  }

  comparison_text <- paste(parts, collapse="\n\n")

  list(
    comparison = comparison_text,
    autoc_results = margot_interpret_rate(autoc_df, flipped_outcomes, "AUTOC"),
    qini_results  = margot_interpret_rate(qini_df, flipped_outcomes, "QINI"),
    autoc_model_names  = autoc_models,
    qini_model_names   = qini_models,
    both_model_names   = both_models,
    either_model_names = either_models,
    not_excluded_autoc_model_names = not_excl_autoc,
    not_excluded_qini_model_names  = not_excl_qini,
    not_excluded_both              = not_excl_both,
    not_excluded_either            = not_excl_either
  )
}





# old
# margot_interpret_rate_comparison <- function(autoc_df,
#                                              qini_df,
#                                              flipped_outcomes = NULL) {
#   req <- c("model", "outcome", "RATE Estimate", "2.5%", "97.5%")
#   if (!all(req %in% names(autoc_df)) || !all(req %in% names(qini_df))) {
#     stop("data frames must include columns: ", paste(req, collapse = ", "))
#   }
#
#   # Get adjustment method, alpha, and whether adjustment was applied
#   adjust <- attr(autoc_df, "adjust")
#   alpha <- attr(autoc_df, "alpha")
#   apply_adjustment <- attr(autoc_df, "apply_adjustment")
#   total_hypotheses <- attr(autoc_df, "total_hypotheses")
#
#   # Determine significant models based on whether adjustment was applied
#   if (!is.null(adjust) && apply_adjustment && "is_significant" %in% names(autoc_df)) {
#     pos_autoc <- autoc_df$model[autoc_df$is_significant & autoc_df$`RATE Estimate` > 0]
#     pos_qini <- qini_df$model[qini_df$is_significant & qini_df$`RATE Estimate` > 0]
#   } else {
#     # Use conventional CI approach otherwise
#     pos_autoc <- autoc_df$model[autoc_df$`2.5%` > 0]
#     pos_qini <- qini_df$model[qini_df$`2.5%` > 0]
#   }
#
#   autoc_models <- unique(pos_autoc)
#   qini_models <- unique(pos_qini)
#   both_models <- intersect(autoc_models, qini_models)
#   either_models <- union(autoc_models, qini_models)
#
#   label_map <- setNames(autoc_df$outcome, autoc_df$model)
#
#   parts <- list(
#     "### Comparison of targeting operating characteristic (TOC) by rank average treatment effect (RATE): AUTOC vs QINI",
#     "We applied two TOC by RATE methods to the same causal-forest \\tau(x) estimates:",
#     "- **AUTOC** intensifies focus on top responders via logarithmic weighting.",
#     "- **QINI** balances effect size and prevalence via linear weighting."
#   )
#
#   # Add detailed adjustment information if applicable
#   if (!is.null(adjust) && adjust != "none") {
#     # Check if using FDR methods
#     is_fdr <- adjust %in% c("BH", "BY", "fdr")
#
#     if (is_fdr) {
#       # Specialized language for FDR methods
#       if (apply_adjustment) {
#         adj_part <- sprintf(
#           "We treated the RATE analysis as exploratory. All %d outcomes were prespecified. To flag promising signals we controlled the false discovery rate at q=%.2f.",
#           total_hypotheses, alpha
#         )
#       } else {
#         adj_part <- sprintf(
#           "We treated the RATE analysis as exploratory. All %d outcomes were prespecified. Models were pre-screened by controlling the false discovery rate at q=%.2f.",
#           total_hypotheses, alpha
#         )
#       }
#     } else {
#       # Map adjustment method to error rate controlled
#       error_rate_map <- list(
#         "bonferroni" = "family-wise error rate (FWER)",
#         "holm" = "family-wise error rate (FWER)",
#         "hochberg" = "family-wise error rate (FWER)",
#         "hommel" = "family-wise error rate (FWER)"
#       )
#
#       error_rate <- error_rate_map[[adjust]] %||% "error rate"
#
#       # Calculate critical value based on method
#       if (adjust == "bonferroni") {
#         critical_value <- alpha / total_hypotheses
#       } else if (adjust == "holm") {
#         critical_value <- alpha / (total_hypotheses - (1:total_hypotheses - 1)[1])
#       } else {
#         critical_value <- alpha  # Default for methods where critical value varies by rank
#       }
#
#       if (apply_adjustment) {
#         adj_part <- sprintf(
#           "Results shown are adjusted for multiple testing using the %s method (m = %d hypotheses, controlling %s, adjusted threshold = %.3f).",
#           adjust, total_hypotheses, error_rate, critical_value
#         )
#       } else {
#         adj_part <- sprintf(
#           "Results shown reflect models pre-screened with multiple testing correction using the %s method (m = %d hypotheses, controlling %s, adjusted threshold = %.3f).",
#           adjust, total_hypotheses, error_rate, critical_value
#         )
#       }
#     }
#
#     parts <- c(parts, adj_part)
#   }
#
#   if (length(both_models) > 0) {
#     parts <- c(
#       parts,
#       sprintf("Both methods yield positive RATE estimates for: %s.",
#               paste(label_map[both_models], collapse = ", ")),
#       "This concordance indicates robust evidence of treatment effect heterogeneity."
#     )
#   }
#
#   pos_only_a <- setdiff(autoc_models, qini_models)
#   pos_only_q <- setdiff(qini_models, autoc_models)
#   if (length(pos_only_a) > 0 || length(pos_only_q) > 0) {
#     desc_parts <- character()
#     if (length(pos_only_a) > 0) {
#       desc_parts <- c(desc_parts, sprintf(
#         "only AUTOC yields a positive RATE for %s",
#         paste(label_map[pos_only_a], collapse = ", ")
#       ))
#     }
#     if (length(pos_only_q) > 0) {
#       desc_parts <- c(desc_parts, sprintf(
#         "only QINI yields a positive RATE for %s",
#         paste(label_map[pos_only_q], collapse = ", ")
#       ))
#     }
#     parts <- c(
#       parts,
#       sprintf(
#         "When QINI and AUTOC disagree on positive RATE (%s), choose **QINI** to maximise overall benefit or **AUTOC** to focus on top responders.",
#         paste(desc_parts, collapse = "; ")
#       )
#     )
#   }
#
#   if (length(either_models) == 0) {
#     is_fdr <- !is.null(adjust) && adjust %in% c("BH", "BY", "fdr")
#
#     if (!is.null(adjust) && apply_adjustment) {
#       if (is_fdr) {
#         significance_phrase <- sprintf("statistically significant at the FDR threshold of q = %.2f", alpha)
#       } else {
#         significance_phrase <- sprintf("statistically significant (after %s adjustment)", adjust)
#       }
#     } else {
#       significance_phrase <- "statistically significant"
#     }
#
#     parts <- c(
#       parts,
#       sprintf("Neither TOC AUTOC nor TOC QINI yields a %s RATE for any outcome; evidence is inconclusive.",
#               significance_phrase)
#     )
#   }
#
#   list(
#     comparison         = paste(parts, collapse = "\n\n"),
#     autoc_results      = margot_interpret_rate(autoc_df, flipped_outcomes, "AUTOC"),
#     qini_results       = margot_interpret_rate(qini_df, flipped_outcomes, "QINI"),
#     autoc_model_names  = autoc_models,
#     qini_model_names   = qini_models,
#     both_model_names   = both_models,
#     either_model_names = either_models
#   )
# }
