#' Interpret RATE estimates
#'
#' Produce a compact Markdown summary describing which outcomes show positive,
#' negative, or inconclusive heterogeneous treatment effects.
#'
#' @param rate_df A data frame from margot_rate() or a list containing
#'   rate_autoc and rate_qini.
#' @param flipped_outcomes Character vector of outcomes inverted during
#'   preprocessing (unused here but kept for API symmetry).
#' @param target Character; either "AUTOC" or "QINI" (ignored when rate_df
#'   is a list).
#' @return If rate_df is a data frame, a Markdown string. If rate_df is a list,
#'   returns a list produced by margot_interpret_rate_comparison().
#' @keywords export
margot_interpret_rate <- function(rate_df,
                                  flipped_outcomes = NULL,
                                  target = "AUTOC") {
  # handle comparison case
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
    return(
      margot_interpret_rate_comparison(
        rate_df$rate_autoc, rate_df$rate_qini, flipped_outcomes
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

  # header and description
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

  low   <- rate_df$`2.5%`
  high  <- rate_df$`97.5%`
  pos   <- which(low  > 0)
  neg   <- which(high < 0)
  incon <- setdiff(seq_len(nrow(rate_df)), union(pos, neg))

  parts <- list(header, desc)

  # positive effects
  if (length(pos) > 0) {
    labs <- rate_df$outcome[pos]
    ests <- sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      labs, rate_df$`RATE Estimate`[pos], low[pos], high[pos]
    )
    parts <- c(
      parts,
      sprintf("Positive RATE estimates for: %s.", paste(labs, collapse = ", ")),
      sprintf("Estimates (%s) show robust heterogeneity.", paste(ests, collapse = "; "))
    )
  }

  # negative effects
  if (length(neg) > 0) {
    labs <- rate_df$outcome[neg]
    ests <- sprintf(
      "%s: %.3f (95%% CI %.3f, %.3f)",
      labs, rate_df$`RATE Estimate`[neg], low[neg], high[neg]
    )
    parts <- c(
      parts,
      sprintf("Negative RATE estimates for: %s.", paste(labs, collapse = ", ")),
      sprintf("Estimates (%s) caution against CATE prioritisation.",
              paste(ests, collapse = "; "))
    )
  }

  # inconclusive
  if (length(incon) > 0) {
    parts <- c(
      parts,
      sprintf(
        "For outcomes with 95%% CI crossing zero (%s), evidence is inconclusive.",
        paste(rate_df$outcome[incon], collapse = ", ")
      )
    )
  }

  paste(parts, collapse = "\n\n")
}

#' Compare and interpret RATE estimates from AUTOC and QINI
#'
#' @param autoc_df Data frame of AUTOC results from margot_rate().
#' @param qini_df Data frame of QINI results from margot_rate().
#' @param flipped_outcomes Character vector of outcomes inverted during preprocessing.
#' @return A list with elements:
#' * comparison: Markdown comparison text
#' * autoc_results: Output of margot_interpret_rate() for AUTOC
#' * qini_results: Output of margot_interpret_rate() for QINI
#' * autoc_model_names: Models with positive AUTOC
#' * qini_model_names: Models with positive QINI
#' * both_model_names: Models positive in both
#' * either_model_names: Models positive in either
#' @keywords internal
margot_interpret_rate_comparison <- function(autoc_df,
                                             qini_df,
                                             flipped_outcomes = NULL) {
  req <- c("model", "outcome", "RATE Estimate", "2.5%", "97.5%")
  if (!all(req %in% names(autoc_df)) || !all(req %in% names(qini_df))) {
    stop("data frames must include columns: ", paste(req, collapse = ", "))
  }

  pos_autoc  <- autoc_df$model[autoc_df$`2.5%` > 0]
  pos_qini   <- qini_df$model[qini_df$`2.5%` > 0]
  autoc_models  <- unique(pos_autoc)
  qini_models   <- unique(pos_qini)
  both_models   <- intersect(autoc_models, qini_models)
  either_models <- union(autoc_models, qini_models)

  label_map <- setNames(autoc_df$outcome, autoc_df$model)

  parts <- list(
    "### Comparison of targeting operating characteristic (TOC) by rank average treatment effect (RATE): AUTOC vs QINI",
    "We applied two TOC by RATE methods to the same causal-forest \\tau(x) estimates:",
    "- **AUTOC** intensifies focus on top responders via logarithmic weighting.",
    "- **QINI** balances effect size and prevalence via linear weighting."
  )

  if (length(both_models) > 0) {
    parts <- c(
      parts,
      sprintf("Both methods yield positive RATE estimates for: %s.",
              paste(label_map[both_models], collapse = ", ")),
      "This concordance indicates robust evidence of treatment effect heterogeneity."
    )
  }

  pos_only_a <- setdiff(autoc_models, qini_models)
  pos_only_q <- setdiff(qini_models, autoc_models)
  if (length(pos_only_a) > 0 || length(pos_only_q) > 0) {
    desc_parts <- character()
    if (length(pos_only_a) > 0) {
      desc_parts <- c(desc_parts, sprintf(
        "only AUTOC yields a positive RATE for %s",
        paste(label_map[pos_only_a], collapse = ", ")
      ))
    }
    if (length(pos_only_q) > 0) {
      desc_parts <- c(desc_parts, sprintf(
        "only QINI yields a positive RATE for %s",
        paste(label_map[pos_only_q], collapse = ", ")
      ))
    }
    parts <- c(
      parts,
      sprintf(
        "When QINI and AUTOC disagree on positive RATE (%s), choose **QINI** to maximise overall benefit or **AUTOC** to focus on top responders.",
        paste(desc_parts, collapse = "; ")
      )
    )
  }

  if (length(either_models) == 0) {
    parts <- c(
      parts,
      "Neither TOC AUTOC nor TOC QINI yields a statistically significant RATE for any outcome; evidence is inconclusive."
    )
  }

  list(
    comparison          = paste(parts, collapse = "\n\n"),
    autoc_results       = margot_interpret_rate(autoc_df, flipped_outcomes, "AUTOC"),
    qini_results        = margot_interpret_rate(qini_df, flipped_outcomes, "QINI"),
    autoc_model_names   = autoc_models,
    qini_model_names    = qini_models,
    both_model_names    = both_models,
    either_model_names  = either_models
  )
}

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
#' @param policy Character; "treat_best" (default) or "withhold_best".
#' @param round_digits Integer; decimal places (default 3).
#' @param highlight_significant Logical; bold outcomes whose 95 percent CI excludes 0 (default TRUE).
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param remove_tx_prefix Logical; remove treatment prefix from variable names (default TRUE).
#' @param remove_z_suffix Logical; remove z-score suffix from variable names (default TRUE).
#' @param use_title_case Logical; convert variable names to title case (default TRUE).
#' @param remove_underscores Logical; replace underscores with spaces (default TRUE).
#' @param seed Integer; base seed for reproducible RATE computations (default 12345).
#' @return A list with elements:
#' * rate_autoc: AUTOC RATE table
#' * rate_qini: QINI RATE table
#' @export
margot_rate <- function(models,
                        policy = c("treat_best", "withhold_best"),
                        round_digits = 3,
                        highlight_significant = TRUE,
                        label_mapping = NULL,
                        remove_tx_prefix = TRUE,
                        remove_z_suffix = TRUE,
                        use_title_case = TRUE,
                        remove_underscores = TRUE,
                        seed = 12345) {
  policy <- match.arg(policy)

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

    if (highlight_significant) {
      sig <- with(tab, `2.5%` > 0 | `97.5%` < 0)
      tab$outcome[sig] <- paste0("**", tab$outcome[sig], "**")
    }

    tab %>% dplyr::arrange(dplyr::desc(`RATE Estimate`))
  }

  list(
    rate_autoc = make_tbl("AUTOC"),
    rate_qini  = make_tbl("QINI")
  )
}
