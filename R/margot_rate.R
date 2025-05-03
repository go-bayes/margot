#' Interpret RATE estimates
#'
#' Produce a compact Markdown summary describing which outcomes show positive,
#' negative, or inconclusive heterogeneous treatment effects.
#'
#' @param rate_df A data frame from `margot_rate()` *or* a list containing
#'   `rate_autoc` and `rate_qini`.
#' @param flipped_outcomes Character vector of outcomes that were inverted during
#'   preprocessing (unused here but kept for API symmetry).
#' @param target Either `"AUTOC"` or `"Qini"` (ignored when `rate_df` is a list).
#' @return A Markdown string (single method) or a comparison list (two methods).
#' @keywords internal
margot_interpret_rate <- function(rate_df,
                                  flipped_outcomes = NULL,
                                  target = "AUTOC") {
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
    return(margot_interpret_rate_comparison(
      rate_df$rate_autoc, rate_df$rate_qini, flipped_outcomes
    ))
  }

  required <- c("RATE Estimate", "2.5%", "97.5%", "outcome")
  if (!all(required %in% names(rate_df))) {
    stop("rate_df must contain RATE Estimate, 2.5%, 97.5%, and outcome columns")
  }

  # inside margot_interpret_rate() --------------------------------------------
  # 1. filter on policy if the column exists
  if ("policy" %in% names(rate_df))
    rate_df <- dplyr::filter(rate_df, policy == unique(rate_df$policy)[1])

  # 2. adapt the explanatory header
  header <- paste0(
    "### Evidence for heterogeneous treatment effects (policy = ",
    ifelse(rate_df$policy[1] == "treat_best",
           "treat best responders",
           "withhold from best responders"), ") using ", target
  )

  desc <- if (target == "AUTOC") {
    "AUTOC uses logarithmic weighting to focus treatment on top responders."
  } else {
    "Qini uses linear weighting to balance effect size and prevalence for aggregate gain."
  }

  low   <- rate_df$`2.5%`
  high  <- rate_df$`97.5%`
  pos   <- which(low  > 0)
  neg   <- which(high < 0)
  incon <- setdiff(seq_len(nrow(rate_df)), union(pos, neg))

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
                 "For outcomes with 95%% CI crossing zero (",
                 paste(rate_df$outcome[incon], collapse = ", "),
                 "), evidence is inconclusive."
               )
    )
  }

  paste(parts, collapse = "\n\n")
}

#' @keywords internal
make_table <- function(targ) {
  tab <- margot_rate_batch(models,
                           policy       = policy,
                           target       = targ,
                           round_digits = round_digits) |>
    dplyr::mutate(
      outcome = vapply(
        model,
        transform_var_name,
        character(1),
        label_mapping,
        remove_tx_prefix,
        remove_z_suffix,
        use_title_case,
        remove_underscores
      )
    )

  if (highlight_significant) {
    sig <- which(tab$`2.5%` > 0 | tab$`97.5%` < 0)
    tab$outcome[sig] <- paste0("**", tab$outcome[sig], "**")
  }

  dplyr::arrange(tab, dplyr::desc(`RATE Estimate`))   # ← new line
}

#' Compare and interpret RATE estimates from AUTOC and Qini
#'
#' @param autoc_df Data frame of AUTOC results produced by `margot_rate()`.
#' @param qini_df  Data frame of Qini  results produced by `margot_rate()`.
#' @param flipped_outcomes Character vector of outcomes inverted during preprocessing.
#' @return A list with comparison text, individual summaries, and model name sets.
#' @keywords internal
margot_interpret_rate_comparison <- function(autoc_df,
                                             qini_df,
                                             flipped_outcomes = NULL) {
  req <- c("model", "outcome", "RATE Estimate", "2.5%", "97.5%")
  if (!all(req %in% names(autoc_df)) || !all(req %in% names(qini_df))) {
    stop("data frames must include columns: ", paste(req, collapse = ", "))
  }

  pos_auto <- autoc_df$model[autoc_df$`2.5%` > 0]
  pos_qini <- qini_df$model[qini_df$`2.5%` > 0]

  autoc_models  <- unique(pos_auto)
  qini_models   <- unique(pos_qini)
  both_models   <- intersect(autoc_models, qini_models)
  either_models <- union(autoc_models, qini_models)

  label_map <- setNames(autoc_df$outcome, autoc_df$model)

  parts <- list(
    "### Comparison of targeting operating characteristic (TOC) by rank average treatment effect (RATE): AUTOC vs Qini",
    "We applied two TOC by RATE methods to the same causal-forest $\\tau(x)$ estimates:",
    "- **AUTOC** intensifies focus on top responders via logarithmic weighting.",
    "- **Qini**  balances effect size and prevalence via linear weighting."
  )

  if (length(both_models) > 0) {
    parts <- c(parts,
               paste0("Both methods yield positive RATE estimates for: ",
                      paste(label_map[both_models], collapse = ", "), "."),
               "This concordance indicates robust evidence of treatment effect heterogeneity."
    )
  }

  pos_only_a <- setdiff(autoc_models, qini_models)
  pos_only_q <- setdiff(qini_models, autoc_models)
  if (length(pos_only_a) + length(pos_only_q) > 0) {
    desc <- character()
    if (length(pos_only_a) > 0) {
      desc <- c(desc,
                paste0("only AUTOC yields a positive RATE for ",
                       paste(label_map[pos_only_a], collapse = ", "))
      )
    }
    if (length(pos_only_q) > 0) {
      desc <- c(desc,
                paste0("only Qini yields a positive RATE for ",
                       paste(label_map[pos_only_q], collapse = ", "))
      )
    }
    parts <- c(parts,
               paste0(
                 "When Qini and AUTOC disagree on positive RATE (",
                 paste(desc, collapse = "; "),
                 "), choose **Qini** to maximise overall benefit or **AUTOC** to focus on top responders."
               )
    )
  }

  if (length(either_models) == 0) {
    parts <- c(parts,
               "Neither TOC AUTOC nor TOC Qini yields a statistically significant RATE for any outcome; evidence is inconclusive."
    )
  }

  list(
    comparison          = paste(parts, collapse = "\n\n"),
    autoc_results       = margot_interpret_rate(autoc_df, flipped_outcomes, "AUTOC"),
    qini_results        = margot_interpret_rate(qini_df,  flipped_outcomes, "Qini"),
    autoc_model_names   = autoc_models,
    qini_model_names    = qini_models,
    both_model_names    = both_models,
    either_model_names  = either_models
  )
}

#' Batch-compute RATEs for every outcome in a *margot_causal_forest* result
#'
#' This replaces the legacy "internal RATE" code and is the only function that
#' actually talks to **grf**.  It flips the CATE vector on the fly when
#' `policy = "withhold_best"`.
#'
#' @param model_results List returned by `margot_causal_forest()` (possibly
#'   flipped by `margot_flip_forests()`), containing `$results` and
#'   `$full_models`.
#' @param policy Either `"treat_best"` (default) or `"withhold_best"`.
#' @param target Weighting scheme: `"AUTOC"` (default) or `"QINI"`.
#' @param level  Wald confidence level (default 0.95).
#' @param round_digits Decimal places to keep (default 3).
#' @param model_prefix Common prefix on model names (default `"model_"`).
#' @param verbose Print progress with **cli**?
#'
#' @return A tibble with columns
#'   `model`, `outcome`, `policy`, `target`,
#'   `RATE Estimate`, `Std Error`, `2.5%`, `97.5%`.
#' @export
#' @importFrom grf rank_average_treatment_effect
#' @importFrom stats qnorm
#' @importFrom tibble tibble
#' @importFrom cli   cli_alert_info cli_alert_warning
margot_rate_batch <- function(model_results,
                              policy       = c("treat_best", "withhold_best"),
                              target       = c("AUTOC", "QINI"),
                              level        = 0.95,
                              round_digits = 3,
                              model_prefix = "model_",
                              verbose      = TRUE) {

  stopifnot(is.list(model_results),
            all(c("results", "full_models") %in% names(model_results)))

  policy <- match.arg(policy)
  target <- match.arg(target)
  z      <- qnorm(1 - (1 - level) / 2)

  say <- function(fun, msg) if (verbose) fun(msg)

  out <- lapply(names(model_results$results), function(mn) {

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

    ra <- grf::rank_average_treatment_effect(forest, tau_hat, target = target)

    est <- round(ra$estimate, round_digits)
    se  <- round(ra$std.err , round_digits)
    ci  <- round(est + c(-1, 1) * qnorm(1 - (1 - level) / 2) * se, round_digits)

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

  dplyr::bind_rows(out)
}

#' Assemble RATE tables (AUTOC and Qini)
#'
#' Convenience wrapper around `margot_rate_batch()`.  Returns two data frames,
#' both sorted by descending `RATE Estimate`, and with reliable results
#' highlighted in **bold**.
#'
#' @param models  List from `margot_causal_forest()` (possibly flipped).
#' @param policy  `"treat_best"` (default) or `"withhold_best"`.
#' @param round_digits Integer; decimal places (default 3).
#' @param highlight_significant Logical; bold outcomes whose 95% CI excludes
#'   0 (default TRUE).
#' @param label_mapping Named character vector for converting variable names to readable labels.
#' @param remove_tx_prefix Logical; remove treatment prefix from variable names (default TRUE).
#' @param remove_z_suffix Logical; remove z-score suffix from variable names (default TRUE).
#' @param use_title_case Logical; convert variable names to title case (default TRUE).
#' @param remove_underscores Logical; replace underscores with spaces (default TRUE).
#' @return List with elements `rate_autoc`, `rate_qini`.
#' @export
margot_rate <- function(models,
                        policy               = c("treat_best", "withhold_best"),
                        round_digits         = 3,
                        highlight_significant = TRUE,
                        label_mapping        = NULL,
                        remove_tx_prefix     = TRUE,
                        remove_z_suffix      = TRUE,
                        use_title_case       = TRUE,
                        remove_underscores   = TRUE) {

  policy <- match.arg(policy)

  ## local helper -----------------------------------------------------------
  make_table <- function(target) {
    tab <- margot_rate_batch(models,
                             policy       = policy,
                             target       = target,
                             round_digits = round_digits) |>
      dplyr::mutate(
        outcome = vapply(
          model,
          transform_var_name,
          character(1),
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

    dplyr::arrange(tab, dplyr::desc(`RATE Estimate`))
  }

  list(
    rate_autoc = make_table("AUTOC"),
    rate_qini  = make_table("QINI")
  )
}
