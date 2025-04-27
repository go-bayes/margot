#' Format RATE results into readable tables, keeping raw model names
#'
#' Assemble two tables that summarise rank-average treatment effect (RATE)
#' estimates from causal-forest models under two targeting operating
#' characteristics (TOC): **AUTOC** and **Qini**.  Each table retains the raw
#' model names but maps them to human-readable outcome labels and appends 95\%
#' Wald confidence intervals.
#'
#' @details
#' Outcome labels are created with `transform_var_name()`.
#' Setting `highlight_significant = TRUE` wraps the outcome label in **bold**
#' Markdown when the 95 \% confidence interval lies strictly above zero, making
#' it easy to emphasise robust results in Quarto or R Markdown documents.
#'
#' @param models  A list returned by `margot_multi_arm_causal_forest()` that
#'   contains an element `results`, itself a named list of model fits.
#' @param label_mapping Named character vector mapping raw variable names to
#'   presentation labels.
#' @param remove_tx_prefix Logical. Drop a leading `t0_` from variable names?
#' @param remove_z_suffix Logical. Drop a trailing `_z` from variable names?
#' @param use_title_case Logical. Convert labels to title case?
#' @param remove_underscores Logical. Replace underscores with spaces?
#' @param round_digits Integer. Decimal places to keep (default = 3).
#' @param highlight_significant Logical. Bold labels whose 95 \% CI is entirely
#'   above zero.
#'
#' @return A named list with two data frames
#' \describe{
#'   \item{rate_autoc}{Data frame with columns
#'     \code{model}, \code{outcome}, \code{RATE Estimate},
#'     \code{Std Error}, \code{2.5\%}, \code{97.5\%}.}
#'   \item{rate_qini}{Identical structure, estimated with Qini weights.}
#' }
#'
#' @examples
#' \dontrun{
#' rate <- margot_rate(my_models)
#' knitr::kable(rate$rate_autoc)
#' }
#' @export
margot_rate <- function(models,
                        label_mapping      = NULL,
                        remove_tx_prefix   = TRUE,
                        remove_z_suffix    = TRUE,
                        use_title_case     = TRUE,
                        remove_underscores = TRUE,
                        round_digits       = 3,
                        highlight_significant = TRUE) {

  build_table <- function(field) {
    rows <- lapply(names(models$results), function(mod_name) {
      res <- models$results[[mod_name]][[field]]
      res <- res[setdiff(names(res), c("TOC", "target"))]
      res <- lapply(res, function(x) {
        if (is.numeric(x)) x <- round(x, round_digits)
        if (length(x) > 1) x[1] else x
      })
      label <- transform_var_name(
        mod_name, label_mapping,
        remove_tx_prefix   = remove_tx_prefix,
        remove_z_suffix    = remove_z_suffix,
        use_title_case     = use_title_case,
        remove_underscores = remove_underscores
      )
      df <- data.frame(
        model   = mod_name,
        outcome = label,
        stringsAsFactors = FALSE
      )
      for (nm in names(res)) df[[nm]] <- res[[nm]]
      df
    })
    tab <- do.call(rbind, rows)
    rownames(tab) <- NULL
    names(tab)[names(tab) == "estimate"] <- "RATE Estimate"
    names(tab)[names(tab) == "std.err"]  <- "Std Error"
    if (all(c("RATE Estimate", "Std Error") %in% names(tab))) {
      tab$`2.5%`  <- round(tab$`RATE Estimate` - 1.96 * tab$`Std Error`, round_digits)
      tab$`97.5%` <- round(tab$`RATE Estimate` + 1.96 * tab$`Std Error`, round_digits)
    }
    if (highlight_significant && all(c("2.5%", "97.5%") %in% names(tab))) {
      sig <- which(tab$`2.5%` > 0 & tab$`97.5%` > 0)
      tab$outcome[sig] <- paste0("**", tab$outcome[sig], "**")
    }
    tab
  }

  list(
    rate_autoc = build_table("rate_result"),
    rate_qini  = build_table("rate_qini")
  )
}

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
#' @export
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

  header <- paste0(
    "### Targeting operating characteristic (TOC) by rank average treatment effect (RATE): ",
    target
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

#' Compare and interpret RATE estimates from AUTOC and Qini
#'
#' @keywords internal
#' @param autoc_df Data frame of AUTOC results produced by `margot_rate()`.
#' @param qini_df  Data frame of Qini  results produced by `margot_rate()`.
#' @param flipped_outcomes Character vector of outcomes inverted during preprocessing.
#' @return A list with comparison text, individual summaries, and model name sets.
#' @export
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
