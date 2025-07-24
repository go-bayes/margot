#' Interpret RATE Estimates
#'
#' @description
#' Produces a concise summary of which outcomes show positive, negative, or 
#' inconclusive heterogeneous treatment effects based on RATE (Rank Average 
#' Treatment Effect) analysis.
#'
#' @param rate_df A data frame returned by `margot_rate()` or the list that
#'   `margot_rate()` returns (containing both rate_autoc and rate_qini).
#' @param flipped_outcomes Character vector of outcomes that were inverted
#'   during preprocessing. Optional.
#' @param target Character. "AUTOC" or "QINI" (ignored when rate_df is a
#'   list containing both).
#'
#' @return For a single table: a markdown-formatted character string summarizing
#'   the RATE results. For a list input: a list containing comparison results
#'   and individual interpretations for both AUTOC and QINI.
#'
#' @details
#' RATE quantifies how much better CATE-based targeting performs compared to
#' uniform treatment (ATE). The interpretation differs by weighting scheme:
#' \itemize{
#'   \item AUTOC: Uses logarithmic weighting, emphasizing top responders
#'   \item QINI: Uses linear weighting, balancing effect size and prevalence
#' }
#'
#' Positive RATE values indicate heterogeneity can be exploited for better
#' targeting. Negative values suggest CATE targeting would underperform uniform
#' treatment.
#'
#' @examples
#' \dontrun{
#' # Compute RATE for causal forest results
#' rate_results <- margot_rate(causal_forest_results)
#' 
#' # Interpret the results (handles both AUTOC and QINI)
#' interpretation <- margot_interpret_rate(rate_results)
#' cat(interpretation$comparison)
#' 
#' # Or interpret just AUTOC results
#' autoc_interpretation <- margot_interpret_rate(
#'   rate_results$rate_autoc,
#'   target = "AUTOC"
#' )
#' }
#'
#' @export
#' @importFrom dplyr filter
margot_interpret_rate <- function(rate_df,
                                 flipped_outcomes = NULL,
                                 target = "AUTOC") {
  
  # dispatch to comparison helper if both tables provided
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
    return(
      margot_interpret_rate_comparison(
        autoc_df = rate_df$rate_autoc,
        qini_df = rate_df$rate_qini,
        flipped_outcomes = flipped_outcomes
      )
    )
  }
  
  # basic checks
  stopifnot(target %in% c("AUTOC", "QINI"))
  req <- c("RATE Estimate", "2.5%", "97.5%", "outcome")
  if (!all(req %in% names(rate_df))) {
    stop("rate_df must contain columns: ", paste(req, collapse = ", "))
  }
  
  # isolate a single policy if present
  if ("policy" %in% names(rate_df)) {
    pol <- unique(rate_df$policy)
    if (length(pol) != 1) {
      stop("rate_df mixes multiple policies; filter before calling.")
    }
    rate_df <- dplyr::filter(rate_df, policy == pol)
    pol_txt <- ifelse(pol == "treat_best",
                     "treat best responders",
                     "withhold best responders")
  } else {
    pol_txt <- "treat best responders"  # legacy default
  }
  
  # mark inverted outcomes if specified
  if (!is.null(flipped_outcomes) && length(flipped_outcomes)) {
    w <- rate_df$outcome %in% flipped_outcomes
    rate_df$outcome[w] <- paste0(rate_df$outcome[w], " (inverted)")
  }
  
  # classify outcomes by statistical significance
  lo <- rate_df$`2.5%`
  hi <- rate_df$`97.5%`
  pos <- which(lo > 0)
  neg <- which(hi < 0)
  inc <- setdiff(seq_along(lo), c(pos, neg))
  
  # assemble markdown text
  lines <- list(
    paste0("### RATE (", target, ", policy = ", pol_txt, ")"),
    if (target == "AUTOC") {
      "AUTOC applies logarithmic weighting, sharpening focus on top responders."
    } else {
      "QINI applies linear weighting, maximising aggregate gain."
    }
  )
  
  if (length(pos)) {
    lines <- c(lines,
               paste0("**Positive RATE**  CATE targeting outperforms ATE for: ",
                      paste(rate_df$outcome[pos], collapse = ", "), "."))
  }
  if (length(neg)) {
    lines <- c(lines,
               paste0("**Negative RATE**  CATE targeting would *underperform* ATE for: ",
                      paste(rate_df$outcome[neg], collapse = ", "), "."))
  }
  if (length(inc)) {
    lines <- c(lines,
               paste0("Evidence inconclusive for: ",
                      paste(rate_df$outcome[inc], collapse = ", "),
                      " (95% CI crosses 0)."))
  }
  
  paste(lines, collapse = "\n\n")
}


#' Compare AUTOC and QINI RATE Tables
#'
#' @description
#' Internal function that compares AUTOC and QINI RATE results to identify
#' where the two weighting schemes agree or disagree on heterogeneity evidence.
#'
#' @param autoc_df Data frame from margot_rate() with AUTOC results
#' @param qini_df Data frame from margot_rate() with QINI results
#' @param flipped_outcomes Character vector of flipped outcome names (optional)
#'
#' @return A list containing:
#' \itemize{
#'   \item comparison: Markdown summary comparing AUTOC and QINI
#'   \item autoc_results: Interpretation of AUTOC results
#'   \item qini_results: Interpretation of QINI results
#'   \item autoc_model_names: Models with positive RATE in AUTOC
#'   \item qini_model_names: Models with positive RATE in QINI
#'   \item both_model_names: Models with positive RATE in both
#' }
#'
#' @keywords internal
margot_interpret_rate_comparison <- function(autoc_df,
                                           qini_df,
                                           flipped_outcomes = NULL) {
  
  req <- c("model", "outcome", "RATE Estimate", "2.5%", "97.5%")
  stopifnot(all(req %in% names(autoc_df)),
            all(req %in% names(qini_df)))
  
  # align on a single policy
  if ("policy" %in% names(autoc_df)) {
    pol <- unique(autoc_df$policy)
    if (length(pol) != 1 || !all(pol %in% unique(qini_df$policy))) {
      stop("AUTOC and QINI tables refer to different policies.")
    }
    autoc_df <- dplyr::filter(autoc_df, policy == pol)
    qini_df <- dplyr::filter(qini_df, policy == pol)
    pol_txt <- ifelse(pol == "treat_best",
                     "treat best responders",
                     "withhold best responders")
  } else {
    pol_txt <- "treat best responders"
  }
  
  # significant positives only
  pos_auto <- autoc_df$model[autoc_df$`2.5%` > 0]
  pos_qini <- qini_df$model[qini_df$`2.5%` > 0]
  
  both_pos <- intersect(pos_auto, pos_qini)
  only_auto <- setdiff(pos_auto, pos_qini)
  only_qini <- setdiff(pos_qini, pos_auto)
  
  lbl <- setNames(autoc_df$outcome, autoc_df$model)
  
  # build narrative
  txt <- c(
    paste0("### AUTOC vs QINI (policy = ", pol_txt, ")"),
    "* AUTOC  logarithmic weighting (top responders)",
    "* QINI   linear weighting (aggregate gain)"
  )
  
  if (length(both_pos)) {
    txt <- c(txt,
             paste0("Both methods agree on **positive RATE** for: ",
                    paste(lbl[both_pos], collapse = ", "), "."))
  }
  
  if (length(only_auto) || length(only_qini)) {
    disc <- c()
    if (length(only_auto)) {
      disc <- c(disc,
                paste0("AUTOC-only positive: ",
                       paste(lbl[only_auto], collapse = ", ")))
    }
    if (length(only_qini)) {
      disc <- c(disc,
                paste0("QINI-only positive: ",
                       paste(lbl[only_qini], collapse = ", ")))
    }
    txt <- c(txt,
             "Weighting choice matters:",
             paste(disc, collapse = "; "), ".")
  }
  
  if (!length(c(both_pos, only_auto, only_qini))) {
    txt <- c(txt, "No statistically reliable positives under either weighting.")
  }
  
  list(
    comparison = paste(txt, collapse = "\n\n"),
    autoc_results = margot_interpret_rate(autoc_df, flipped_outcomes, "AUTOC"),
    qini_results = margot_interpret_rate(qini_df, flipped_outcomes, "QINI"),
    autoc_model_names = pos_auto,
    qini_model_names = pos_qini,
    both_model_names = both_pos
  )
}