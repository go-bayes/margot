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
#' @export
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


