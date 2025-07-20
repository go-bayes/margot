# ────────────────────────────────────────────────────────────────────────────
# 1. Interpret a single AUTOC or QINI table
#' interpret RATE estimates (single table or pair of tables)
#'
#' @param rate_df data-frame returned by `margot_rate()` **or** the list
#'   (`rate_autoc`, `rate_qini`) that `margot_rate()` returns.
#' @param flipped_outcomes character vector of outcome names that were
#'   direction-flipped during preprocessing.
#' @param target character; "AUTOC" or "QINI". ignored when `rate_df`
#'   is the list of two tables.
#'
#' @return markdown character string (single table) or the comparison list.
#' @keywords internal
margot_interpret_rate <- function(rate_df,
                                  flipped_outcomes = NULL,
                                  target           = "AUTOC") {

  ## ── send two-table input to the comparison helper ──────────────────────
  if (is.list(rate_df) && !is.data.frame(rate_df) &&
      all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
    return(
      margot_interpret_rate_comparison(
        autoc_df         = rate_df$rate_autoc,
        qini_df          = rate_df$rate_qini,
        flipped_outcomes = flipped_outcomes
      )
    )
  }

  ## ── sanity checks ──────────────────────────────────────────────────────
  stopifnot(target %in% c("AUTOC", "QINI"))
  req <- c("RATE Estimate", "2.5%", "97.5%", "outcome")
  if (!all(req %in% names(rate_df)))
    stop("rate_df must contain columns: ", paste(req, collapse = ", "))

  ## ── isolate a single policy, if present ────────────────────────────────
  if ("policy" %in% names(rate_df)) {
    pol <- unique(rate_df$policy)
    if (length(pol) != 1)
      stop("rate_df mixes multiple policies; filter before calling.")
    rate_df <- dplyr::filter(rate_df, policy == pol)
    pol_txt <- ifelse(pol == "treat_best",
                      "treat best responders",
                      "withhold best responders")
  } else {
    pol_txt <- "treat best responders"   # legacy default
  }

  ## ── mark inverted outcomes (optional) ──────────────────────────────────
  if (!is.null(flipped_outcomes) && length(flipped_outcomes)) {
    w <- rate_df$outcome %in% flipped_outcomes
    rate_df$outcome[w] <- paste0(rate_df$outcome[w], " (inverted)")
  }

  ## ── classify outcomes ──────────────────────────────────────────────────
  lo  <- rate_df$`2.5%`
  hi  <- rate_df$`97.5%`
  pos <- which(lo  > 0)
  neg <- which(hi < 0)
  inc <- setdiff(seq_along(lo), c(pos, neg))

  ## ── assemble markdown text ─────────────────────────────────────────────
  lines <- list(
    paste0("### RATE (", target, ", policy = ", pol_txt, ")"),
    if (target == "AUTOC")
      "AUTOC applies logarithmic weighting, sharpening focus on top responders."
    else
      "QINI applies linear weighting, maximising aggregate gain."
  )

  if (length(pos))
    lines <- c(lines,
               paste0("**Positive RATE** – CATE targeting outperforms ATE for: ",
                      paste(rate_df$outcome[pos], collapse = ", "), "."))
  if (length(neg))
    lines <- c(lines,
               paste0("**Negative RATE** – CATE targeting would *underperform* ATE for: ",
                      paste(rate_df$outcome[neg], collapse = ", "), "."))
  if (length(inc))
    lines <- c(lines,
               paste0("Evidence inconclusive for: ",
                      paste(rate_df$outcome[inc], collapse = ", "),
                      " (95 % CI crosses 0)."))

  paste(lines, collapse = "\n\n")
}

#' Compare AUTOC and QINI RATE tables for the *same* policy
#'
#' @param autoc_df Data frame from `margot_rate()` (AUTOC).
#' @param qini_df  Data frame from `margot_rate()` (QINI).
#' @param flipped_outcomes Character (unused; kept for API symmetry).
#'
#' @return List with `comparison`, `autoc_results`, `qini_results`,
#'   and the four sets of model names.
#' @keywords internal
margot_interpret_rate_comparison <- function(autoc_df,
                                             qini_df,
                                             flipped_outcomes = NULL) {

  req <- c("model", "outcome", "RATE Estimate", "2.5%", "97.5%")
  stopifnot(all(req %in% names(autoc_df)),
            all(req %in% names(qini_df)))

  ## ── align on a single policy ───────────────────────────────────────────
  if ("policy" %in% names(autoc_df)) {
    pol <- unique(autoc_df$policy)
    if (length(pol) != 1 ||
        !all(pol %in% unique(qini_df$policy)))
      stop("AUTOC and QINI tables refer to different policies.")
    autoc_df <- dplyr::filter(autoc_df, policy == pol)
    qini_df  <- dplyr::filter(qini_df,  policy == pol)
    pol_txt  <- ifelse(pol == "treat_best",
                       "treat best responders",
                       "withhold best responders")
  } else {
    pol_txt  <- "treat best responders"
  }

  ## ── significant positives only (your original convention) ──────────────
  pos_auto <- autoc_df$model[autoc_df$`2.5%` > 0]
  pos_qini <- qini_df$model[qini_df$`2.5%`  > 0]

  both_pos  <- intersect(pos_auto, pos_qini)
  only_auto <- setdiff(pos_auto, pos_qini)
  only_qini <- setdiff(pos_qini, pos_auto)

  lbl <- setNames(autoc_df$outcome, autoc_df$model)

  ## ── build narrative ────────────────────────────────────────────────────
  txt <- c(
    paste0("### AUTOC vs QINI (policy = ", pol_txt, ")"),
    "* AUTOC — logarithmic weighting (top responders)",
    "* QINI  — linear weighting (aggregate gain)"
  )

  if (length(both_pos)) {
    txt <- c(txt,
             paste0("Both methods agree on **positive RATE** for: ",
                    paste(lbl[both_pos], collapse = ", "), "."))
  }
  if (length(only_auto) || length(only_qini)) {
    disc <- c()
    if (length(only_auto))
      disc <- c(disc,
                paste0("AUTOC-only positive: ",
                       paste(lbl[only_auto], collapse = ", ")))
    if (length(only_qini))
      disc <- c(disc,
                paste0("QINI-only positive: ",
                       paste(lbl[only_qini], collapse = ", ")))
    txt <- c(txt,
             "Weighting choice matters:",
             paste(disc, collapse = "; "), ".")
  }
  if (!length(c(both_pos, only_auto, only_qini))) {
    txt <- c(txt, "No statistically reliable positives under either weighting.")
  }

  list(
    comparison          = paste(txt, collapse = "\n\n"),
    autoc_results       = margot_interpret_rate(autoc_df, flipped_outcomes, "AUTOC"),
    qini_results        = margot_interpret_rate(qini_df,  flipped_outcomes, "QINI"),
    autoc_model_names   = pos_auto,
    qini_model_names    = pos_qini,
    both_model_names    = both_pos
  )
}

# ────────────────────────────────────────────────────────────────────────────
# old
# margot_interpret_rate <- function(rate_df,
#                                   flipped_outcomes = NULL,
#                                   target = "AUTOC") {
#
#   ## ── dispatch to comparison helper ───────────────────────────────────────
#   if (is.list(rate_df) && !is.data.frame(rate_df) &&
#       all(c("rate_autoc", "rate_qini") %in% names(rate_df))) {
#     return(
#       margot_interpret_rate_comparison(
#         autoc_df        = rate_df$rate_autoc,
#         qini_df         = rate_df$rate_qini,
#         flipped_outcomes = flipped_outcomes
#       )
#     )
#   }
#
#   ## ── basic checks ────────────────────────────────────────────────────────
#   stopifnot(target %in% c("AUTOC", "QINI"))
#   req <- c("RATE Estimate", "2.5%", "97.5%")
#   if (!all(req %in% names(rate_df)))
#     stop("rate_df must contain the columns: ", paste(req, collapse = ", "))
#
#   ## ── isolate one policy (if present) ─────────────────────────────────────
#   if ("policy" %in% names(rate_df)) {
#     pol <- unique(rate_df$policy)
#     if (length(pol) > 1)
#       stop("rate_df mixes different policies; filter before calling.")
#     rate_df <- dplyr::filter(rate_df, policy == pol)
#     policy_string <- ifelse(pol == "treat_best",
#                             "treat best responders",
#                             "withhold best responders")
#   } else {
#     policy_string <- "treat best responders"   # legacy default
#   }
#
#   ## ── identify signals ────────────────────────────────────────────────────
#   out   <- rate_df[[1]]
#   lo    <- rate_df$`2.5%`
#   hi    <- rate_df$`97.5%`
#   pos   <- which(lo > 0)
#   neg   <- which(hi < 0)
#   incon <- setdiff(seq_along(out), c(pos, neg))
#
#   ## ── construct narrative ────────────────────────────────────────────────
#   bullets <- list(
#     paste0("### Evidence for heterogeneous treatment effects (policy = ",
#            policy_string, ") via ", target),
#     if (target == "AUTOC")
#       "AUTOC uses logarithmic weighting, emphasising the top responders."
#     else
#       "QINI uses linear weighting, balancing effect size and prevalence."
#   )
#
#   if (length(pos)) {
#     bullets <- c(
#       bullets,
#       paste0("**Positive RATE** – CATE targeting outperforms ATE for: ",
#              paste(out[pos], collapse = ", "), ".")
#     )
#   }
#   if (length(neg)) {
#     bullets <- c(
#       bullets,
#       paste0("**Negative RATE** – CATE targeting would *underperform* ATE for: ",
#              paste(out[neg], collapse = ", "), ".")
#     )
#   }
#   if (length(incon)) {
#     bullets <- c(
#       bullets,
#       paste0(
#         "Evidence remains inconclusive for: ",
#         paste(out[incon], collapse = ", "),
#         " (95 % CI crosses zero)."
#       )
#     )
#   }
#   paste(bullets, collapse = "\n\n")
# }
