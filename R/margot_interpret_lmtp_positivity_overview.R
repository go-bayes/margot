#' Overview bullets for multiple LMTP positivity analyses
#'
#' Creates concise, supplement-ready overview bullets and an optional table
#' summarising worst-case positivity diagnostics across multiple analyses
#' (e.g., different outcome/shift sets).
#'
#' @param reports A named list where each element is the result of
#'   `margot_report_lmtp_positivity()` for one analysis. The list names or the
#'   `labels` vector will be used as the analysis labels.
#' @param labels Optional character vector of labels (same length/order as
#'   `reports`). If NULL, uses `names(reports)`.
#' @param digits Integer for rounding ESS/N in bullets and table (default 2).
#' @param include_table Logical; if TRUE, returns a data.frame `overview_table`.
#'
#' @return A list with
#'   - bullets: character vector of bullet lines
#'   - overview_table: data.frame (if include_table = TRUE) with worst-zero and
#'     worst ESS/N per analysis
#' @export
margot_interpret_lmtp_positivity_overview <- function(reports,
                                                      labels = names(reports),
                                                      digits = 2,
                                                      include_table = TRUE) {
  if (!length(reports)) return(list(bullets = character(0)))
  if (is.null(labels) || length(labels) != length(reports)) labels <- names(reports)
  if (is.null(labels)) labels <- paste0("Analysis ", seq_along(reports))

  fmt_pct <- function(x) {
    if (is.na(x)) return("NA")
    paste0(sprintf("%0.2f", 100 * x), "%")
  }
  fmt_frac <- function(x) {
    if (is.na(x)) return("NA")
    sprintf(paste0("%0.", max(0, digits), "f"), x)
  }

  rows <- vector("list", length(reports))
  for (i in seq_along(reports)) {
    rep <- reports[[i]]
    lab <- labels[[i]]
    ov <- rep$overall
    if (is.null(ov) || !nrow(ov)) {
      rows[[i]] <- data.frame(
        label = lab,
        worst_zero = NA_real_,
        worst_zero_estimand = NA_character_,
        worst_ess = NA_real_,
        worst_ess_estimand = NA_character_
      )
      next
    }
    # Identify worst cases from overall table
    zero_idx <- suppressWarnings(which.max(ov$Prop_zero))
    ess_idx  <- suppressWarnings(which.min(ov$`ESS/N`))
    rows[[i]] <- data.frame(
      label = lab,
      worst_zero = ov$Prop_zero[zero_idx],
      worst_zero_estimand = ov$Estimand[zero_idx],
      worst_ess = ov$`ESS/N`[ess_idx],
      worst_ess_estimand = ov$Estimand[ess_idx]
    )
  }

  summary_by_model <- do.call(rbind, rows)

  bullets <- apply(summary_by_model, 1, function(r) {
    paste0(
      "- ", r[["label"]], ": zeros peaked at ", fmt_pct(as.numeric(r[["worst_zero"]])),
      " (", r[["worst_zero_estimand"]], "); ESS/N stayed above ", fmt_frac(as.numeric(r[["worst_ess"]])),
      " (", r[["worst_ess_estimand"]], ")."
    )
  })

  out <- list(bullets = unname(bullets))
  if (isTRUE(include_table)) {
    overview_table <- data.frame(
      Analysis = summary_by_model$label,
      `Worst-zero estimand` = summary_by_model$worst_zero_estimand,
      `Max prop_zero` = round(summary_by_model$worst_zero, digits),
      `Worst ESS/N estimand` = summary_by_model$worst_ess_estimand,
      `Min ESS/N` = round(summary_by_model$worst_ess, digits),
      check.names = FALSE
    )
    out$overview_table <- overview_table
  }

  out
}

