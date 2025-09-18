#' Condensed table: treated-only summary
#'
#' Produces a one-row-per outcome × depth table focused on the treated-only signal:
#' average uplift among treated with 95% CI (if available) and coverage (% treated).
#'
#' @param report_df Data frame returned by [margot_report_consensus_policy_value()],
#'   called with `include_treated_only = TRUE`.
#' @param label_mapping Optional named list for outcome label mapping.
#' @param digits Integer; rounding digits for numeric columns (default 3).
#'
#' @return A condensed data frame with columns:
#'   outcome, depth, avg_uplift_treated, (optional) avg_uplift_treated_ci,
#'   coverage_treated_%.
#'
#' @export
margot_table_treated_only <- function(report_df,
                                      label_mapping = NULL,
                                      digits = 3) {
  if (is.null(report_df) || !nrow(report_df)) return(report_df)

  keep <- unique(report_df[, c("outcome", "outcome_label", "depth",
                               "avg_uplift_treated", "avg_uplift_treated_ci_lo",
                               "avg_uplift_treated_ci_hi", "coverage_treated")])

  # Apply label mapping or use outcome_label if present
  if (!is.null(label_mapping)) {
    keep$outcome <- vapply(keep$outcome, function(x) .apply_label_stability(x, label_mapping), character(1))
  } else if ("outcome_label" %in% names(keep)) {
    keep$outcome <- keep$outcome_label
  }

  # Round and format
  keep$avg_uplift_treated <- round(keep$avg_uplift_treated, digits)
  if (all(c("avg_uplift_treated_ci_lo", "avg_uplift_treated_ci_hi") %in% names(keep))) {
    keep$avg_uplift_treated_ci <- paste0(
      "[", round(keep$avg_uplift_treated_ci_lo, digits), ", ",
      round(keep$avg_uplift_treated_ci_hi, digits), "]"
    )
  }
  keep$coverage_treated_pct <- round(100 * keep$coverage_treated, 1)

  cols <- c("outcome", "depth", "avg_uplift_treated")
  if ("avg_uplift_treated_ci" %in% names(keep)) cols <- c(cols, "avg_uplift_treated_ci")
  cols <- c(cols, "coverage_treated_pct")
  out <- keep[, cols]
  rownames(out) <- NULL
  out
}
