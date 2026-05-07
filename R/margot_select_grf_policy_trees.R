#' Select GRF policy trees for manuscript graphing
#'
#' Applies a transparent reporting rule to the policy-brief table returned by
#' `margot_policy_workflow()`. The default rule graphs a policy tree only when
#' the selected policy value and the uplift among treated units both have lower
#' confidence limits above the specified thresholds.
#'
#' @param policy_brief Data frame with `Outcome`, `Policy Value (95% CI)`, and
#'   `Uplift in Treated (95% CI)` columns, typically
#'   `policy_workflow$policy_brief_df`.
#' @param model_names Optional character vector of model names. If supplied with
#'   `outcome_labels`, the returned table includes model names aligned by
#'   outcome label.
#' @param outcome_labels Optional character vector of outcome labels aligned with
#'   `model_names`.
#' @param policy_value_lower_threshold Numeric threshold for the policy-value
#'   lower confidence limit. Default is 0.
#' @param treated_uplift_lower_threshold Numeric threshold for the treated-uplift
#'   lower confidence limit. Default is 0.
#' @param require_policy_value Logical. Require policy-value lower confidence
#'   limit to exceed `policy_value_lower_threshold`. Default TRUE.
#' @param require_treated_uplift Logical. Require treated-uplift lower confidence
#'   limit to exceed `treated_uplift_lower_threshold`. Default TRUE.
#'
#' @return A data frame containing parsed estimates, lower confidence limits,
#'   and `graph_policy_tree`.
#' @export
margot_select_grf_policy_trees <- function(policy_brief,
                                           model_names = NULL,
                                           outcome_labels = NULL,
                                           policy_value_lower_threshold = 0,
                                           treated_uplift_lower_threshold = 0,
                                           require_policy_value = TRUE,
                                           require_treated_uplift = TRUE) {
  if (!is.data.frame(policy_brief)) {
    stop("policy_brief must be a data frame.", call. = FALSE)
  }

  required_cols <- c("Outcome", "Policy Value (95% CI)", "Uplift in Treated (95% CI)")
  missing_cols <- setdiff(required_cols, names(policy_brief))
  if (length(missing_cols) > 0) {
    stop("policy_brief is missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.null(model_names)) {
    model_names <- as.character(model_names)
    if (is.null(outcome_labels)) {
      stop("outcome_labels must be supplied when model_names is supplied.", call. = FALSE)
    }
    outcome_labels <- as.character(outcome_labels)
    if (length(model_names) != length(outcome_labels)) {
      stop("model_names and outcome_labels must have the same length.", call. = FALSE)
    }
  }

  policy_stats <- parse_grf_policy_ci(policy_brief[["Policy Value (95% CI)"]])
  uplift_stats <- parse_grf_policy_ci(policy_brief[["Uplift in Treated (95% CI)"]])

  selected <- rep(TRUE, nrow(policy_brief))
  if (isTRUE(require_policy_value)) {
    selected <- selected & !is.na(policy_stats$lower) & policy_stats$lower > policy_value_lower_threshold
  }
  if (isTRUE(require_treated_uplift)) {
    selected <- selected & !is.na(uplift_stats$lower) & uplift_stats$lower > treated_uplift_lower_threshold
  }

  out <- data.frame(
    Outcome = as.character(policy_brief[["Outcome"]]),
    stringsAsFactors = FALSE
  )

  if ("Depth" %in% names(policy_brief)) {
    out$Depth <- policy_brief[["Depth"]]
  }

  out$policy_value <- policy_stats$estimate
  out$policy_value_lower <- policy_stats$lower
  out$policy_value_upper <- policy_stats$upper
  out$treated_uplift <- uplift_stats$estimate
  out$treated_uplift_lower <- uplift_stats$lower
  out$treated_uplift_upper <- uplift_stats$upper
  out$policy_value_lower_threshold <- policy_value_lower_threshold
  out$treated_uplift_lower_threshold <- treated_uplift_lower_threshold
  out$graph_policy_tree <- selected

  if (!is.null(model_names)) {
    map <- data.frame(
      Outcome = outcome_labels,
      model_name = model_names,
      stringsAsFactors = FALSE
    )
    out <- merge(map, out, by = "Outcome", all.y = TRUE, sort = FALSE)
    out <- out[match(as.character(policy_brief[["Outcome"]]), out$Outcome), , drop = FALSE]
    rownames(out) <- NULL
  }

  out
}

#' Parse estimate and confidence interval strings
#' @noRd
parse_grf_policy_ci <- function(x) {
  x <- as.character(x)
  estimate <- suppressWarnings(as.numeric(sub("^\\s*([^\\s\\[]+).*$", "\\1", x)))
  lower <- suppressWarnings(as.numeric(sub("^.*\\[\\s*([^,]+),.*$", "\\1", x)))
  upper <- suppressWarnings(as.numeric(sub("^.*?,\\s*([^\\]]+)\\].*$", "\\1", x)))

  no_ci <- !grepl("\\[", x)
  lower[no_ci] <- NA_real_
  upper[no_ci] <- NA_real_

  data.frame(
    estimate = estimate,
    lower = lower,
    upper = upper
  )
}
