#' Policy Value Explainer (shared text)
#'
#' Returns a concise, reusable explanation of the policy value quantities
#' used across Margot's policy tree reporting functions. This helper is
#' intended to be a single source of truth for how we describe these
#' estimands to end users and policymakers.
#'
#' @param audience Character; one of "policy" or "research". The wording is
#'   identical for now, but the parameter allows future tailoring.
#' @return A single character string with the explanation paragraph(s).
#' @keywords internal
margot_policy_value_explainer <- function(audience = c("policy", "research")) {
  audience <- match.arg(audience)
  paste0(
    "Policy value vs control-all: average benefit when treating only those ",
    "recommended by the policy instead of treating no one; formally, the mean ",
    "of the predicted treatment effect among those the policy chooses to treat. ",
    "Policy value vs treat-all: average benefit when withholding treatment only ",
    "where the policy recommends control instead of treating everyone; formally, ",
    "the mean predicted benefit of withholding among those the policy chooses to control. ",
    "Avg uplift among treated: the mean predicted treatment effect among units the ",
    "policy recommends to treat. Coverage: the share the policy recommends to treat. ",
    "Identity: PV(control-all) = Coverage × Avg uplift among treated."
  )
}

