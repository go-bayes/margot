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
  # allow boilerplate override
  bp <- margot_get_boilerplate("policy_value_explainer", audience = audience, context = list(audience = audience))
  if (!is.null(bp) && nzchar(bp)) return(bp)
  paste0(
    "Policy Value (PV): the average predicted benefit from following the policy, ",
    "compared to a simple baseline. ",
    "PV vs control-all: average benefit when treating only those recommended by the policy ",
    "instead of treating no one; formally, the mean of the predicted treatment effect among ",
    "those the policy chooses to treat. ",
    "PV vs treat-all: average benefit when withholding treatment only where the policy recommends control ",
    "instead of treating everyone; formally, the mean predicted benefit of withholding among those the policy chooses to control. ",
    "Average uplift (treated): the mean predicted treatment effect among units the policy recommends to treat. ",
    "Coverage: the share of people the policy recommends to treat. ",
    "Confidence interval (CI): the 95% uncertainty range for each estimate. ",
    "Identity: PV(control-all) = Coverage $\\times$ Average uplift (treated). ",
    "Dominant split: the branch at the first split that contributes the largest share of PV(control-all). ",
    "Restricted policy: a simplified policy that treats only within the dominant split and withholds elsewhere.",
    "\n\nCommon acronyms:\n",
    "* RWA — Right-Wing Authoritarianism\n",
    "* SDO — Social Dominance Orientation\n",
    "* PWI — Personal Well-Being Index\n",
    "* NZSEI — Occupational Prestige Index"
  )
}
