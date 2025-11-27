#' Policy Value Explainer (shared text)
#'
#' Returns a concise, reusable explanation of the policy value quantities
#' used across Margot's policy tree reporting functions. This helper is
#' intended to be a single source of truth for how we describe these
#' estimands to end users and policymakers.
#'
#' @param audience Character; one of "policy" or "research". The wording is
#'   identical for now, but the parameter allows future tailoring.
#' @param include_acronyms Logical; if TRUE, append a list of common acronyms
#'   used in NZAVS research (RWA, SDO, PWI, NZSEI). Default FALSE.
#' @return A single character string with the explanation paragraph(s).
#' @keywords internal
margot_policy_value_explainer <- function(audience = c("policy", "research"),
                                          include_acronyms = FALSE) {
  audience <- match.arg(audience)
  # allow boilerplate override
  bp <- margot_get_boilerplate("policy_value_explainer", audience = audience, context = list(audience = audience))
  if (!is.null(bp) && nzchar(bp)) return(bp)

  base_text <- paste0(
    "The following terms are used throughout this report. ",
    "*Policy value* (PV) is the average predicted benefit from following the learned treatment rule compared to treating no one (control-all baseline). ",
    "*Coverage* is the proportion of participants recommended for treatment. ",
    "*Uplift* is the mean predicted treatment effect among those recommended for treatment. ",
    "By construction, PV = Coverage $\\times$ Uplift. ",
    "All estimates are reported with 95% confidence intervals. ",
    "The *dominant split* is the first-level branch contributing most to PV. ",
    "A *restricted policy* treats only within the dominant split and withholds elsewhere."
  )

  if (isTRUE(include_acronyms)) {
    acronyms_text <- paste0(
      "\n\nCommon acronyms:\n",
      "* RWA — Right-Wing Authoritarianism\n",
      "* SDO — Social Dominance Orientation\n",
      "* PWI — Personal Well-Being Index\n",
      "* NZSEI — Occupational Prestige Index"
    )
    base_text <- paste0(base_text, acronyms_text)
  }

  base_text
}
