#' Generate Confounding Control Description
#'
#' @param appendix_ref Character string specifying the appendix reference for covariates.
#' @param protocol_url Character string specifying the URL for the study protocol.
#'
#' @return A character string containing the confounding control description in markdown format.
#'
#' @keywords internal
boilerplate_methods_confounding_control <- function(appendix_ref = "B", protocol_url = "https://osf.io/ce4t9/") {
  markdown_text <- glue::glue("
### Confounding Control

To manage confounding in our analysis, we implement [@vanderweele2019]'s *modified disjunctive cause criterion* by following these steps:

1. **Identified all common causes** of both the treatment and outcomes.
2. **Excluded instrumental variables** that affect the exposure but not the outcome. Instrumental variables do not contribute to controlling confounding and can reduce the efficiency of the estimates.
3. **Included proxies for unmeasured confounders** affecting both exposure and outcome. According to the principles of d-separation @pearl2009a, using proxies allows us to control for their associated unmeasured confounders indirectly.
4. **Controlled for baseline exposure** and **baseline outcome**. Both are used as proxies for unmeasured common causes, enhancing the robustness of our causal estimates, refer to @vanderweele2020.

[Appendix {appendix_ref}](#appendix-demographics) details the covariates we included for confounding control. These methods adhere to the guidelines provided in [@bulbulia2024PRACTICAL] and were pre-specified in our study protocol [{protocol_url}]({protocol_url}).
  ")

  return(markdown_text)
}
