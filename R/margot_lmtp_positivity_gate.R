#' Registered trim-ladder positivity gate for LMTP fits (defunct)
#'
#' `margot_lmtp_positivity_gate()` implemented the retired trim ladder, the
#' effective-sample-size floor, and the product support band as a mechanical
#' pass/fail per policy and rung. The guide-architecture change of 29 July 2026
#' retired that machinery: no `margot` function computes a traffic light, a
#' tolerance, a retention profile, or an override, and no return value carries a
#' verdict. The function is therefore defunct, and calling it errors with a
#' condition of class `margot_error_defunct`. No registered study used it.
#'
#' The replacement is the question-review workflow in the `margot.lmtp` package.
#' There an author precommits expectations against a controlled vocabulary of
#' report quantities, the density-ratio report prints those expectations beside
#' their realised values, and the retain, revise, withdraw, or stop decision is
#' recorded as a human judgement rather than derived from a constant.
#'
#' @param fit,outcome,shifts,rungs,ess_floor,trim_mass_budget,test_thresholds,label_mapping,verbose
#'   Retained for signature compatibility; the function errors before reading
#'   any of them.
#'
#' @return Nothing. The function always errors.
#'
#' @seealso [margot_lmtp_positivity()] for the descriptive density-ratio
#'   summaries that remain.
#' @keywords internal
#' @export
margot_lmtp_positivity_gate <- function(fit,
                                        outcome = NULL,
                                        shifts = NULL,
                                        rungs = c(0.99, 0.98, 0.96),
                                        ess_floor = 0.5,
                                        trim_mass_budget = 0.05,
                                        test_thresholds = NULL,
                                        label_mapping = NULL,
                                        verbose = TRUE) {
  margot_positivity_defunct(
    what = "margot_lmtp_positivity_gate",
    replacement = paste(
      "Precommit expectations with `margot.lmtp::margot_lmtp_expectations_spec()`,",
      "read the realised quantities from `margot.lmtp::margot_lmtp_ratio_report()`,",
      "and record the decision with `margot.lmtp::margot_lmtp_question_decision()`."
    )
  )
}
