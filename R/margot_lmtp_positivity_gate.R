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
#' Investigators should state their expectations in the study protocol, report
#' the realised density-ratio quantities with Margot's descriptive functions,
#' and record any retain, revise, withdraw, or stop decision as a human judgement
#' rather than deriving it from a constant.
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
      "State expectations in the study protocol, read the realised quantities",
      "from Margot's descriptive density-ratio reports, and record the decision",
      "as investigators' judgement."
    )
  )
}
