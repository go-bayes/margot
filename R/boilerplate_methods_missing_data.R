#' Generate Missing Data Handling Description
#'
#' @param estimators Character vector specifying the statistical estimators used.
#' @param grf_appendix Character string specifying the appendix for GRF missing data handling.
#'
#' @return A character string containing the missing data handling description in markdown format.
#'
#' @keywords internal
boilerplate_methods_missing_data <- function(estimators = c("lmtp"), grf_appendix = "C", baseline_wave, exposure_wave, outcome_wave) {
  markdown_text <- "### Missing Data\n\n"
  lmtp_sdr_text <- "To mitigate bias from missing data, we implement the following strategies:
**Baseline missingness**: we employed the `ppm` algorithm from the `mice` package in R [@vanbuuren2018] to impute missing baseline data (wave {baseline_wave}) . This method allowed us to reconstruct incomplete datasets by estimating a plausible value for missing observation. Because we could only pass one data set to the {estimator}, we employed single imputation. Approximately 1% of covariate values were missing at baseline. Eligibility for the study required fully observed baseline treatment measures as well as treatment wave treatment measures. Again, we only used baseline data to impute baseline missingness (refer to @zhang2023shouldMultipleImputation).
**Outcome missingness**: to address confounding and selection bias arising from missing responses and panel attrition, we applied censoring weights obtained using nonparametric machine learning ensembles afforded by the `{estimator}` package (and its dependencies) in R [@williams2021]."
  grf_text <- "The GRF package accepts missing values at baseline. To obtain valid inference for missing responses we computed inverse probability of censoring weights. See Appendix {grf_appendix}."
  if ("lmtp" %in% estimators || "sdr" %in% estimators) {
    for (est in c("lmtp", "sdr")) {
      if (est %in% estimators) {
        markdown_text <- paste0(markdown_text, glue::glue(lmtp_sdr_text, estimator = est, baseline_wave = baseline_wave), "\n\n")
      }
    }
  }
  if ("grf" %in% estimators) {
    markdown_text <- paste0(markdown_text, glue::glue(grf_text, grf_appendix = grf_appendix), "\n\n")
  }
  return(trimws(markdown_text))
}
