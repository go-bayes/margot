#' Generate Missing Data Handling Description (DEPRECATED, use `boilerplate` package: <https://go-bayes.github.io/boilerplate/>)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated. Please use `boilerplate::boilerplate_report_missing_data()` instead.
#' Install the new package with `devtools::install_github("go-bayes/boilerplate")`.
#'
#' @param ... All arguments (ignored)
#'
#' @return A message indicating the function is deprecated.
#'
#' @examples
#' \dontrun{
#' # This function is deprecated. Use instead:
#' # devtools::install_github("go-bayes/boilerplate")
#' # library(boilerplate)
#' # boilerplate::boilerplate_report_missing_data(...)
#' }
#'
#' @import lifecycle
#' @import cli
#'
#' @keywords internal
boilerplate_methods_missing_data <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.2.1.23",
    what = "boilerplate_methods_missing_data()",
    with = "boilerplate::boilerplate_report_missing_data()"
  )

  cli::cli_alert_warning("This function is deprecated. Please use boilerplate::boilerplate_report_missing_data() instead.")
  cli::cli_alert_info("Install the new package with: devtools::install_github(\"go-bayes/boilerplate\")")
  cli::cli_alert_info("After installation, load the package with: library(boilerplate)")
}
#' #' Generate Missing Data Handling Description
#' #'
#' #' @param estimators Character vector specifying the statistical estimators used.
#' #' @param grf_appendix Character string specifying the appendix for GRF missing data handling.
#' #' @param baseline_wave Character string specifying the baseline wave of the study.
#' #' @param exposure_wave Character string specifying the exposure wave of the study.
#' #' @param outcome_wave Character string specifying the outcome wave of the study.
#' #' @param baseline_missing_data_proportion Numeric value indicating the percentage of missing data at baseline.
#' #'
#' #' @return A character string containing the missing data handling description in markdown format.
#' #'
#' #' @examples
#' #' missing_data_text <- boilerplate_methods_missing_data(
#' #'   estimators = c("lmtp", "grf"),
#' #'   grf_appendix = "D",
#' #'   baseline_wave = "NZAVS time 10, years 2018-2019",
#' #'   exposure_wave = "NZAVS time 11, years 2019-2020",
#' #'   outcome_wave = "NZAVS time 12, years 2020-2021",
#' #'   baseline_missing_data_proportion = 15
#' #' )
#' #'
#' #' @export
#' boilerplate_methods_missing_data <- function(estimators = c("lmtp"), grf_appendix = "C", baseline_wave, exposure_wave, outcome_wave, baseline_missing_data_proportion) {
#'   markdown_text <- "### Missing Data\n\n"
#'   lmtp_sdr_text <- "To mitigate bias from missing data, we implement the following strategies:
#' **Baseline missingness**: we employed the `ppm` algorithm from the `mice` package in R [@vanbuuren2018] to impute missing baseline data (wave {baseline_wave}). This method allowed us to reconstruct incomplete datasets by estimating a plausible value for missing observation. Because we could only pass one data set to the {estimator}, we employed single imputation. Approximately {baseline_missing_data_proportion}% of covariate values were missing at {baseline_wave}. We only used baseline data to impute baseline wave missingness (refer to @zhang2023shouldMultipleImputation).
#' **Outcome missingness**: to address confounding and selection bias arising from missing responses and panel attrition at the end of study {outcome_wave}, we applied censoring weights obtained using nonparametric machine learning ensembles afforded by the `{estimator}` package (and its dependencies) in R [@williams2021]."
#'   grf_text <- "The GRF package accepts missing values at baseline. To obtain valid inference for missing responses we computed inverse probability of censoring weights. See Appendix {grf_appendix}."
#'   if ("lmtp" %in% estimators || "sdr" %in% estimators) {
#'     for (est in c("lmtp", "sdr")) {
#'       if (est %in% estimators) {
#'         markdown_text <- paste0(markdown_text, glue::glue(lmtp_sdr_text,
#'                                                           estimator = est,
#'                                                           baseline_wave = baseline_wave,
#'                                                           baseline_missing_data_proportion = baseline_missing_data_proportion,
#'                                                           outcome_wave = outcome_wave), "\n\n")
#'       }
#'     }
#'   }
#'   if ("grf" %in% estimators) {
#'     markdown_text <- paste0(markdown_text, glue::glue(grf_text, grf_appendix = grf_appendix), "\n\n")
#'   }
#'   return(trimws(markdown_text))
#' }
