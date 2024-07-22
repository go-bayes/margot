#' Omnibus Heterogeneity Test for GRF Models
#'
#' This function performs an omnibus heterogeneity test for specified models
#' outputted from margot::margot_run_models_grf() and provides interpretations for each result.
#'
#' @param model_results A list of model results from margot::margot_run_models_grf().
#' @param outcome_vars A character vector of outcome variable names.
#'
#' @return A list containing two elements: a data frame with summary statistics and a character vector with interpretations.
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map_dfr map_chr
#' @importFrom glue glue
#'
#' @export
margot_omnibus_hetero_test <- function(model_results, outcome_vars) {
  # Extract test calibration results
  calibration_results <- purrr::map_dfr(outcome_vars, function(outcome) {
    model_name <- paste0("model_", outcome)
    if (!model_name %in% names(model_results)) {
      return(NULL)  # Skip if model not found
    }

    calib <- model_results[[model_name]]$test_calibration
    data.frame(
      outcome = outcome,
      mean_prediction_estimate = glue::glue("{round(calib['mean.forest.prediction', 'Estimate'], 3)} [{round(calib['mean.forest.prediction', 'Estimate'] - 1.96 * calib['mean.forest.prediction', 'Std. Error'], 3)}, {round(calib['mean.forest.prediction', 'Estimate'] + 1.96 * calib['mean.forest.prediction', 'Std. Error'], 3)}]"),
      differential_prediction_estimate = glue::glue("{round(calib['differential.forest.prediction', 'Estimate'], 3)} [{round(calib['differential.forest.prediction', 'Estimate'] - 1.96 * calib['differential.forest.prediction', 'Std. Error'], 3)}, {round(calib['differential.forest.prediction', 'Estimate'] + 1.96 * calib['differential.forest.prediction', 'Std. Error'], 3)}]"),
      differential_forest_prediction_p = calib["differential.forest.prediction", "Pr(>t)"]
    )
  })

  # Create interpretations
  general_statement <- "Test calibration of the forest computes the best linear fit using (1) forest predictions on held-out data and (2) the mean forest prediction as regressors, with one-sided heteroskedasticity-robust (HC3) SEs. A coefficient of 1 for the mean forest prediction suggests that the mean prediction is correct, while a coefficient of 1 for the differential forest prediction additionally suggests that the heterogeneity estimates are well calibrated. The p-value of the differential forest prediction coefficient acts as an omnibus test for the presence of heterogeneity."

  interpretations <- purrr::map_chr(1:nrow(calibration_results), function(i) {
    result <- calibration_results[i, ]
    glue::glue(
      "For {result$outcome}: mean prediction estimate: {result$mean_prediction_estimate}, ",
      "differential prediction estimate: {result$differential_prediction_estimate}. ",
      "{if(result$differential_forest_prediction_p < 0.05) 'We reject the null of no heterogeneity.' else 'We do not rule out the null of no heterogeneity.'}"
    )
  })

  interpretations <- c(general_statement, interpretations)

  return(list(
    summary_table = calibration_results[, c("outcome", "mean_prediction_estimate", "differential_prediction_estimate")],
    interpretations = interpretations
  ))
}
