#' Omnibus Heterogeneity Test for GRF Models
#'
#' This function performs an omnibus heterogeneity test for specified models
#' outputted from margot::margot_run_models_grf() and provides interpretations for each result.
#'
#' @param model_results A list of model results from margot::margot_run_models_grf().
#' @param outcome_vars A character vector of outcome variable names.
#' @param alpha Significance level for hypothesis tests. Default is 0.05.
#' @param detail_level Character string specifying the level of detail in the output.
#'        Options are "basic", "standard" (default), or "detailed".
#' @param plot Logical indicating whether to create a plot of the results. Default is FALSE.
#'
#' @return A list containing a simple summary table, interpretations, and optionally a plot.
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map_dfr map_chr safely
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot geom_point geom_errorbar theme_minimal labs
#' @importFrom stringr str_extract
#'
#' @export
margot_omnibus_hetero_test <- function(model_results, outcome_vars, alpha = 0.05,
                                       detail_level = "standard", plot = FALSE) {
  # input validation
  if (!is.list(model_results) || length(model_results) == 0) {
    stop("model_results must be a non-empty list")
  }
  if (!is.character(outcome_vars) || length(outcome_vars) == 0) {
    stop("outcome_vars must be a non-empty character vector")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a number between 0 and 1")
  }
  if (!detail_level %in% c("basic", "standard", "detailed")) {
    stop("detail_level must be 'basic', 'standard', or 'detailed'")
  }

  # helper function to format p-values
  format_pval <- function(p) {
    if (p < 0.001) return(sprintf("%.2e", p))
    return(sprintf("%.3f", p))
  }

  # safe version of the extraction function
  safe_extract <- safely(function(outcome) {
    model_name <- paste0("model_", outcome)
    if (!model_name %in% names(model_results)) {
      stop(glue::glue("Model for outcome '{outcome}' not found in model_results"))
    }

    calib <- model_results[[model_name]]$test_calibration
    data.frame(
      outcome = outcome,
      mean_prediction_estimate = glue::glue("{round(calib['mean.forest.prediction', 'Estimate'], 2)} [{round(calib['mean.forest.prediction', 'Estimate'] - 1.96 * calib['mean.forest.prediction', 'Std. Error'], 2)}, {round(calib['mean.forest.prediction', 'Estimate'] + 1.96 * calib['mean.forest.prediction', 'Std. Error'], 2)}]"),
      differential_prediction_estimate = glue::glue("{round(calib['differential.forest.prediction', 'Estimate'], 2)} [{round(calib['differential.forest.prediction', 'Estimate'] - 1.96 * calib['differential.forest.prediction', 'Std. Error'], 2)}, {round(calib['differential.forest.prediction', 'Estimate'] + 1.96 * calib['differential.forest.prediction', 'Std. Error'], 2)}]"),
      differential_forest_prediction_t = calib["differential.forest.prediction", "t value"],
      differential_forest_prediction_p = calib["differential.forest.prediction", "Pr(>t)"],
      diff_estimate = calib["differential.forest.prediction", "Estimate"],
      diff_lower = calib["differential.forest.prediction", "Estimate"] - 1.96 * calib["differential.forest.prediction", "Std. Error"],
      diff_upper = calib["differential.forest.prediction", "Estimate"] + 1.96 * calib["differential.forest.prediction", "Std. Error"]
    )
  })

  # extract test calibration results
  calibration_results <- purrr::map_dfr(outcome_vars, ~safe_extract(.x)$result)

  # create interpretations based on detail level
  create_interpretation <- function(result) {
    base_interp <- glue::glue(
      "For {result$outcome}: mean prediction estimate: {result$mean_prediction_estimate}, ",
      "differential prediction estimate: {result$differential_prediction_estimate}. ",
      "t={round(result$differential_forest_prediction_t, 2)}, ",
      "p={format_pval(result$differential_forest_prediction_p)}. ",
      "{if(result$differential_forest_prediction_p < alpha) 'We reject the null of no heterogeneity.' else 'We do not rule out the null of no heterogeneity.'}"
    )

    if (detail_level == "basic") {
      glue::glue(
        "For {result$outcome}: ",
        "{if(result$differential_forest_prediction_p < alpha) 'We reject the null of no heterogeneity' else 'We do not rule out the null of no heterogeneity'} ",
        "(p={format_pval(result$differential_forest_prediction_p)})."
      )
    } else {
      base_interp
    }
  }

  general_statement <- "Test calibration of the forest computes the best linear fit using (1) forest predictions on held-out data and (2) the mean forest prediction as regressors, with one-sided heteroskedasticity-robust (HC3) SEs. A coefficient of 1 for the mean forest prediction suggests that the mean prediction is correct, while a coefficient of 1 for the differential forest prediction additionally suggests that the heterogeneity estimates are well calibrated. The p-value of the differential forest prediction coefficient acts as an omnibus test for the presence of heterogeneity."

  interpretations <- purrr::map_chr(1:nrow(calibration_results), ~create_interpretation(calibration_results[.x, ]))
  interpretations <- c(general_statement, interpretations)

  # create plot if requested
  if (plot) {
    p <- ggplot2::ggplot(calibration_results, ggplot2::aes(x = outcome, y = diff_estimate)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = diff_lower, ymax = diff_upper), width = 0.2) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = "Differential Prediction Estimates with 95% CI",
                    y = "Estimate",
                    x = "Outcome") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    p <- NULL
  }

  return(list(
    summary_table = calibration_results[, c("outcome", "mean_prediction_estimate", "differential_prediction_estimate")],
    interpretations = interpretations,
    plot = p
  ))
}
