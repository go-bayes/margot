#' Omnibus Heterogeneity Test for GRF Models
#'
#' This function performs an omnibus heterogeneity test for specified models
#' outputted from margot::margot_run_models_grf() and provides interpretations for each result.
#'
#' @param model_results A list of model results from margot::margot_run_models_grf().
#' @param outcome_vars Optional. A character vector of outcome variable names. If NULL,
#'        the function will attempt to use the outcome_vars from the model_results input.
#' @param alpha Significance level for hypothesis tests. Default is 0.05.
#' @param detail_level Character string specifying the level of detail in the output.
#'        Options are "basic", "standard" (default), or "detailed".
#' @param label_mapping Optional. A named list mapping outcome variable names to display labels.
#'        For example: list("t2_agreeableness_z" = "Agreeableness").
#'
#' @return A list containing a summary table and interpretations.
#'        The summary table includes mean and differential predictions with standard errors.
#'        Interpretations provide detailed analysis of the test calibration results.
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map_dfr map_chr safely
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @importFrom cli cli_alert_info
#'
#' @export
margot_omnibus_hetero_test <- function(model_results, outcome_vars = NULL, alpha = 0.05,
                                       detail_level = "standard", label_mapping = NULL) {
  # input validation
  if (!is.list(model_results) || length(model_results) == 0) {
    stop("model_results must be a non-empty list")
  }

  # if outcome_vars is not provided, try to get it from model_results, or use all model names
  if (is.null(outcome_vars)) {
    if ("outcome_vars" %in% names(model_results)) {
      outcome_vars <- model_results$outcome_vars
    } else {
      outcome_vars <- gsub("^model_", "", names(model_results$results))
    }
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

  # create display labels for outcomes
  display_labels <- sapply(outcome_vars, function(outcome) {
    transform_label(outcome, label_mapping,
                    list(remove_tx_prefix = TRUE,
                         remove_z_suffix = TRUE,
                         remove_underscores = TRUE,
                         use_title_case = TRUE))
  })

  # create a mapping between original outcomes and display labels
  outcome_to_display <- setNames(display_labels, outcome_vars)
  display_to_outcome <- setNames(outcome_vars, display_labels)

  # helper function to format p-values
  format_pval <- function(p) {
    if (p < 0.001) {
      return(sprintf("%.2e", p))
    }
    return(sprintf("%.3f", p))
  }

  # safe version of the extraction function
  safe_extract <- safely(function(outcome) {
    model_name <- paste0("model_", outcome)
    if (!model_name %in% names(model_results$results)) {
      stop(glue::glue("Model for outcome '{outcome}' not found in model_results"))
    }

    calib <- model_results$results[[model_name]]$test_calibration
    data.frame(
      outcome = outcome,
      display_label = outcome_to_display[outcome],
      mean_prediction = glue::glue("{round(calib['mean.forest.prediction', 'Estimate'], 2)} ({round(calib['mean.forest.prediction', 'Std. Error'], 2)})"),
      mean_prediction_estimate = round(calib["mean.forest.prediction", "Estimate"], 2),
      mean_prediction_se = round(calib["mean.forest.prediction", "Std. Error"], 2),
      mean_prediction_t = round(calib["mean.forest.prediction", "Estimate"] / calib["mean.forest.prediction", "Std. Error"], 2),
      mean_prediction_p = calib["mean.forest.prediction", "Pr(>t)"],
      differential_prediction = glue::glue("{round(calib['differential.forest.prediction', 'Estimate'], 2)} ({round(calib['differential.forest.prediction', 'Std. Error'], 2)})"),
      differential_prediction_estimate = round(calib["differential.forest.prediction", "Estimate"], 2),
      differential_prediction_se = round(calib["differential.forest.prediction", "Std. Error"], 2),
      differential_prediction_t = round(calib["differential.forest.prediction", "Estimate"] / calib["differential.forest.prediction", "Std. Error"], 2),
      differential_prediction_p = calib["differential.forest.prediction", "Pr(>t)"]
    )
  })

  # extract test calibration results
  calibration_results <- purrr::map_dfr(outcome_vars, ~ safe_extract(.x)$result)

  # interpretations based on detail level
  create_interpretation <- function(result) {
    # use display label instead of raw outcome name
    display_name <- result$display_label

    base_interp <- glue::glue(
      "For {display_name}: The mean forest prediction has an estimate of {result$mean_prediction_estimate} ",
      "with a standard error of {result$mean_prediction_se}, resulting in a t-value of {result$mean_prediction_t} ",
      "and a {if(result$mean_prediction_p < alpha) 'statistically significant' else 'statistically non-significant'} ",
      "p-value of {format_pval(result$mean_prediction_p)}. ",
      "The differential forest prediction has an estimate of {result$differential_prediction_estimate} ",
      "with a standard error of {result$differential_prediction_se}, resulting in a t-value of {result$differential_prediction_t} ",
      "and a {if(result$differential_prediction_p < alpha) 'statistically significant' else 'statistically non-significant'} ",
      "p-value of {format_pval(result$differential_prediction_p)}. ",
      "{if(result$mean_prediction_p < alpha) 'We find the mean prediction is reliably calibrated for the held-out data.' else 'We find the mean prediction is not reliably calibrated for the held-out data.'} ",
      "{if(result$differential_prediction_p < alpha) 'We reject the null hypothesis of no heterogeneity.' else 'We fail to reject the null hypothesis of no heterogeneity.'}"
    )

    if (detail_level == "basic") {
      glue::glue(
        "For {display_name}: ",
        "{if(result$differential_prediction_p < alpha) 'We reject the null hypothesis of no heterogeneity' else 'We fail to reject the null hypothesis of no heterogeneity'} ",
        "(p={format_pval(result$differential_prediction_p)})."
      )
    } else {
      base_interp
    }
  }

  general_statement <- "Test calibration of the forest computes the best linear fit using (1) forest predictions on held-out data and (2) the mean forest prediction as regressors, with one-sided heteroskedasticity-robust (HC3) SEs. A coefficient of 1 for the mean forest prediction suggests that the mean prediction is accurate for the held-out data. A coefficient of 1 for the differential forest prediction suggests that the heterogeneity estimates are well calibrated. The p-value of the differential forest prediction coefficient acts as an omnibus test for the presence of heterogeneity."

  interpretations <- purrr::map_chr(1:nrow(calibration_results), ~ create_interpretation(calibration_results[.x, ]))
  interpretations <- c(general_statement, interpretations)

  # create a formatted summary table
  summary_table <- calibration_results[, c("display_label", "mean_prediction", "differential_prediction")]
  colnames(summary_table) <- c("Outcome", "Mean Prediction (SE)", "Differential Prediction (SE)")
  # fix row names to be clean numbers instead of the original outcome names
  row.names(summary_table) <- NULL

  # function to create concluding paragraph
  create_concluding_paragraph <- function(results) {
    reliable_means_outcomes <- results$outcome[results$mean_prediction_p < alpha]
    reliable_heterogeneity_outcomes <- results$outcome[results$differential_prediction_p < alpha]

    # convert to display labels
    reliable_means <- results$display_label[results$mean_prediction_p < alpha]
    reliable_heterogeneity <- results$display_label[results$differential_prediction_p < alpha]

    mean_conclusion <- if (length(reliable_means) > 1) {
      glue::glue("Models for {paste(reliable_means, collapse = ', ')} have reliably calibrated mean predictions on held-out data. We can be confident that these models predict similar average treatment effects on unseen data.")
    } else if (length(reliable_means) == 1) {
      glue::glue("The model for {reliable_means} has a reliably calibrated mean prediction on held-out data. We can be confident that this model predicts similar average treatment effects on unseen data.")
    } else {
      "We do not find that any of the outcomes here have reliably calibrated mean predictions on held-out data."
    }

    heterogeneity_conclusion <- if (length(reliable_heterogeneity) > 1) {
      glue::glue("Models for {paste(reliable_heterogeneity, collapse = ', ')} show evidence of heterogeneity on held-out data. We can be confident that these models predict heterogeneity for unseen data.")
    } else if (length(reliable_heterogeneity) == 1) {
      glue::glue("The model for {reliable_heterogeneity} shows evidence of heterogeneity on held-out data. We can be confident that this model predicts heterogeneity for unseen data.")
    } else {
      "We do not find evidence of heterogeneity for any of the models on held-out data."
    }

    explanation <- "Test calibration of the forest computes the best linear fit using (1) forest predictions on held-out data and (2) the mean forest prediction as regressors, with one-sided heteroskedasticity-robust (HC3) SEs. A coefficient of 1 for the mean forest prediction suggests that the mean prediction is accurate for the held-out data. A coefficient of 1 for the differential forest prediction suggests that the heterogeneity estimates are well calibrated. The p-value of the differential forest prediction coefficient acts as an omnibus test for the presence of heterogeneity."

    brief <- paste(explanation, mean_conclusion, heterogeneity_conclusion)
    full <- paste("In summary,", brief)

    list(brief = brief, full = full)
  }

  # Create concluding paragraphs
  concluding_paragraphs <- create_concluding_paragraph(calibration_results)
  interpretations <- c(interpretations, concluding_paragraphs$full)

  return(list(
    summary_table = summary_table,
    interpretations = interpretations,
    brief_interpretation = concluding_paragraphs$brief
  ))
}
