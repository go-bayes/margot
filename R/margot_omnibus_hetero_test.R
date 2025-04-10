#' @keywords internal
#' for margot_plot()
transform_label <- function(label, label_mapping = NULL, options = list()) {
  original_label <- label

  # Apply mapping with partial substitutions and remove numbers
  if (!is.null(label_mapping)) {
    for (pattern in names(label_mapping)) {
      if (grepl(pattern, label, fixed = TRUE)) {
        replacement <- label_mapping[[pattern]]
        label <- gsub(pattern, replacement, label, fixed = TRUE)
        cli::cli_alert_info("Mapped label: {pattern} -> {replacement}")
      }
    }
  }

  # Remove the numerical part (e.g., " - (3.0,7.0] - [1.0,2.0]")
  label <- sub(" - \\(.*\\]$", "", label)

  # Apply default transformations if the label wasn't fully replaced
  if (label == original_label) {
    if (options$remove_tx_prefix) {
      label <- sub("^t[0-9]+_", "", label)
    }
    if (options$remove_z_suffix) {
      label <- sub("_z$", "", label)
    }
    if (options$remove_underscores) {
      label <- gsub("_", " ", label)
    }
    if (options$use_title_case) {
      label <- tools::toTitleCase(label)
      # Preserve "NZ" capitalization
      label <- gsub("Nz", "NZ", label)
    }
  }

  if (label != original_label) {
    cli::cli_alert_info("Transformed label: {original_label} -> {label}")
  }

  return(label)
}


#' Omnibus Heterogeneity Test for GRF Models
#'
#' This function performs an omnibus heterogeneity test for specified models
#' outputted from margot::margot_run_models_grf() and provides interpretations in a readable format.
#'
#' @param model_results A list of model results from margot::margot_run_models_grf().
#' @param outcome_vars Optional. A character vector of outcome variable names. If NULL,
#'        the function will attempt to use the outcome_vars from the model_results input.
#' @param alpha Significance level for hypothesis tests. Default is 0.05.
#' @param detail_level Character string specifying the level of detail in the output.
#'        Options are "basic", "standard" (default), or "detailed".
#' @param label_mapping Optional. A named list mapping outcome variable names to display labels.
#'        For example: list("t2_agreeableness_z" = "Agreeableness").
#' @param format Output format: "table" (default), "markdown", or "text".
#'        "table" returns a tibble for use with tidyverse tools.
#'        "markdown" returns formatted markdown text for Quarto documents.
#'        "text" returns plain text interpretations.
#'
#' @return A list containing:
#'   - summary_table: A tibble with all test results
#'   - interpretations: Results formatted according to the format parameter
#'   - brief_interpretation: A concise summary of all results
#'
#' @importFrom dplyr bind_rows mutate select filter group_by summarise arrange %>%
#' @importFrom purrr map_dfr map_chr safely
#' @importFrom glue glue
#' @importFrom stringr str_extract str_wrap
#' @importFrom knitr kable
#' @importFrom tibble tibble
#'
#' @export
margot_omnibus_hetero_test <- function(model_results, outcome_vars = NULL, alpha = 0.05,
                                       detail_level = "standard", label_mapping = NULL,
                                       format = "table") {
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
  if (!format %in% c("table", "markdown", "text")) {
    stop("format must be 'table', 'markdown', or 'text'")
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

  # helper function to format p-values - vectorized version
  format_pval <- function(p) {
    sapply(p, function(x) {
      if (x < 0.001) {
        return(sprintf("%.2e", x))
      }
      return(sprintf("%.3f", x))
    })
  }

  # safe version of the extraction function
  safe_extract <- safely(function(outcome) {
    model_name <- paste0("model_", outcome)
    if (!model_name %in% names(model_results$results)) {
      stop(glue::glue("Model for outcome '{outcome}' not found in model_results"))
    }

    calib <- model_results$results[[model_name]]$test_calibration
    tibble::tibble(
      outcome = outcome,
      display_label = outcome_to_display[outcome],
      mean_prediction_estimate = round(calib["mean.forest.prediction", "Estimate"], 2),
      mean_prediction_se = round(calib["mean.forest.prediction", "Std. Error"], 2),
      mean_prediction_t = round(calib["mean.forest.prediction", "Estimate"] /
                                  calib["mean.forest.prediction", "Std. Error"], 2),
      mean_prediction_p = calib["mean.forest.prediction", "Pr(>t)"],
      mean_prediction_significant = mean_prediction_p < alpha,
      differential_prediction_estimate = round(calib["differential.forest.prediction", "Estimate"], 2),
      differential_prediction_se = round(calib["differential.forest.prediction", "Std. Error"], 2),
      differential_prediction_t = round(calib["differential.forest.prediction", "Estimate"] /
                                          calib["differential.forest.prediction", "Std. Error"], 2),
      differential_prediction_p = calib["differential.forest.prediction", "Pr(>t)"],
      differential_prediction_significant = differential_prediction_p < alpha
    )
  })

  # extract test calibration results
  calibration_results <- purrr::map_dfr(outcome_vars, ~ safe_extract(.x)$result)

  # add formatted text columns for display
  calibration_results <- calibration_results %>%
    dplyr::mutate(
      mean_prediction_text = glue::glue("{mean_prediction_estimate} ({mean_prediction_se})"),
      differential_prediction_text = glue::glue("{differential_prediction_estimate} ({differential_prediction_se})"),
      mean_prediction_p_formatted = format_pval(mean_prediction_p),
      differential_prediction_p_formatted = format_pval(differential_prediction_p),
      calibration_status = ifelse(mean_prediction_significant,
                                  "Calibrated", "Not calibrated"),
      heterogeneity_status = ifelse(differential_prediction_significant,
                                    "Heterogeneity present", "No heterogeneity")
    )

  # create a clean summary table for display
  summary_table <- calibration_results %>%
    dplyr::select(
      Outcome = display_label,
      `Mean Est. (SE)` = mean_prediction_text,
      `Mean p-value` = mean_prediction_p_formatted,
      `Mean Status` = calibration_status,
      `Diff. Est. (SE)` = differential_prediction_text,
      `Diff. p-value` = differential_prediction_p_formatted,
      `Heterogeneity` = heterogeneity_status
    )

  # for internal use, create grouped results
  results_by_calibration <- calibration_results %>%
    dplyr::group_by(mean_prediction_significant) %>%
    dplyr::summarise(outcomes = list(display_label))

  results_by_heterogeneity <- calibration_results %>%
    dplyr::group_by(differential_prediction_significant) %>%
    dplyr::summarise(outcomes = list(display_label))

  # create general explanation
  general_explanation <- "Test calibration of the forest computes the best linear fit using:
  1. Forest predictions on held-out data
  2. The mean forest prediction as regressors

  Testing uses heteroskedasticity-robust (HC3) standard errors. A coefficient of 1 for:
  - Mean forest prediction: indicates the mean prediction is accurate for held-out data
  - Differential forest prediction: indicates heterogeneity estimates are well calibrated

  The p-value of the differential forest prediction coefficient acts as an omnibus test for heterogeneity."

  # ensure these summaries are defined for all code paths
  calibrated_models <- calibration_results %>%
    dplyr::filter(mean_prediction_significant) %>%
    dplyr::pull(display_label)

  heterogeneous_models <- calibration_results %>%
    dplyr::filter(differential_prediction_significant) %>%
    dplyr::pull(display_label)

  # create summaries
  if (length(calibrated_models) > 0) {
    if (length(calibrated_models) > 5) {
      mean_summary <- glue::glue(
        "{length(calibrated_models)} of {nrow(calibration_results)} models have reliably calibrated mean predictions on held-out data."
      )
    } else {
      mean_summary <- glue::glue(
        "Models for {paste(calibrated_models, collapse = ', ')} have reliably calibrated mean predictions on held-out data."
      )
    }
  } else {
    mean_summary <- "None of the models have reliably calibrated mean predictions on held-out data."
  }

  if (length(heterogeneous_models) > 0) {
    if (length(heterogeneous_models) > 5) {
      heterogeneity_summary <- glue::glue(
        "{length(heterogeneous_models)} of {nrow(calibration_results)} models show evidence of heterogeneity on held-out data."
      )
    } else {
      heterogeneity_summary <- glue::glue(
        "Models for {paste(heterogeneous_models, collapse = ', ')} show evidence of heterogeneity on held-out data."
      )
    }
  } else {
    heterogeneity_summary <- "None of the models show evidence of heterogeneity on held-out data."
  }

  # Create interpretations based on detail level and format
  if (format == "table") {
    # for table format, return the raw tibble
    interpretations <- calibration_results
  } else {
    # for markdown and text formats

    # create individual outcome interpretations
    create_outcome_interpretation <- function(result) {
      display_name <- result$display_label

      if (detail_level == "basic") {
        return(glue::glue(
          "**{display_name}**: {result$heterogeneity_status} (p={result$differential_prediction_p_formatted}). ",
          "Mean prediction is {tolower(result$calibration_status)} (p={result$mean_prediction_p_formatted})."
        ))
      } else {
        return(glue::glue(
          "**{display_name}**:\n",
          "- Mean prediction: {result$mean_prediction_estimate} (SE={result$mean_prediction_se}), ",
          "t={result$mean_prediction_t}, p={result$mean_prediction_p_formatted}, {tolower(result$calibration_status)}.\n",
          "- Differential prediction: {result$differential_prediction_estimate} (SE={result$differential_prediction_se}), ",
          "t={result$differential_prediction_t}, p={result$differential_prediction_p_formatted}, {tolower(result$heterogeneity_status)}."
        ))
      }
    }

    # generate individual interpretations
    individual_interpretations <- purrr::map_chr(
      seq_len(nrow(calibration_results)),
      ~ create_outcome_interpretation(calibration_results[.x, ])
    )

    # combine summaries
    summary_paragraph <- glue::glue(
      "## Summary\n\n",
      "{mean_summary} {heterogeneity_summary}"
    )

    # combine all text components based on format
    if (format == "markdown") {
      interpretations <- c(
        "# Omnibus Heterogeneity Test Results",
        "",
        "## Explanation",
        general_explanation,
        "",
        "## Results by Outcome",
        individual_interpretations,
        "",
        summary_paragraph
      )

      # join with double newlines for markdown formatting
      interpretations <- paste(interpretations, collapse = "\n\n")
    } else { # text format
      interpretations <- c(
        "OMNIBUS HETEROGENEITY TEST RESULTS",
        "",
        "EXPLANATION:",
        general_explanation,
        "",
        "RESULTS BY OUTCOME:",
        individual_interpretations,
        "",
        gsub("##", "", summary_paragraph)
      )

      # join with newlines for text formatting
      interpretations <- paste(interpretations, collapse = "\n")
    }
  }

  # create brief interpretation for all formats - single paragraph for scientific articles
  brief_interpretation <- paste(
    "Test calibration of the forest computed the best linear fit using forest predictions on held-out data and the mean forest prediction as regressors, with heteroskedasticity-robust (HC3) standard errors.",
    "A coefficient of 1 for the mean forest prediction suggests accurate prediction for held-out data, while a coefficient of 1 for the differential forest prediction suggests well-calibrated heterogeneity estimates.",
    "The p-value of the differential forest prediction coefficient acted as an omnibus test for heterogeneity.",
    mean_summary,
    heterogeneity_summary,
    sep = " "
  )

  # Return results based on format
  if (format == "table") {
    # for table format, convert interpretations to character to ensure it's usable with cat()
    interpretations_text <- paste(
      "Results for", nrow(calibration_results), "models analyzed:",
      paste0("\n- ", mean_summary),
      paste0("\n- ", heterogeneity_summary)
    )

    return(list(
      summary_table = summary_table,
      interpretations = interpretations_text,
      brief_interpretation = brief_interpretation,
      results_table = calibration_results
    ))
  } else {
    return(list(
      summary_table = summary_table,
      interpretations = interpretations,
      brief_interpretation = brief_interpretation
    ))
  }
}
