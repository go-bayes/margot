#' Interpret Qini Results for Both Binary and Multi-Arm Treatments
#'
#' This function interprets Qini results for both binary and multi-arm treatments.
#' It detects the treatment type based on the input data structure and calls the
#' appropriate sub-function. A new parameter, \code{model_names}, restricts the
#' analysis to the specified models; if not supplied, all models are processed.
#'
#' @param multi_batch List output from margot_batch_policy() with diff_gain_summaries.
#' @param label_mapping Optional named list mapping model names to readable labels.
#' @param alpha Significance level for confidence intervals. Default is 0.05.
#' @param decimal_places Number of decimal places for rounding. Default is 2.
#' @param model_names Optional character vector specifying models to process.
#'        Default is NULL (i.e. process all).
#'
#' @return A list with a summary table and a single combined explanation in
#'         \code{qini_explanation} that starts with a header "### Explanation of Qini Curves".
#'
#' @export
margot_interpret_qini <- function(multi_batch,
                                  label_mapping = NULL,
                                  alpha = 0.05,
                                  decimal_places = 2,
                                  model_names = NULL) {
  cli::cli_alert_info("Starting Qini interpretation...")

  # Restrict processing to selected models if model_names is provided.
  if (!is.null(model_names) && length(model_names) > 0) {
    multi_batch <- multi_batch[model_names]
  }

  # Determine if the model is binary or multi-arm by checking the diff_gain_summaries structure.
  is_binary <- !("all_arms" %in% names(multi_batch[[1]]$diff_gain_summaries$spend_0.2))

  if (is_binary) {
    cli::cli_alert_success("Detected binary treatment model.")
    result <- margot_interpret_qini_binary(multi_batch, label_mapping, alpha, decimal_places)
  } else {
    cli::cli_alert_success("Detected multi-arm treatment model.")
    result <- margot_interpret_qini_multi_arm(multi_batch, label_mapping, alpha, decimal_places)
  }

  # Combine the common explanation with the individual model explanations into one block.
  combined_explanation <- paste0(
    "### Explanation of Qini Curves\n\n",
    result$qini_explanation, "\n\n",
    paste(sapply(names(result$explanations), function(model) {
      paste0("**", model, "**\n", result$explanations[[model]])
    }), collapse = "\n\n")
  )

  cli::cli_alert_info("Qini interpretation completed.")
  # Return only the summary table and the combined explanation.
  return(list(
    summary_table   = result$summary_table,
    qini_explanation = combined_explanation
  ))
}

#' @keywords internal
margot_interpret_qini_binary <- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {
  cli::cli_alert_info("Processing binary treatment model...")

  # Use the new explanation for binary Qini curves.
  qini_explanation <- create_qini_explanation_binary()

  extract_estimates <- function(diff_gain_summary) {
    tryCatch({
      if (is.null(diff_gain_summary$diff_gain)) {
        cli::cli_alert_warning("diff_gain is NULL in diff_gain_summary")
        return(c(estimate = NA, ci_lower = NA, ci_upper = NA))
      }
      estimate_match <- regexec("^([-]?\\d+\\.?\\d*)", diff_gain_summary$diff_gain)
      estimate <- as.numeric(regmatches(diff_gain_summary$diff_gain, estimate_match)[[1]][2])
      se_match <- regexec("\\((SE: )?(\\d+\\.\\d+)\\)", diff_gain_summary$diff_gain)
      se <- as.numeric(regmatches(diff_gain_summary$diff_gain, se_match)[[1]][3])
      if (is.na(estimate) || is.na(se)) {
        cli::cli_alert_warning("Unable to extract estimate or SE from diff_gain")
        return(c(estimate = NA, ci_lower = NA, ci_upper = NA))
      }
      ci_lower <- estimate - qnorm(1 - alpha/2) * se
      ci_upper <- estimate + qnorm(1 - alpha/2) * se
      return(c(estimate = estimate, ci_lower = ci_lower, ci_upper = ci_upper))
    }, error = function(e) {
      cli::cli_alert_warning(paste("Error in extract_estimates:", e$message))
      return(c(estimate = NA, ci_lower = NA, ci_upper = NA))
    })
  }

  transform_label <- function(label) {
    if (!is.null(label_mapping)) {
      clean_label <- sub("^model_", "", label)
      if (clean_label %in% names(label_mapping)) return(label_mapping[[clean_label]])
      if (label %in% names(label_mapping)) return(label_mapping[[label]])
    }
    label <- sub("^model_", "", label)
    label <- sub("^t[0-9]+_", "", label)
    label <- sub("_z$", "", label)
    label <- gsub("_", " ", label)
    label <- tools::toTitleCase(label)
    label <- gsub("Nz", "NZ", label)
    return(label)
  }

  create_explanation <- function(diff_gain_summary, model_name, spend) {
    estimates <- extract_estimates(diff_gain_summary)
    if (any(is.na(estimates))) {
      return(paste("Unable to create explanation for", model_name, "at", spend*100, "% spend level due to missing estimates."))
    }
    direction <- if(estimates["estimate"] > 0) "better" else if(estimates["estimate"] < 0) "worse" else "indistinguishable"
    reliability <- if(estimates["ci_lower"] * estimates["ci_upper"] > 0) "is reliably" else "is not reliably"
    explanation <- glue::glue(
      "For the outcome {transform_label(model_name)}, at the {spend*100}% spend level, using the conditional average treatment effect (CATE) to prioritise treatments {reliability} {direction} than using the average treatment effect (ATE) to assign treatment. The difference when prioritising CATE is {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} [95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}]."
    )
    if (reliability == "is reliably" && direction == "better") {
      explanation <- gsub("is reliably", "is reliably", explanation)
      explanation <- paste(explanation, "This result suggests that prioritising CATE may increase the average treatment response relative to no prioritisation at this spend level.")
    } else if (reliability == "is reliably" && direction == "worse") {
      explanation <- gsub("is reliably", "**is reliably**", explanation)
      explanation <- paste(explanation, "This result suggests that prioritising CATE may decrease the average treatment response relative to no prioritisation at this spend level.")
    }
    return(explanation)
  }

  format_estimate_ci <- function(estimate, ci_lower, ci_upper) {
    if (any(is.na(c(estimate, ci_lower, ci_upper)))) return("NA [NA, NA]")
    formatted <- sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
                         estimate, ci_lower, ci_upper)
    if (ci_lower > 0) {
      formatted <- paste0("**", formatted, "**")
    } else if (ci_upper < 0) {
      formatted <- paste0("*", formatted, "*")
    }
    return(formatted)
  }

  cli::cli_alert_info("Processing individual models...")
  summary_data <- purrr::map_dfr(names(multi_batch), function(model_name) {
    cli::cli_alert_info(paste("Processing model:", model_name))
    model_results <- multi_batch[[model_name]]$diff_gain_summaries
    purrr::map_dfr(names(model_results), function(spend) {
      estimates <- extract_estimates(model_results[[spend]])
      data.frame(
        Model = transform_label(model_name),
        Spend = as.numeric(gsub("spend_", "", spend)) * 100,
        estimate_ci = format_estimate_ci(estimates["estimate"], estimates["ci_lower"], estimates["ci_upper"])
      )
    })
  })

  cli::cli_alert_info("Creating summary table...")
  summary_table <- summary_data %>%
    tidyr::pivot_wider(
      names_from = Spend,
      values_from = estimate_ci,
      names_prefix = "Spend "
    ) %>%
    dplyr::rename_with(~paste0(., "%"), -Model)

  cli::cli_alert_info("Generating explanations...")
  explanations <- purrr::map(names(multi_batch), function(model_name) {
    cli::cli_alert_info(paste("Generating explanation for model:", model_name))
    model_results <- multi_batch[[model_name]]$diff_gain_summaries
    spend_levels <- names(model_results)
    explanation <- purrr::map_chr(spend_levels, function(spend) {
      create_explanation(
        model_results[[spend]],
        model_name,
        as.numeric(gsub("spend_", "", spend))
      )
    })
    paste(explanation, collapse = "\n\n")
  })
  names(explanations) <- purrr::map_chr(names(multi_batch), transform_label)

  cli::cli_alert_success("Binary treatment model processing completed.")
  return(list(
    summary_table = summary_table,
    explanations = explanations,
    qini_explanation = qini_explanation
  ))
}

#' @keywords internal
create_qini_explanation_binary <- function() {
  explanation <- paste(
    "The values in the table and explanations represent the difference in gain between two Qini curves at various spend levels, along with 95% confidence intervals.",
    "Given two Qini curves, Q_a and Q_b, we obtain an estimate of the difference Q_a(B) - Q_b(B), at a spend level B.",
    "This difference indicates how much better (or worse) using the conditional average treatment effect (CATE) to prioritise treatments performs compared to using the average treatment effect (ATE) or no prioritisation.",
    "Positive values suggest that prioritising treatment based on CATE may lead to higher average responses at that spend level, while negative values suggest it may lead to lower average responses.",
    sep = "\n"
  )
  return(explanation)
}

