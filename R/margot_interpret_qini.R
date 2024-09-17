#' Interpret Qini Results for Both Binary and Multi-Arm Treatments
#'
#' This function interprets Qini results for both binary and multi-arm treatments.
#' It detects the treatment type based on the input data structure and calls the appropriate sub-function.
#'
#' @param multi_batch List output from margot_batch_policy() with diff_gain_summaries.
#' @param label_mapping Optional named list mapping model names to readable labels.
#' @param alpha Significance level for confidence intervals. Default 0.05.
#' @param decimal_places Number of decimal places for rounding. Default 2.
#'
#' @return List with summary tables and explanations. Format varies for binary and multi-arm treatments.
#'
#' @examples
#' \dontrun{
#' label_mapping <- list(
#'   "t2_env_not_climate_chg_concern_z" = "Deny Climate Change Concern",
#'   "t2_env_not_climate_chg_cause_z" = "Deny Humans Cause Climate Change"
#' )
#'
#' binary_results <- margot_interpret_qini(binary_multi_batch, label_mapping = label_mapping)
#' print(binary_results$summary_table)
#'
#' multi_arm_results <- margot_interpret_qini(multi_arm_multi_batch, label_mapping = label_mapping)
#' print(multi_arm_results$arm1$summary_table)
#' }
#'
#' @references
#' Sverdrup, E., Wu, H., Athey, S., & Wager, S. (2024). Qini Curves for Multi-Armed Treatment Rules.
#' arXiv preprint arXiv:2306.11979.
#'
#' @export
margot_interpret_qini <- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {
  cli::cli_alert_info("Starting Qini interpretation...")

  # Determine if it's a binary or multi-arm model
  is_binary <- !("all_arms" %in% names(multi_batch[[1]]$diff_gain_summaries$spend_0.2))

  if (is_binary) {
    cli::cli_alert_success("Detected binary treatment model.")
    result <- margot_interpret_qini_binary(multi_batch, label_mapping, alpha, decimal_places)
  } else {
    cli::cli_alert_success("Detected multi-arm treatment model.")
    result <- margot_interpret_qini_multi_arm(multi_batch, label_mapping, alpha, decimal_places)
  }

  cli::cli_alert_info("Qini interpretation completed.")
  return(result)
}

#' @keywords internal
margot_interpret_qini_binary <- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {
  cli::cli_alert_info("Processing binary treatment model...")

  # qini explanation (function below)
  qini_explanation <- create_qini_explanation()


  # Helper functions
  extract_estimates <- function(diff_gain_summary) {
    tryCatch({
      if (is.null(diff_gain_summary$diff_gain)) {
        cli::cli_alert_warning(paste("diff_gain is NULL in diff_gain_summary"))
        return(c(estimate = NA, ci_lower = NA, ci_upper = NA))
      }

      estimate_match <- regexec("^([-]?\\d+\\.?\\d*)", diff_gain_summary$diff_gain)
      estimate <- as.numeric(regmatches(diff_gain_summary$diff_gain, estimate_match)[[1]][2])

      se_match <- regexec("\\((SE: )?(\\d+\\.\\d+)\\)", diff_gain_summary$diff_gain)
      se <- as.numeric(regmatches(diff_gain_summary$diff_gain, se_match)[[1]][3])

      if (is.na(estimate) || is.na(se)) {
        cli::cli_alert_warning(paste("Unable to extract estimate or SE from:", diff_gain_summary$diff_gain))
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
    original_label <- label
    if (!is.null(label_mapping)) {
      # Remove 'model_' prefix if present
      clean_label <- sub("^model_", "", label)
      if (clean_label %in% names(label_mapping)) {
        return(label_mapping[[clean_label]])
      } else if (label %in% names(label_mapping)) {
        return(label_mapping[[label]])
      }
    }
    # If no mapping found, apply default transformations
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
      "For the outcome {transform_label(model_name)}, at the {spend*100}% spend level, using the conditional average treatment effect (CATE) to prioritise treatments ",
      "{reliability} {direction} ",
      "than using the average treatment effect (ATE) to assign treatment. The difference when prioritising conditional average treatment effects is {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} ",
      "[95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}]."
    )

    # Add conditional statement for when CATE is reliably better or worse than ATE

    if (reliability == "is reliably" && direction == "better") {
      explanation <- gsub("is reliably", "**is reliably**", explanation)
      explanation <- paste(explanation, "**This result suggests that prioritising CATE may increase the average treatment response relative to no prioritisation at this spend level.**")
    } else if (reliability == "is reliably" && direction == "worse") {
      explanation <- gsub("is reliably", "**is reliably**", explanation)
      explanation <- paste(explanation, "**This result suggests that prioritising CATE may decrease the average treatment response relative to no prioritisation at this spend level.**")
    }
    return(explanation)
  }

  # format ci
  format_estimate_ci <- function(estimate, ci_lower, ci_upper) {
    if (any(is.na(c(estimate, ci_lower, ci_upper)))) {
      return("NA [NA, NA]")
    }
    formatted <- sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
                         estimate, ci_lower, ci_upper)
    # Bold for reliably positive, italics for reliably negative
    if (ci_lower > 0) {
      formatted <- paste0("**", formatted, "**")
    } else if (ci_upper < 0) {
      formatted <- paste0("*", formatted, "*")
    }
    return(formatted)
  }

  # process models
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

  # Create summary table
  cli::cli_alert_info("Creating summary table...")
  summary_table <- summary_data %>%
    tidyr::pivot_wider(
      names_from = Spend,
      values_from = estimate_ci,
      names_prefix = "Spend "
    ) %>%
    dplyr::rename_with(~paste0(., "%"), -Model)

  # Create explanations
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

  # Apply label_mapping to explanation names
  names(explanations) <- purrr::map_chr(names(multi_batch), transform_label)

  cli::cli_alert_success("Binary treatment model processing completed.")
  return(list(
    summary_table = summary_table,
    explanations = explanations,
    qini_explanation = qini_explanation
  ))
}
#' @keywords internal
margot_interpret_qini_multi_arm <- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {

  # qini explanation function below
  qini_explanation <- create_qini_explanation()


  # Function to extract estimates and create CI
  extract_estimates <- function(diff_gain_summary) {
    estimate <- as.numeric(gsub(".*?([-]?\\d+\\.\\d+).*", "\\1", diff_gain_summary$diff_gain))
    se <- as.numeric(gsub(".*?(\\d+\\.\\d+)\\).*", "\\1", diff_gain_summary$diff_gain))
    ci_lower <- estimate - qnorm(1 - alpha/2) * se
    ci_upper <- estimate + qnorm(1 - alpha/2) * se
    return(c(estimate = estimate, ci_lower = ci_lower, ci_upper = ci_upper))
  }

  # Function to transform labels
  transform_label <- function(label) {
    original_label <- label
    if (!is.null(label_mapping)) {
      clean_label <- sub("^model_", "", label)
      if (clean_label %in% names(label_mapping)) {
        label <- label_mapping[[clean_label]]
      } else if (label %in% names(label_mapping)) {
        label <- label_mapping[[label]]
      } else {
        label <- sub("^model_", "", label)
        label <- sub("^t[0-9]+_", "", label)
        label <- sub("_z$", "", label)
        label <- gsub("_", " ", label)
        label <- tools::toTitleCase(label)
        label <- gsub("Nz", "NZ", label)
      }
    }
    return(label)
  }

  # Function to create explanation
  create_explanation <- function(diff_gain_summary, model_name, spend, arm = NULL) {
    estimates <- extract_estimates(diff_gain_summary)
    direction <- if(estimates["estimate"] > 0) "better" else if(estimates["estimate"] < 0) "worse" else "indistinguishable"
    reliability <- if(estimates["ci_lower"] * estimates["ci_upper"] > 0) "is reliably" else "is not reliably"

    explanation <- glue::glue(
      "For the outcome {transform_label(model_name)}, at the {spend*100}% spend level, using the conditional average treatment effect (CATE) to prioritise treatments ",
      "{reliability} {direction} ",
      "than assigning treatments without priorities. The difference when prioritising conditional average treatment effects is {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} ",
      "[95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}]."
    )

    if (reliability == "is reliably" && direction == "better") {
      explanation <- gsub("is reliably", "**is reliably**", explanation)
      explanation <- paste(explanation, "**This result suggests that prioritising CATE may increase the average treatment response relative to no prioritisation at this spend level.**")
    } else if (reliability == "is reliably" && direction == "worse") {
      explanation <- gsub("is reliably", "**is reliably**", explanation)
      explanation <- paste(explanation, "**This result suggests that prioritising CATE may decrease the average treatment response relative to no prioritisation at this spend level.**")
    }

    return(explanation)
  }

  # function to format estimates with CI
  format_estimate_ci <- function(estimate, ci_lower, ci_upper) {
    formatted <- sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
                         estimate, ci_lower, ci_upper)
    # Bold for reliably positive, italics for reliably negative
    if (ci_lower > 0) {
      formatted <- paste0("**", formatted, "**")
    } else if (ci_upper < 0) {
      formatted <- paste0("*", formatted, "*")
    }
    return(formatted)
  }

  # process multi-arm model
  arms <- c("all_arms", names(multi_batch[[1]]$diff_gain_summaries$spend_0.2)[-1])
  results <- list()

  for (arm in arms) {
    summary_table <- purrr::map_dfr(names(multi_batch), function(model_name) {
      model_results <- multi_batch[[model_name]]$diff_gain_summaries
      purrr::map_dfr(names(model_results), function(spend) {
        estimates <- extract_estimates(model_results[[spend]][[arm]])
        data.frame(
          Model = transform_label(model_name),
          Spend = as.numeric(gsub("spend_", "", spend)) * 100,
          estimate_ci = format_estimate_ci(estimates["estimate"], estimates["ci_lower"], estimates["ci_upper"])
        )
      })
    })

    # Reshape the summary table
    summary_table <- summary_table %>%
      tidyr::pivot_wider(
        names_from = Spend,
        values_from = estimate_ci,
        names_prefix = "Spend "
      ) %>%
      dplyr::rename_with(~paste0(., "%"), -Model)

    # Create explanations grouped by model
    explanations <- purrr::map(names(multi_batch), function(model_name) {
      model_results <- multi_batch[[model_name]]$diff_gain_summaries
      explanation <- purrr::map_chr(names(model_results), function(spend) {
        create_explanation(
          model_results[[spend]][[arm]],
          model_name,
          as.numeric(gsub("spend_", "", spend)),
          arm
        )
      })
      paste(explanation, collapse = "\n\n")  # Add paragraph stop between spend levels
    })

    # Name the explanations with transformed labels
    names(explanations) <- purrr::map_chr(names(multi_batch), transform_label)

    results[[arm]] <- list(summary_table = summary_table, explanations = explanations)

  }

  # add qini expanation
  results$qini_explanation <- qini_explanation
  return(results)
}


#' @keywords internal
create_qini_explanation <- function() {
  explanation <- paste(
    "The values in the table and explanations represent the difference in gain between two Qini curves at various spend levels, along with 95% confidence intervals.",
    "Given two Qini curves, Q_a and Q_b, we obtain an estimate of the difference Q_a(B) - Q_b(B), at a spend level B.",
    "This difference indicates how much better (or worse) the conditional average treatment effect (CATE) performs compared to the average treatment effect (ATE) or no prioritisation, depending on the context.",
    "Positive values suggest that using CATE for treatment prioritisation may lead to higher average responses at that spend level, while negative values suggest it may lead to lower average responses.",
    sep = "\n"
  )
  return(explanation)
}
