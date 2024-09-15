#' Interpret Qini Results for Both Binary and Multi-Arm Treatments
#'
#' This function serves as a wrapper to interpret Qini results for both binary and multi-arm treatments.
#' It calls the appropriate subfunction based on the structure of the input data.
#'
#' @param multi_batch The output of margot_batch_policy() function
#' @param label_mapping A named list mapping model names to labels (optional)
#' @param alpha The significance level for confidence intervals (default is 0.05)
#' @param decimal_places Number of decimal places for rounding (default is 2)
#'
#' @return A list containing summary tables and explanations for each arm or binary treatment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have multi_batch objects from margot_batch_policy()
#' label_mapping <- list(
#'   "t2_env_not_climate_chg_concern_z" = "Deny Climate Change Concern",
#'   "t2_env_not_climate_chg_cause_z" = "Deny Humans Cause Climate Change",
#'   "t2_env_not_climate_chg_real_z" = "Deny Climate Change Real",
#'   "t2_env_not_env_efficacy_z" = "Deny Personal Env Efficacy",
#'   "t2_env_sat_nz_environment_z" = "Not Sat NZ Environment"
#' )
#'
#' # For binary treatment
#' binary_results <- margot_interpret_qini(binary_multi_batch, label_mapping = label_mapping)
#'
#' # For multi-arm treatment
#' multi_arm_results <- margot_interpret_qini(multi_arm_multi_batch, label_mapping = label_mapping)
#' }
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

    return(explanation)
  }

  format_estimate_ci <- function(estimate, ci_lower, ci_upper) {
    if (any(is.na(c(estimate, ci_lower, ci_upper)))) {
      return("NA [NA, NA]")
    }
    sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
            estimate, ci_lower, ci_upper)
  }

  # Process models
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
  return(list(summary_table = summary_table, explanations = explanations))
}

#' @keywords internal
margot_interpret_qini_multi_arm<- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {
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
      # Remove 'model_' prefix if present
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
        # Preserve "NZ" capitalization
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
      "{if(!is.null(arm)) paste('to', arm, 'treatment') else ''} yields {reliability} {direction} ",
      "than using the average treatment effect (ATE) to assign treatment. The difference when prioritising conditional average treatment effects is {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} ",
      "[95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}]."
    )

    if (reliability == "is reliably" && direction == "better") {
      explanation <- gsub("is reliably", "**is reliably**", explanation)
      explanation <- paste(explanation, "This result suggests it may be beneficial to target CATE at this spend level.")
    }

    return(explanation)
  }

  # Function to format estimates with CI
  format_estimate_ci <- function(estimate, ci_lower, ci_upper) {
    sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
            estimate, ci_lower, ci_upper)
  }

  # Determine if it's a binary or multi-arm model
  is_binary <- "diff_gain_summary" %in% names(multi_batch[[1]])

  # Process multi-arm model
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

    # Create explanations
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

    results[[arm]] <- list(summary_table = summary_table, explanations = unlist(explanations))
  }

  return(results)
}


