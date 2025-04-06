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
#' @param format Output format for tables. Can be "markdown" (default) or "latex".
#'
#' @return A list with summary tables in both markdown and latex formats, and a single
#'         combined explanation in \code{qini_explanation} that starts with a
#'         header "### Explanation of Qini Curves".
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom glue glue
#' @importFrom purrr map_dfr map_chr map
#' @importFrom dplyr rename_with
#' @importFrom tidyr pivot_wider
#' @importFrom stats qnorm
#' @export
margot_interpret_qini <- function(multi_batch,
                                  label_mapping = NULL,
                                  alpha = 0.05,
                                  decimal_places = 2,
                                  model_names = NULL,
                                  format = "markdown") {
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

  # Return the requested format, or both if not specified
  if (format == "markdown") {
    summary_table <- result$summary_table$markdown
  } else if (format == "latex") {
    summary_table <- result$summary_table$latex
  } else {
    summary_table <- result$summary_table
  }

  return(list(
    summary_table   = summary_table,
    qini_explanation = combined_explanation
  ))
}

#' @keywords internal
margot_interpret_qini_binary <- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {
  cli::cli_alert_info("Processing binary treatment model...")

  # Use the new explanation for binary Qini curves.
  qini_explanation <- create_qini_explanation_binary()

  # Wrapper to transform model labels using the package helper.
  transform_label_wrapper <- function(label) {
    original_label <- label
    if (!is.null(label_mapping)) {
      # Remove "model_" prefix before checking mapping.
      clean_label <- sub("^model_", "", label)
      if (clean_label %in% names(label_mapping)) {
        mapped <- label_mapping[[clean_label]]
        cli::cli_alert_info("Mapped label: {original_label} -> {mapped}")
        return(mapped)
      }
      if (label %in% names(label_mapping)) {
        mapped <- label_mapping[[label]]
        cli::cli_alert_info("Mapped label: {original_label} -> {mapped}")
        return(mapped)
      }
    }
    # Remove "model_" prefix and then apply the helper transformation.
    label <- sub("^model_", "", label)
    transformed <- transform_label(label, label_mapping = NULL, options = list(
      remove_tx_prefix   = TRUE,
      remove_z_suffix    = TRUE,
      remove_underscores = TRUE,
      use_title_case     = TRUE
    ))
    return(transformed)
  }

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
      ci_lower <- estimate - stats::qnorm(1 - alpha/2) * se
      ci_upper <- estimate + stats::qnorm(1 - alpha/2) * se
      return(c(estimate = estimate, ci_lower = ci_lower, ci_upper = ci_upper))
    }, error = function(e) {
      cli::cli_alert_warning(paste("Error in extract_estimates:", e$message))
      return(c(estimate = NA, ci_lower = NA, ci_upper = NA))
    })
  }

  create_explanation <- function(diff_gain_summary, model_name, spend) {
    estimates <- extract_estimates(diff_gain_summary)
    if (any(is.na(estimates))) {
      return(paste("Unable to create explanation for", model_name, "at", spend * 100, "% spend level due to missing estimates."))
    }

    # Determine direction and reliability.
    direction <- if (estimates["estimate"] > 0) "better" else if (estimates["estimate"] < 0) "worse" else "indistinguishable"
    reliably <- estimates["ci_lower"] * estimates["ci_upper"] > 0

    if (!reliably) {
      return(paste("At the", spend * 100, "% spend level for outcome", transform_label_wrapper(model_name),
                   "there is no statistically reliable evidence to support using CATE to prioritise treatments."))
    }

    # Revised explanation for reliably negative results
    if (direction == "worse") {
      return(paste("At the", spend * 100, "% spend level for outcome", transform_label_wrapper(model_name),
                   "the evidence indicates a CAUTION: Using CATE to prioritise treatments would lead to reliably worse average responses (",
                   format(round(estimates['estimate'], decimal_places), nsmall = decimal_places),
                   " [95% CI: ",
                   format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places),
                   ", ",
                   format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places),
                   "]) compared to using the ATE. For this outcome, targeting the CATE is not recommended."))
    }

    # If evidence is reliably positive, output the enhanced explanation
    explanation <- glue::glue(
      "For the outcome {transform_label_wrapper(model_name)}, at the {spend * 100}% spend level, using the conditional average treatment effect (CATE) to prioritise treatments is reliably better than using the average treatment effect (ATE) to assign treatment. The difference when prioritising CATE is {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} [95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}]."
    )
    explanation <- paste(explanation, "This result suggests that prioritising CATE may increase the average treatment response relative to no prioritisation at this spend level.")
    return(explanation)
  }

  format_estimate_ci <- function(estimate, ci_lower, ci_upper, format = "markdown") {
    if (any(is.na(c(estimate, ci_lower, ci_upper)))) return("NA [NA, NA]")

    formatted <- sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
                         estimate, ci_lower, ci_upper)

    # Format based on the output type
    if (format == "latex") {
      # LaTeX formatting
      if (ci_lower > 0) {
        return(paste0("\\textbf{", formatted, "}"))
      }
      else if (ci_upper < 0) {
        return(paste0("(!!) \\textit{", formatted, "}"))
      }
    } else {
      # Markdown formatting
      if (ci_lower > 0) {
        return(paste0("**", formatted, "**"))
      }
      else if (ci_upper < 0) {
        return(paste0("(!!) *", formatted, "*"))
      }
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
        Model     = transform_label_wrapper(model_name),
        Spend     = as.numeric(gsub("spend_", "", spend)) * 100,
        estimate_ci = format_estimate_ci(estimates["estimate"], estimates["ci_lower"], estimates["ci_upper"])
      )
    })
  })

  cli::cli_alert_info("Creating summary table...")

  # Create two versions of the summary table - one for markdown and one for latex
  summary_data_md <- tidyr::pivot_wider(
    summary_data,
    names_from = Spend,
    values_from = estimate_ci,
    names_prefix = "Spend "
  ) %>%
    dplyr::rename_with(~paste0(., "%"), -Model)

  # For LaTeX output, reprocess with LaTeX formatting
  summary_data_latex <- purrr::map_dfr(names(multi_batch), function(model_name) {
    model_results <- multi_batch[[model_name]]$diff_gain_summaries
    purrr::map_dfr(names(model_results), function(spend) {
      estimates <- extract_estimates(model_results[[spend]])
      data.frame(
        Model     = transform_label_wrapper(model_name),
        Spend     = as.numeric(gsub("spend_", "", spend)) * 100,
        estimate_ci = format_estimate_ci(estimates["estimate"], estimates["ci_lower"],
                                         estimates["ci_upper"], format = "latex")
      )
    })
  })

  summary_table_latex <- tidyr::pivot_wider(
    summary_data_latex,
    names_from = Spend,
    values_from = estimate_ci,
    names_prefix = "Spend "
  ) %>%
    dplyr::rename_with(~paste0(., "%"), -Model)

  # Create a list with both formats
  summary_table <- list(
    markdown = summary_data_md,
    latex = summary_table_latex
  )

  cli::cli_alert_info("Generating explanations...")
  explanations <- purrr::map(names(multi_batch), function(model_name) {
    cli::cli_alert_info(paste("Generating explanation for model:", model_name))
    model_results <- multi_batch[[model_name]]$diff_gain_summaries
    spend_levels <- names(model_results)
    explanations_by_spend <- purrr::map_chr(spend_levels, function(spend) {
      create_explanation(
        model_results[[spend]],
        model_name,
        as.numeric(gsub("spend_", "", spend))
      )
    })

    # Group explanations into positive, negative, and unreliable
    pos_explanations <- c()
    neg_explanations <- c()
    non_rel_explanations <- c()

    for (explanation in explanations_by_spend) {
      if (grepl("CAUTION:", explanation)) {
        neg_explanations <- c(neg_explanations, explanation)
      } else if (grepl("no statistically reliable evidence", explanation)) {
        non_rel_explanations <- c(non_rel_explanations, explanation)
      } else {
        pos_explanations <- c(pos_explanations, explanation)
      }
    }

    # Combine explanations with appropriate grouping
    result <- c()

    if (length(pos_explanations) > 0) {
      result <- c(result, "**Positive Treatment Effects:**", pos_explanations)
    }

    if (length(neg_explanations) > 0) {
      result <- c(result, "**Caution - Negative Treatment Effects:**", neg_explanations)
    }

    if (length(non_rel_explanations) > 0) {
      result <- c(result, "**Unreliable Effects:**", non_rel_explanations)
    }

    paste(result, collapse = "\n\n")
  })
  names(explanations) <- purrr::map_chr(names(multi_batch), transform_label_wrapper)

  cli::cli_alert_success("Binary treatment model processing completed.")
  return(list(
    summary_table = summary_table,
    explanations  = explanations,
    qini_explanation = qini_explanation
  ))
}

#' @keywords internal
create_qini_explanation_binary <- function() {
  explanation <- paste(
    "The values in the table and explanations represent the difference in gain between two Qini curves at various spend levels, along with 95% confidence intervals.",
    "Given two Qini curves, Q_a and Q_b, we obtain an estimate of the difference Q_a(B) - Q_b(B), at a spend level B.",
    "This difference indicates how much better (or worse) using the conditional average treatment effect (CATE) to prioritise treatments performs compared to using the average treatment effect (ATE) or no prioritisation.",
    "Positive values (in bold) suggest that prioritising treatment based on CATE may lead to higher average responses at that spend level.",
    "Negative values (marked with '(!!)') indicate a caution: targeting treatments using CATE would lead to reliably worse outcomes than using the ATE and is not recommended.",
    sep = "\n"
  )
  return(explanation)
}#' Interpret RATE estimates in markdown
#'
#' This function provides a concise interpretation of Rank-Weighted Average Treatment Effect (RATE)
#' estimates from \code{margot_rate()}. The RATE ranks individuals by predicted treatment effects
#' and computes average effects for selected percentiles.
#'
#' @param rate_df A data frame from \code{margot_rate()} with columns:
#'   \code{RATE Estimate}, \code{2.5\%}, and \code{97.5\%}. First column contains outcome names.
#' @param flipped_outcomes Character vector of outcome names that were inverted during pre-processing.
#' @param target Character string specifying RATE type: \code{AUTOC} (default) or \code{QINI}.
#'
#' @return A markdown string interpreting statistically reliable RATE estimates.
#'
#' @examples
#' \dontrun{
#' interpretation <- margot_interpret_rate(rate_table, flipped_outcomes = c("t2_neuroticism_z"))
#' cat(interpretation)
#' }
#'
#' @importFrom stats qnorm
#' @export
margot_interpret_rate <- function(rate_df, flipped_outcomes = NULL, target = "AUTOC") {
  # validate target parameter
  if (!target %in% c("AUTOC", "QINI")) {
    stop("target must be either 'AUTOC' or 'QINI'.")
  }

  # check for required columns
  required_cols <- c("RATE Estimate", "2.5%", "97.5%")
  if (!all(required_cols %in% colnames(rate_df))) {
    stop("rate_df must contain columns 'RATE Estimate', '2.5%', and '97.5%'.")
  }

  # outcome names are assumed to be in the first column
  outcome_names <- rate_df[[1]]

  # ensure flipped_outcomes is always a character vector
  if (!is.null(flipped_outcomes)) {
    if (!is.vector(flipped_outcomes)) {
      flipped_outcomes <- c(as.character(flipped_outcomes))
    } else {
      flipped_outcomes <- as.character(flipped_outcomes)
    }
  }

  # determine reliability: reliable if the 95% CI doesn't include zero
  is_reliable <- (rate_df[,"2.5%"] > 0) | (rate_df[,"97.5%"] < 0)
  rel_idx <- which(is_reliable)

  # separate positive and negative reliable effects
  pos_rel_idx <- which(rate_df[,"2.5%"] > 0)
  neg_rel_idx <- which(rate_df[,"97.5%"] < 0)

  # create the main heading
  interpretation_parts <- list("### Evidence for Heterogeneous Treatment Effects")

  # brief explanation of RATE based on selected target
  if (target == "AUTOC") {
    target_explanation <- paste0(
      "The Rank-Weighted Average Treatment Effect (RATE) identifies subgroups of individuals ",
      "with different responses to treatment. We used the AUTOC targeting method, which is ",
      "appropriate when heterogeneity is concentrated in a smaller subset of the population."
    )
  } else { # QINI
    target_explanation <- paste0(
      "The Rank-Weighted Average Treatment Effect (RATE) identifies subgroups of individuals ",
      "with different responses to treatment. We used the QINI targeting method, which is ",
      "appropriate when heterogeneity is broadly distributed across the population."
    )
  }
  interpretation_parts[[length(interpretation_parts) + 1]] <- target_explanation

  # check if there are any reliably positive outcomes
  if (length(pos_rel_idx) > 0) {
    # loop over reliably positive outcomes
    for (i in pos_rel_idx) {
      outcome <- outcome_names[i]
      rate_est <- rate_df[i, "RATE Estimate"]
      ci_lower <- rate_df[i, "2.5%"]
      ci_upper <- rate_df[i, "97.5%"]

      # check if outcome was flipped
      is_flipped <- FALSE
      if (!is.null(flipped_outcomes)) {
        # force both sides to be character for comparison and strip any asterisks
        outcome_str <- gsub("\\*", "", as.character(outcome))
        flipped_clean <- gsub("\\*", "", flipped_outcomes)
        is_flipped <- outcome_str %in% flipped_clean
      }

      # for positive effects, we provide detailed interpretations with headings
      header <- paste0("#### ", outcome)
      estimate_text <- sprintf("Estimated RATE: %.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper)

      # determine if effect is positive based on RATE estimate and flipped status
      if (is_flipped) {
        interpretation_text <- paste0(
          "This result indicates reliable treatment effect heterogeneity with positive effects for targeted subgroups (after inverting the outcome). ",
          "This suggests that targeting treatment using CATE may lead to better outcomes for certain individuals."
        )
      } else {
        interpretation_text <- paste0(
          "This result indicates reliable treatment effect heterogeneity with positive effects for targeted subgroups. ",
          "This suggests that targeting treatment using CATE may lead to better outcomes for certain individuals."
        )
      }

      outcome_interpretation <- paste(header, estimate_text, interpretation_text, sep = "\n\n")
      interpretation_parts[[length(interpretation_parts) + 1]] <- outcome_interpretation
    }
  }

  # handle reliably negative outcomes as a group with a caution
  if (length(neg_rel_idx) > 0) {
    neg_outcome_names <- outcome_names[neg_rel_idx]

    # create a subsection for negative outcomes
    neg_header <- "#### Caution for Negative Treatment Effects"

    # list the outcomes with negative effects
    neg_outcomes_list <- paste(neg_outcome_names, collapse = ", ")

    neg_interpretation <- paste0(
      "For the following outcome(s): ", neg_outcomes_list, ", the analysis shows reliably negative RATE estimates ",
      "(95% confidence intervals entirely below zero). This is an important caution that targeting treatment ",
      "based on predicted effects would lead to worse outcomes than using the average treatment effect (ATE). ",
      "For these outcomes, targeting the CATE is expected to lead to reliably worse outcomes and ",
      "is not recommended."
    )

    # Add estimates for each negative outcome
    neg_estimates <- "Specific estimates:"
    for (i in neg_rel_idx) {
      outcome <- outcome_names[i]
      rate_est <- rate_df[i, "RATE Estimate"]
      ci_lower <- rate_df[i, "2.5%"]
      ci_upper <- rate_df[i, "97.5%"]

      is_flipped <- FALSE
      if (!is.null(flipped_outcomes)) {
        outcome_str <- gsub("\\*", "", as.character(outcome))
        flipped_clean <- gsub("\\*", "", flipped_outcomes)
        is_flipped <- outcome_str %in% flipped_clean
      }

      flip_note <- if (is_flipped) " (after inverting the outcome)" else ""
      neg_estimates <- paste0(
        neg_estimates, "\n- ", outcome, ": ",
        sprintf("%.3f (95%% CI: %.3f, %.3f)", rate_est, ci_lower, ci_upper), flip_note
      )
    }

    neg_section <- paste(neg_header, neg_interpretation, neg_estimates, sep = "\n\n")
    interpretation_parts[[length(interpretation_parts) + 1]] <- neg_section
  }

  # check if any outcomes are reliable (either positive or negative)
  if (length(rel_idx) == 0) {
    # no reliable outcomes at all
    no_rel_text <- "No statistically reliable evidence of treatment effect heterogeneity was detected for any outcome (all 95% confidence intervals crossed zero)."
    interpretation_parts[[length(interpretation_parts) + 1]] <- no_rel_text
  } else if (sum(is_reliable) < length(outcome_names)) {
    # some outcomes are not reliable
    non_rel_idx <- which(!is_reliable)
    non_rel_outcomes <- outcome_names[non_rel_idx]

    if (length(non_rel_outcomes) > 0) {
      non_rel_text <- paste0(
        "For the remaining outcome(s): ", paste(non_rel_outcomes, collapse = ", "),
        ", no statistically reliable evidence of treatment effect heterogeneity was detected ",
        "(95% confidence intervals crossed zero)."
      )
      interpretation_parts[[length(interpretation_parts) + 1]] <- non_rel_text
    }
  }

  interpretation_text <- paste(interpretation_parts, collapse = "\n\n")
  return(interpretation_text)
}

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

  # Wrapper to transform model labels using the package helper.
  transform_label_wrapper <- function(label) {
    original_label <- label
    if (!is.null(label_mapping)) {
      # Remove "model_" prefix before checking mapping.
      clean_label <- sub("^model_", "", label)
      if (clean_label %in% names(label_mapping)) {
        mapped <- label_mapping[[clean_label]]
        cli::cli_alert_info("Mapped label: {original_label} -> {mapped}")
        return(mapped)
      }
      if (label %in% names(label_mapping)) {
        mapped <- label_mapping[[label]]
        cli::cli_alert_info("Mapped label: {original_label} -> {mapped}")
        return(mapped)
      }
    }
    # Remove "model_" prefix and then apply the helper transformation.
    label <- sub("^model_", "", label)
    transformed <- transform_label(label, label_mapping = NULL, options = list(
      remove_tx_prefix   = TRUE,
      remove_z_suffix    = TRUE,
      remove_underscores = TRUE,
      use_title_case     = TRUE
    ))
    return(transformed)
  }

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

  create_explanation <- function(diff_gain_summary, model_name, spend) {
    estimates <- extract_estimates(diff_gain_summary)
    if (any(is.na(estimates))) {
      return(paste("Unable to create explanation for", model_name, "at", spend * 100, "% spend level due to missing estimates."))
    }

    # Determine direction and reliability.
    direction <- if (estimates["estimate"] > 0) "better" else if (estimates["estimate"] < 0) "worse" else "indistinguishable"
    reliably <- estimates["ci_lower"] * estimates["ci_upper"] > 0

    if (!reliably) {
      return(paste("At the", spend * 100, "% spend level for outcome", transform_label_wrapper(model_name),
                   "there is no reliable evidence to support using CATE to prioritise treatments."))
    }

    # Revised explanation for reliably negative results
    if (direction == "worse") {
      return(paste("At the", spend * 100, "% spend level for outcome", transform_label_wrapper(model_name),
                   "the evidence indicates a CAUTION: Using CATE to prioritise treatments would lead to reliably worse average responses (",
                   format(round(estimates['estimate'], decimal_places), nsmall = decimal_places),
                   " [95% CI: ",
                   format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places),
                   ", ",
                   format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places),
                   "]) compared to using the ATE. For this outcome, targeting the CATE is not recommended."))
    }

    # If evidence is reliably positive, output the enhanced explanation
    explanation <- glue::glue(
      "For the outcome {transform_label_wrapper(model_name)}, at the {spend * 100}% spend level, using the conditional average treatment effect (CATE) to prioritise treatments is reliably better than using the average treatment effect (ATE) to assign treatment. The difference when prioritising CATE is {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} [95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}]."
    )
    explanation <- paste(explanation, "This result suggests that prioritising CATE may increase the average treatment response relative to no prioritisation at this spend level.")
    return(explanation)
  }

  format_estimate_ci <- function(estimate, ci_lower, ci_upper) {
    if (any(is.na(c(estimate, ci_lower, ci_upper)))) return("NA [NA, NA]")

    formatted <- sprintf(paste0("%.", decimal_places, "f [%.", decimal_places, "f, %.", decimal_places, "f]"),
                         estimate, ci_lower, ci_upper)

    # Check if we're using kableExtra or producing LaTeX directly
    if (requireNamespace("kableExtra", quietly = TRUE)) {
      # For kableExtra users, use cell_spec for better formatting
      # Mark positive reliable estimates with bold
      if (ci_lower > 0) {
        # This should work with both LaTeX and HTML output
        return(paste0("**", formatted, "**"))
      }
      # Mark negative reliable estimates with warning
      else if (ci_upper < 0) {
        return(paste0("(!!) *", formatted, "*"))
      }
    } else {
      # Basic approach using Markdown syntax that kable will handle
      if (ci_lower > 0) {
        return(paste0("**", formatted, "**"))
      }
      else if (ci_upper < 0) {
        return(paste0("(!!) *", formatted, "*"))
      }
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
        Model     = transform_label_wrapper(model_name),
        Spend     = as.numeric(gsub("spend_", "", spend)) * 100,
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
    explanations_by_spend <- purrr::map_chr(spend_levels, function(spend) {
      create_explanation(
        model_results[[spend]],
        model_name,
        as.numeric(gsub("spend_", "", spend))
      )
    })

    # Group explanations into positive, negative, and non-significant
    pos_explanations <- c()
    neg_explanations <- c()
    non_sig_explanations <- c()

    for (explanation in explanations_by_spend) {
      if (grepl("CAUTION:", explanation)) {
        neg_explanations <- c(neg_explanations, explanation)
      } else if (grepl("no reliable evidence", explanation)) {
        non_sig_explanations <- c(non_sig_explanations, explanation)
      } else {
        pos_explanations <- c(pos_explanations, explanation)
      }
    }

    # Combine explanations with appropriate grouping
    result <- c()

    if (length(pos_explanations) > 0) {
      result <- c(result, "**Positive Treatment Effects:**", pos_explanations)
    }

    if (length(neg_explanations) > 0) {
      result <- c(result, "**Caution - Negative Treatment Effects:**", neg_explanations)
    }

    if (length(non_sig_explanations) > 0) {
      result <- c(result, "**Non-Significant Effects:**", non_sig_explanations)
    }

    paste(result, collapse = "\n\n")
  })
  names(explanations) <- purrr::map_chr(names(multi_batch), transform_label_wrapper)

  cli::cli_alert_success("Binary treatment model processing completed.")
  return(list(
    summary_table = summary_table,
    explanations  = explanations,
    qini_explanation = qini_explanation
  ))
}

#' @keywords internal
create_qini_explanation_binary <- function() {
  explanation <- paste(
    "The values in the table and explanations represent the difference in gain between two Qini curves at various spend levels, along with 95% confidence intervals.",
    "Given two Qini curves, Q_a and Q_b, we obtain an estimate of the difference Q_a(B) - Q_b(B), at a spend level B.",
    "This difference indicates how much better (or worse) using the conditional average treatment effect (CATE) to prioritise treatments performs compared to using the average treatment effect (ATE) or no prioritisation.",
    "Positive values (in bold) suggest that prioritising treatment based on CATE may lead to higher average responses at that spend level.",
    "Negative values (marked with '(!!)') indicate a caution: targeting treatments using CATE would lead to reliably worse outcomes than using the ATE and is not recommended.",
    sep = "\n"
  )
  return(explanation)
}
