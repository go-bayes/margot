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
#'         \code{qini_explanation}.
#'
margot_interpret_qini <- function(multi_batch,
                                  label_mapping = NULL,
                                  alpha = 0.05,
                                  decimal_places = 2,
                                  model_names = NULL) {
  cli::cli_alert_info("starting qini interpretation...")

  # detect whether we're dealing with a single model or a batch
  if ("diff_gain_summaries" %in% names(multi_batch)) {
    # it's a single model result, not a batch
    cli::cli_alert_info("detected single model result (not a batch)")
    single_model <- TRUE

    # get spend level names
    spend_level_names <- names(multi_batch$diff_gain_summaries)

    # convert to batch-like structure for consistent processing
    model_name <- "model_result"
    temp_batch <- list()
    temp_batch[[model_name]] <- multi_batch
    multi_batch <- temp_batch
  } else {
    # it's a batch of models
    single_model <- FALSE

    # check if multi_batch is empty or not a list
    if (!is.list(multi_batch) || length(multi_batch) == 0) {
      cli::cli_alert_danger("multi_batch must be a non-empty list")
      return(list(
        summary_table = NULL,
        qini_explanation = "error: multi_batch must be a non-empty list"
      ))
    }

    # restrict processing to selected models if model_names is provided
    if (!is.null(model_names) && length(model_names) > 0) {
      # check if model_names exist in multi_batch
      valid_models <- intersect(names(multi_batch), model_names)
      if (length(valid_models) == 0) {
        cli::cli_alert_danger("none of the specified model_names exist in multi_batch")
        return(list(
          summary_table = NULL,
          qini_explanation = "error: none of the specified model_names exist in multi_batch"
        ))
      }

      # filter multi_batch to only include specified models
      multi_batch <- multi_batch[valid_models]
      cli::cli_alert_info(glue::glue("processing {length(valid_models)} of {length(model_names)} requested models"))
    }

    # get spend level names from first model
    first_model <- names(multi_batch)[1]
    spend_level_names <- names(multi_batch[[first_model]]$diff_gain_summaries)
  }

  if (length(spend_level_names) == 0) {
    cli::cli_alert_danger("no spend levels found in diff_gain_summaries")
    return(list(
      summary_table = NULL,
      qini_explanation = "error: no spend levels found in diff_gain_summaries"
    ))
  }

  cli::cli_alert_info(glue::glue("detected spend levels: {paste(spend_level_names, collapse=', ')}"))

  # determine if the model is binary or multi-arm by checking the diff_gain_summaries structure
  is_binary <- TRUE  # default assumption

  # safely check the structure
  first_model <- names(multi_batch)[1]
  first_spend_level <- spend_level_names[1]

  if (!is.null(multi_batch[[first_model]]$diff_gain_summaries[[first_spend_level]])) {
    # check if this is a multi-arm treatment
    if (is.list(multi_batch[[first_model]]$diff_gain_summaries[[first_spend_level]]) &&
        "all_arms" %in% names(multi_batch[[first_model]]$diff_gain_summaries[[first_spend_level]])) {
      is_binary <- FALSE
      cli::cli_alert_success("detected multi-arm treatment model")
    } else {
      cli::cli_alert_success("detected binary treatment model")
    }
  } else {
    cli::cli_alert_warning("could not determine treatment type, assuming binary")
  }

  if (is_binary) {
    result <- margot_interpret_qini_binary(multi_batch, label_mapping, alpha, decimal_places)
  } else {
    cli::cli_alert_success("detected multi-arm treatment model")
    if (exists("margot_interpret_qini_multi_arm", mode = "function")) {
      result <- margot_interpret_qini_multi_arm(multi_batch, label_mapping, alpha, decimal_places)
    } else {
      cli::cli_alert_warning("multi-arm treatment interpretation not implemented")
      # placeholder for multi-arm implementation
      result <- list(
        summary_table = NULL,
        explanations = list(),
        qini_explanation = "multi-arm treatment interpretation not implemented yet."
      )
    }
  }

  # create the combined explanation without the header
  combined_explanation <- paste(
    result$qini_explanation, "\n",
    paste(sapply(names(result$explanations), function(model) {
      paste0("**", model, "**\n", result$explanations[[model]])
    }), collapse = "\n\n")
  )

  cli::cli_alert_info("qini interpretation completed")

  # return the summary table and combined explanation
  return(list(
    summary_table = result$summary_table,
    qini_explanation = combined_explanation
  ))
}

#' @keywords internal
margot_interpret_qini_binary <- function(multi_batch, label_mapping = NULL, alpha = 0.05, decimal_places = 2) {
  cli::cli_alert_info("processing binary treatment model...")

  # use the new general explanation for binary qini curves
  qini_explanation <- create_qini_explanation_binary()

  # use transform_var_name helper for consistent variable naming
  transform_model_label <- function(label) {
    # first check if we have direct access to transform_var_name function
    if (exists("transform_var_name", mode = "function")) {
      return(transform_var_name(
        label,
        label_mapping = label_mapping,
        remove_tx_prefix = TRUE,
        remove_z_suffix = TRUE,
        use_title_case = TRUE,
        remove_underscores = TRUE
      ))
    } else {
      # fallback if transform_var_name function is not available
      original_label <- label

      # apply label mapping if available
      if (!is.null(label_mapping)) {
        # try direct mapping
        if (label %in% names(label_mapping)) {
          mapped <- label_mapping[[label]]
          cli::cli_alert_info(glue::glue("mapped label: {original_label} -> {mapped}"))
          return(mapped)
        }

        # remove "model_" prefix before checking mapping
        clean_label <- sub("^model_", "", label)
        if (clean_label %in% names(label_mapping)) {
          mapped <- label_mapping[[clean_label]]
          cli::cli_alert_info(glue::glue("mapped label: {original_label} -> {mapped}"))
          return(mapped)
        }
      }

      # apply transformations if no mapping found
      label <- sub("^model_", "", label)
      label <- sub("^t[0-9]+_", "", label) # remove tx_ prefix
      label <- sub("_z$", "", label)       # remove _z suffix
      label <- gsub("_", " ", label)       # replace underscores with spaces

      # apply title case
      label <- tools::toTitleCase(label)

      # Special case for NZ
      label <- gsub("Nz", "NZ", label)

      return(label)
    }
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
        cli::cli_alert_warning("unable to extract estimate or SE from diff_gain")
        return(c(estimate = NA, ci_lower = NA, ci_upper = NA))
      }
      ci_lower <- estimate - stats::qnorm(1 - alpha/2) * se
      ci_upper <- estimate + stats::qnorm(1 - alpha/2) * se
      return(c(estimate = estimate, ci_lower = ci_lower, ci_upper = ci_upper))
    }, error = function(e) {
      cli::cli_alert_warning(paste("error in extract_estimates:", e$message))
      return(c(estimate = NA, ci_lower = NA, ci_upper = NA))
    })
  }

  create_explanation <- function(diff_gain_summary, model_name, spend) {
    estimates <- extract_estimates(diff_gain_summary)
    if (any(is.na(estimates))) {
      return(paste("Unable to evaluate", spend * 100, "% spend level due to missing estimates."))
    }

    # determine direction and reliability
    direction <- if (estimates["estimate"] > 0) "better" else if (estimates["estimate"] < 0) "worse" else "indistinguishable"
    reliably <- estimates["ci_lower"] * estimates["ci_upper"] > 0

    if (!reliably) {
      return(paste("At", spend * 100, "% spend: No reliable benefits from CATE prioritisation."))
    }

    if (direction != "better") {
      return(paste("At", spend * 100, "% spend: CATE prioritisation worsens outcomes compared to ATE."))
    }

    # if evidence is reliably positive, output the concise explanation
    explanation <- glue::glue(
      "At {spend * 100}% spend: CATE prioritisation is beneficial (diff: {format(round(estimates['estimate'], decimal_places), nsmall = decimal_places)} [95% CI: {format(round(estimates['ci_lower'], decimal_places), nsmall = decimal_places)}, {format(round(estimates['ci_upper'], decimal_places), nsmall = decimal_places)}])."
    )
    return(explanation)
  }

  # extract spend values from spend names (for more flexible handling)
  extract_spend_value <- function(spend_name) {
    # try to extract numeric value from spend_X.Y format
    if (grepl("^spend_", spend_name)) {
      value <- as.numeric(sub("^spend_", "", spend_name))
      return(value * 100)  # convert to percentage
    }

    # fallback: try direct numeric conversion
    tryCatch({
      value <- as.numeric(spend_name)
      return(value * 100)  # convert to percentage
    }, error = function(e) {
      # if all else fails, return the original name
      return(spend_name)
    })
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

  cli::cli_alert_info("processing individual models...")

  # process each model to extract estimates
  summary_data <- purrr::map_dfr(names(multi_batch), function(model_name) {
    cli::cli_alert_info(paste("processing model:", model_name))

    # check if diff_gain_summaries exists
    if (!("diff_gain_summaries" %in% names(multi_batch[[model_name]]))) {
      cli::cli_alert_warning(paste("diff_gain_summaries not found for model:", model_name))
      return(data.frame(Model = character(0), Spend = numeric(0), estimate_ci = character(0)))
    }

    model_results <- multi_batch[[model_name]]$diff_gain_summaries

    # process each spend level
    spend_data <- purrr::map_dfr(names(model_results), function(spend) {
      # check if spend results exist
      if (is.null(model_results[[spend]])) {
        cli::cli_alert_warning(paste("NULL result for spend level:", spend, "in model:", model_name))
        return(NULL)
      }

      # extract estimates
      estimates <- extract_estimates(model_results[[spend]])

      # create data frame row
      data.frame(
        Model = transform_model_label(model_name),
        Spend = extract_spend_value(spend),
        estimate_ci = format_estimate_ci(estimates["estimate"], estimates["ci_lower"], estimates["ci_upper"])
      )
    })

    # handle empty result
    if (is.null(spend_data) || nrow(spend_data) == 0) {
      return(data.frame(Model = character(0), Spend = numeric(0), estimate_ci = character(0)))
    }

    return(spend_data)
  })

  # create summary table
  cli::cli_alert_info("creating summary table...")

  if (nrow(summary_data) == 0) {
    cli::cli_alert_warning("no valid data found for summary table")
    summary_table <- NULL
  } else {
    # check if we have Spend column (for safety)
    if (!"Spend" %in% colnames(summary_data)) {
      cli::cli_alert_warning("spend column not found in summary data")
      summary_table <- summary_data
    } else {
      # reshape data into wide format
      tryCatch({
        summary_table <- summary_data %>%
          tidyr::pivot_wider(
            names_from = Spend,
            values_from = estimate_ci,
            names_prefix = "Spend "
          ) %>%
          dplyr::rename_with(~paste0(., "%"), -Model)
      }, error = function(e) {
        cli::cli_alert_warning(paste("error creating wide format table:", e$message))
        # fallback to original format
        summary_table <- summary_data
      })
    }
  }

  # generate explanations for each model
  cli::cli_alert_info("generating explanations...")

  explanations <- list()
  for (model_name in names(multi_batch)) {
    cli::cli_alert_info(paste("generating explanation for model:", model_name))

    if (!("diff_gain_summaries" %in% names(multi_batch[[model_name]]))) {
      explanations[[transform_model_label(model_name)]] <- "No data available for this model."
      next
    }

    model_results <- multi_batch[[model_name]]$diff_gain_summaries
    spend_levels <- names(model_results)

    # Check for spend levels that match 0.2 (20%) and 0.5 (50%)
    spend_values <- as.numeric(gsub("spend_", "", spend_levels))
    spend_20 <- spend_levels[which(abs(spend_values - 0.2) < 0.01)]
    spend_50 <- spend_levels[which(abs(spend_values - 0.5) < 0.01)]

    # Get explanations for both spend levels
    expl_20 <- if(length(spend_20) > 0 && !is.null(model_results[[spend_20]])) {
      create_explanation(model_results[[spend_20]], model_name, 0.2)
    } else {
      "At 20% spend: No data available."
    }

    expl_50 <- if(length(spend_50) > 0 && !is.null(model_results[[spend_50]])) {
      create_explanation(model_results[[spend_50]], model_name, 0.5)
    } else {
      "At 50% spend: No data available."
    }

    # Check if both levels show no benefits
    if (grepl("No reliable benefits", expl_20) && grepl("No reliable benefits", expl_50)) {
      explanations[[transform_model_label(model_name)]] <-
        "No benefits for priority investments as measured by the QINI curve at the twenty and fifty percent spend levels."
    } else {
      explanations[[transform_model_label(model_name)]] <- paste(expl_20, expl_50, sep = " ")
    }
  }

  cli::cli_alert_success("binary treatment model processing completed")

  return(list(
    summary_table = summary_table,
    explanations = explanations,
    qini_explanation = qini_explanation
  ))
}

#' @keywords internal
create_qini_explanation_binary <- function() {
  # new general comment instead of explanation header
  explanation <- "We computed the cumulative benefits as we increase the treated fraction by prioritising conditional average treatment effects (CATE) at two different spend levels: 20% of a total budget and 50% of a total budget, where the contrast is no priority assignment."
  return(explanation)
}
